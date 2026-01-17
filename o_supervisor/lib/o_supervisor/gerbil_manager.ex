defmodule OSupervisor.GerbilManager do
  @moduledoc """
  Manages a Gerbil process instance via Erlang Port.
  
  Responsibilities:
  - Start and monitor Gerbil process
  - Handle bidirectional MessagePack communication
  - Monitor heartbeat and restart on failure
  - Buffer WAL entries
  - Coordinate checkpoints
  """
  
  use GenServer
  require Logger
  
  defstruct [
    :port,
    :role,
    :name,
    :checkpoint_id,
    :wal_buffer,
    :metrics,
    :last_heartbeat,
    :start_time
  ]

  # Client API

  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  def send_message(pid, message) do
    GenServer.call(pid, {:send_message, message})
  end

  def get_state(pid) do
    GenServer.call(pid, :get_state)
  end

  def hot_reload(pid, new_code) do
    GenServer.call(pid, {:hot_reload, new_code}, 30_000)
  end

  # Server Callbacks

  @impl true
  def init(opts) do
    role = Keyword.fetch!(opts, :role)
    name = Keyword.get(opts, :name, role)
    
    Logger.info("Starting GerbilManager for role: #{role}")
    
    # Start Gerbil process
    port = start_gerbil_process(role, opts)
    
    # Schedule heartbeat check
    schedule_heartbeat_check()
    
    state = %__MODULE__{
      port: port,
      role: role,
      name: name,
      checkpoint_id: nil,
      wal_buffer: :queue.new(),
      metrics: %{},
      last_heartbeat: System.monotonic_time(:millisecond),
      start_time: System.system_time(:second)
    }
    
    {:ok, state}
  end

  @impl true
  def handle_call({:send_message, message}, _from, state) do
    case send_to_gerbil(state.port, message) do
      :ok -> {:reply, :ok, state}
      error -> {:reply, error, state}
    end
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, Map.from_struct(state), state}
  end

  @impl true
  def handle_call({:hot_reload, new_code}, _from, state) do
    message = %{
      type: "hot_reload",
      code: new_code,
      timestamp: System.system_time(:second)
    }
    
    case send_to_gerbil(state.port, message) do
      :ok -> {:reply, :ok, state}
      error -> {:reply, error, state}
    end
  end

  @impl true
  def handle_info({port, {:data, data}}, %{port: port} = state) do
    case Msgpax.unpack(data) do
      {:ok, message} ->
        handle_gerbil_message(message, state)
      
      {:error, reason} ->
        Logger.error("Failed to unpack message: #{inspect(reason)}")
        {:noreply, state}
    end
  end

  @impl true
  def handle_info({port, {:exit_status, status}}, %{port: port} = state) do
    Logger.error("Gerbil process exited with status: #{status}")
    {:stop, {:gerbil_exit, status}, state}
  end

  @impl true
  def handle_info(:heartbeat_check, state) do
    now = System.monotonic_time(:millisecond)
    timeout = Application.get_env(:o_supervisor, :heartbeat_timeout, 5000)
    
    if now - state.last_heartbeat > timeout do
      Logger.error("Gerbil process #{state.role} heartbeat timeout, restarting...")
      {:stop, :heartbeat_timeout, state}
    else
      schedule_heartbeat_check()
      {:noreply, state}
    end
  end

  # Private Functions

  defp start_gerbil_process(role, opts) do
    config = Application.get_all_env(:o_supervisor)
    executable = config[:gerbil_executable]
    main_script = config[:gerbil_main_script]
    
    checkpoint_id = Keyword.get(opts, :checkpoint_id)
    
    args = [
      main_script,
      "--role", to_string(role),
      "--elixir-mode"
    ]
    
    args = if checkpoint_id do
      args ++ ["--restore", checkpoint_id]
    else
      args
    end
    
    Port.open(
      {:spawn_executable, executable},
      [
        {:args, args},
        {:packet, 4},  # 4-byte length prefix
        :binary,
        :exit_status,
        {:env, [{~c"GERBIL_HOME", String.to_charlist(System.get_env("GERBIL_HOME") || "/usr/local/gerbil")}]}
      ]
    )
  end

  defp send_to_gerbil(port, message) do
    try do
      packed = Msgpax.pack!(message)
      Port.command(port, packed)
      :ok
    rescue
      e ->
        Logger.error("Failed to send message to Gerbil: #{inspect(e)}")
        {:error, :send_failed}
    end
  end

  defp handle_gerbil_message(%{"type" => "heartbeat"}, state) do
    {:noreply, %{state | last_heartbeat: System.monotonic_time(:millisecond)}}
  end

  defp handle_gerbil_message(%{"type" => "checkpoint", "data" => snapshot}, state) do
    checkpoint_id = UUID.uuid4()
    
    Logger.info("Received checkpoint request, id: #{checkpoint_id}")
    
    # Save to MemoryVault
    :ok = OSupervisor.MemoryVault.save_checkpoint(
      checkpoint_id,
      snapshot,
      :queue.to_list(state.wal_buffer)
    )
    
    # Send acknowledgment
    ack = %{
      type: "checkpoint_ack",
      checkpoint_id: checkpoint_id,
      timestamp: System.system_time(:second)
    }
    send_to_gerbil(state.port, ack)
    
    {:noreply, %{state | checkpoint_id: checkpoint_id, wal_buffer: :queue.new()}}
  end

  defp handle_gerbil_message(%{"type" => "wal_entry", "data" => entry}, state) do
    # Add to buffer
    new_buffer = :queue.in(entry, state.wal_buffer)
    
    # Also send to WAL manager for persistence
    OSupervisor.WALManager.append_entry(entry)
    
    # Flush if buffer is large
    new_buffer = if :queue.len(new_buffer) > 100 do
      flush_wal_buffer(state.port, new_buffer)
      :queue.new()
    else
      new_buffer
    end
    
    {:noreply, %{state | wal_buffer: new_buffer}}
  end

  defp handle_gerbil_message(%{"type" => "wal_batch", "data" => entries}, state) do
    Enum.each(entries, &OSupervisor.WALManager.append_entry/1)
    {:noreply, state}
  end

  defp handle_gerbil_message(%{"type" => "evolution_intent", "hypothesis" => hypothesis}, state) do
    Logger.info("Received evolution intent: #{inspect(hypothesis)}")
    
    # Notify evolution arbiter
    OSupervisor.EvolutionArbiter.start_shadow_test(
      hypothesis,
      state.checkpoint_id || "none"
    )
    
    {:noreply, state}
  end

  defp handle_gerbil_message(%{"type" => "metrics", "data" => metrics}, state) do
    # Update health monitor
    OSupervisor.HealthMonitor.record_metrics(state.role, metrics)
    {:noreply, %{state | metrics: metrics}}
  end

  defp handle_gerbil_message(message, state) do
    Logger.warning("Unknown message type: #{inspect(message)}")
    {:noreply, state}
  end

  defp flush_wal_buffer(port, buffer) do
    entries = :queue.to_list(buffer)
    message = %{type: "wal_flush_ack", count: length(entries)}
    send_to_gerbil(port, message)
  end

  defp schedule_heartbeat_check do
    interval = Application.get_env(:o_supervisor, :heartbeat_interval, 1000)
    Process.send_after(self(), :heartbeat_check, interval)
  end
end
