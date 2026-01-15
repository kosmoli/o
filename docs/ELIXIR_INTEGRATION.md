# Elixir Integration Guide

This guide provides detailed instructions for integrating Elixir/OTP supervision layer with the Gerbil-based self-evolving agent system.

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Project Setup](#project-setup)
3. [Core Components](#core-components)
4. [Communication Protocol](#communication-protocol)
5. [Checkpoint & Recovery](#checkpoint--recovery)
6. [Shadow Testing](#shadow-testing)
7. [Multi-Threaded Evolution](#multi-threaded-evolution)
8. [Deployment](#deployment)
9. [Troubleshooting](#troubleshooting)

---

## Prerequisites

### Required Software

```bash
# Elixir & Erlang
brew install elixir  # macOS
# or
apt-get install elixir  # Ubuntu

# Verify installation
elixir --version  # Should be 1.14+
erl -version      # Should be OTP 25+

# Gerbil Scheme
# Follow: https://cons.io/

# Zig (for infrastructure layer)
brew install zig  # macOS

# Rust (for compute layer)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### System Requirements

- **OS**: Linux or macOS (Windows via WSL2)
- **RAM**: Minimum 4GB, recommended 8GB+
- **CPU**: Multi-core recommended for parallel evolution
- **Disk**: 10GB+ for checkpoints and WAL logs

---

## Project Setup

### Step 1: Create Elixir Supervisor Project

```bash
cd /Users/liyuhang/work/o

# Create new Elixir application with supervision tree
mix new o_supervisor --sup

cd o_supervisor

# Add dependencies to mix.exs
```

**Edit `mix.exs`:**

```elixir
defmodule OSupervisor.MixProject do
  use Mix.Project

  def project do
    [
      app: :o_supervisor,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :crypto],
      mod: {OSupervisor.Application, []}
    ]
  end

  defp deps do
    [
      {:msgpax, "~> 2.3"},           # MessagePack serialization
      {:jason, "~> 1.4"},            # JSON (for config)
      {:telemetry, "~> 1.2"},        # Metrics
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:uuid, "~> 1.1"},             # UUID generation
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false},
      {:ex_doc, "~> 0.29", only: :dev, runtime: false}
    ]
  end
end
```

```bash
# Install dependencies
mix deps.get
```

### Step 2: Configure Application

**Edit `config/config.exs`:**

```elixir
import Config

config :o_supervisor,
  gerbil_executable: System.get_env("GERBIL_EXECUTABLE") || "gerbil",
  gerbil_main_script: "../gerbil/main.ss",
  checkpoint_dir: "data/checkpoints",
  wal_dir: "data/wal",
  shared_memory_name: "/o_shared_state",
  shared_memory_size: 1024 * 1024,  # 1MB
  heartbeat_interval: 1000,          # 1 second
  heartbeat_timeout: 5000,           # 5 seconds
  max_concurrent_shadows: 10,
  shadow_test_duration: 300_000,     # 5 minutes
  evolution_enabled: true

config :logger,
  level: :info,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, :module, :function]

import_config "#{config_env()}.exs"
```

**Create `config/dev.exs`:**

```elixir
import Config

config :o_supervisor,
  evolution_enabled: true,
  max_concurrent_shadows: 5

config :logger, level: :debug
```

**Create `config/prod.exs`:**

```elixir
import Config

config :o_supervisor,
  evolution_enabled: true,
  max_concurrent_shadows: 20

config :logger, level: :info
```

**Create `config/test.exs`:**

```elixir
import Config

config :o_supervisor,
  evolution_enabled: false,
  max_concurrent_shadows: 2

config :logger, level: :warn
```

---

## Core Components

### Component 1: Application Supervisor

**Create `lib/o_supervisor/application.ex`:**

```elixir
defmodule OSupervisor.Application do
  @moduledoc """
  The OSupervisor Application.
  
  This is the root supervisor that manages all components of the
  Elixir supervision layer for the O self-evolving agent system.
  """
  
  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    Logger.info("Starting O Supervisor Application...")
    
    # Ensure data directories exist
    ensure_data_directories!()
    
    children = [
      # Telemetry supervisor for metrics
      OSupervisor.Telemetry,
      
      # Memory vault for state persistence
      {OSupervisor.MemoryVault, []},
      
      # WAL manager for write-ahead logging
      {OSupervisor.WALManager, []},
      
      # Health monitor for heartbeat and metrics
      {OSupervisor.HealthMonitor, []},
      
      # Evolution arbiter for shadow testing
      {OSupervisor.EvolutionArbiter, []},
      
      # Dynamic supervisor for shadow instances
      {DynamicSupervisor, name: OSupervisor.ShadowSupervisor, strategy: :one_for_one},
      
      # Main Gerbil process manager
      {OSupervisor.GerbilManager, [role: :main, name: :main_gerbil]},
      
      # Traffic splitter for shadow testing
      {OSupervisor.TrafficSplitter, []}
    ]

    opts = [strategy: :one_for_one, name: OSupervisor.Supervisor]
    Supervisor.start_link(children, opts)
  end
  
  defp ensure_data_directories! do
    config = Application.get_all_env(:o_supervisor)
    
    [config[:checkpoint_dir], config[:wal_dir]]
    |> Enum.each(fn dir ->
      File.mkdir_p!(dir)
      Logger.info("Ensured directory exists: #{dir}")
    end)
  end
end
```

### Component 2: Gerbil Manager

**Create `lib/o_supervisor/gerbil_manager.ex`:**

```elixir
defmodule OSupervisor.GerbilManager do
  @moduledoc """
  Manages a Gerbil process instance.
  
  Responsibilities:
  - Start and monitor Gerbil process via Erlang Port
  - Handle bidirectional communication (MessagePack)
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
        {:env, [{'GERBIL_HOME', System.get_env("GERBIL_HOME") || "/usr/local/gerbil"}]}
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
    Logger.warn("Unknown message type: #{inspect(message)}")
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
```

### Component 3: Memory Vault

**Create `lib/o_supervisor/memory_vault.ex`:**

```elixir
defmodule OSupervisor.MemoryVault do
  @moduledoc """
  Persistent storage for agent state checkpoints.
  
  Uses DETS for fast in-memory access with disk persistence.
  Also maintains file-based backups for redundancy.
  """
  
  use GenServer
  require Logger

  defstruct [:dets, :checkpoint_dir]

  # Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def save_checkpoint(checkpoint_id, snapshot, wal_entries) do
    GenServer.call(__MODULE__, {:save, checkpoint_id, snapshot, wal_entries}, 30_000)
  end

  def restore_checkpoint(checkpoint_id) do
    GenServer.call(__MODULE__, {:restore, checkpoint_id})
  end

  def list_checkpoints do
    GenServer.call(__MODULE__, :list)
  end

  def delete_checkpoint(checkpoint_id) do
    GenServer.call(__MODULE__, {:delete, checkpoint_id})
  end

  # Server Callbacks

  @impl true
  def init(_) do
    checkpoint_dir = Application.get_env(:o_supervisor, :checkpoint_dir)
    dets_file = Path.join(checkpoint_dir, "checkpoints.dets")
    
    {:ok, dets} = :dets.open_file(:memory_vault, [
      type: :set,
      file: String.to_charlist(dets_file)
    ])
    
    Logger.info("MemoryVault initialized with DETS: #{dets_file}")
    
    {:ok, %__MODULE__{dets: dets, checkpoint_dir: checkpoint_dir}}
  end

  @impl true
  def handle_call({:save, id, snapshot, wal}, _from, state) do
    Logger.info("Saving checkpoint: #{id}")
    
    # Prepare checkpoint data
    checkpoint_data = %{
      id: id,
      snapshot: snapshot,
      wal_entries: wal,
      timestamp: System.system_time(:second),
      size: byte_size(:erlang.term_to_binary(snapshot))
    }
    
    # Compress and serialize
    compressed = :erlang.term_to_binary(checkpoint_data, [:compressed, {:compressed, 9}])
    
    # Save to DETS
    :dets.insert(state.dets, {id, compressed})
    :dets.sync(state.dets)
    
    # Also save to file for redundancy
    file_path = Path.join(state.checkpoint_dir, "#{id}.ckpt")
    File.write!(file_path, compressed)
    
    Logger.info("Checkpoint saved: #{id} (#{byte_size(compressed)} bytes)")
    
    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:restore, id}, _from, state) do
    Logger.info("Restoring checkpoint: #{id}")
    
    case :dets.lookup(state.dets, id) do
      [{^id, compressed}] ->
        checkpoint_data = :erlang.binary_to_term(compressed)
        
        result = %{
          snapshot: checkpoint_data.snapshot,
          wal_entries: checkpoint_data.wal_entries,
          timestamp: checkpoint_data.timestamp
        }
        
        Logger.info("Checkpoint restored: #{id}")
        {:reply, {:ok, result}, state}
      
      [] ->
        # Try file backup
        file_path = Path.join(state.checkpoint_dir, "#{id}.ckpt")
        
        if File.exists?(file_path) do
          compressed = File.read!(file_path)
          checkpoint_data = :erlang.binary_to_term(compressed)
          
          # Restore to DETS
          :dets.insert(state.dets, {id, compressed})
          
          result = %{
            snapshot: checkpoint_data.snapshot,
            wal_entries: checkpoint_data.wal_entries,
            timestamp: checkpoint_data.timestamp
          }
          
          Logger.info("Checkpoint restored from file: #{id}")
          {:reply, {:ok, result}, state}
        else
          Logger.error("Checkpoint not found: #{id}")
          {:reply, {:error, :not_found}, state}
        end
    end
  end

  @impl true
  def handle_call(:list, _from, state) do
    checkpoints = 
      :dets.foldl(
        fn {id, compressed}, acc ->
          checkpoint_data = :erlang.binary_to_term(compressed)
          
          [%{
            id: id,
            timestamp: checkpoint_data.timestamp,
            size: byte_size(compressed)
          } | acc]
        end,
        [],
        state.dets
      )
      |> Enum.sort_by(& &1.timestamp, :desc)
    
    {:reply, checkpoints, state}
  end

  @impl true
  def handle_call({:delete, id}, _from, state) do
    :dets.delete(state.dets, id)
    
    file_path = Path.join(state.checkpoint_dir, "#{id}.ckpt")
    if File.exists?(file_path), do: File.rm!(file_path)
    
    Logger.info("Checkpoint deleted: #{id}")
    {:reply, :ok, state}
  end

  @impl true
  def terminate(_reason, state) do
    :dets.close(state.dets)
    :ok
  end
end
```

### Component 4: WAL Manager

**Create `lib/o_supervisor/wal_manager.ex`:**

```elixir
defmodule OSupervisor.WALManager do
  @moduledoc """
  Write-Ahead Log manager for operation logging.
  
  Ensures durability by logging operations before they're executed.
  Supports crash recovery via WAL replay.
  """
  
  use GenServer
  require Logger

  @segment_size 10_000

  defstruct [
    :wal_dir,
    :current_segment,
    :file,
    :entry_count,
    :buffer
  ]

  # Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def append_entry(entry) do
    GenServer.cast(__MODULE__, {:append, entry})
  end

  def append_batch(entries) do
    GenServer.cast(__MODULE__, {:append_batch, entries})
  end

  def replay_from_checkpoint(checkpoint_id) do
    GenServer.call(__MODULE__, {:replay, checkpoint_id}, 60_000)
  end

  # Server Callbacks

  @impl true
  def init(_) do
    wal_dir = Application.get_env(:o_supervisor, :wal_dir)
    
    current_segment = get_latest_segment(wal_dir) + 1
    file_path = wal_path(wal_dir, current_segment)
    
    {:ok, file} = File.open(file_path, [:append, :binary])
    
    Logger.info("WALManager initialized, segment: #{current_segment}")
    
    state = %__MODULE__{
      wal_dir: wal_dir,
      current_segment: current_segment,
      file: file,
      entry_count: 0,
      buffer: []
    }
    
    {:ok, state}
  end

  @impl true
  def handle_cast({:append, entry}, state) do
    new_state = write_entry(entry, state)
    {:noreply, new_state}
  end

  @impl true
  def handle_cast({:append_batch, entries}, state) do
    new_state = Enum.reduce(entries, state, &write_entry/2)
    {:noreply, new_state}
  end

  @impl true
  def handle_call({:replay, checkpoint_id}, _from, state) do
    # Get checkpoint metadata to find starting segment
    {:ok, checkpoint_meta} = OSupervisor.MemoryVault.restore_checkpoint(checkpoint_id)
    
    # Read all WAL entries after checkpoint
    entries = read_entries_after(state.wal_dir, checkpoint_meta.timestamp)
    
    {:reply, {:ok, entries}, state}
  end

  @impl true
  def terminate(_reason, state) do
    File.close(state.file)
    :ok
  end

  # Private Functions

  defp write_entry(entry, state) do
    # Add sequence number and timestamp
    entry_with_meta = Map.merge(entry, %{
      sequence: state.entry_count,
      timestamp: System.system_time(:second)
    })
    
    # Serialize
    serialized = :erlang.term_to_binary(entry_with_meta)
    size = byte_size(serialized)
    
    # Write: [size (4 bytes)][data]
    IO.binwrite(state.file, <<size::32>> <> serialized)
    
    new_count = state.entry_count + 1
    
    # Check if need to rotate segment
    if new_count >= @segment_size do
      File.close(state.file)
      
      new_segment = state.current_segment + 1
      file_path = wal_path(state.wal_dir, new_segment)
      {:ok, new_file} = File.open(file_path, [:append, :binary])
      
      Logger.info("WAL segment rotated: #{new_segment}")
      
      %{state | 
        current_segment: new_segment,
        file: new_file,
        entry_count: 0
      }
    else
      %{state | entry_count: new_count}
    end
  end

  defp read_entries_after(wal_dir, timestamp) do
    wal_dir
    |> File.ls!()
    |> Enum.filter(&String.starts_with?(&1, "wal_"))
    |> Enum.sort()
    |> Enum.flat_map(fn filename ->
      path = Path.join(wal_dir, filename)
      read_segment(path)
    end)
    |> Enum.filter(fn entry -> entry.timestamp > timestamp end)
  end

  defp read_segment(path) do
    case File.read(path) do
      {:ok, data} -> parse_wal_data(data, [])
      {:error, _} -> []
    end
  end

  defp parse_wal_data(<<>>, acc), do: Enum.reverse(acc)
  
  defp parse_wal_data(<<size::32, rest::binary>>, acc) do
    <<entry_data::binary-size(size), remaining::binary>> = rest
    entry = :erlang.binary_to_term(entry_data)
    parse_wal_data(remaining, [entry | acc])
  end

  defp wal_path(dir, segment), do: Path.join(dir, "wal_#{segment}.log")

  defp get_latest_segment(wal_dir) do
    case File.ls(wal_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.starts_with?(&1, "wal_"))
        |> Enum.map(fn name ->
          name
          |> String.replace_prefix("wal_", "")
          |> String.replace_suffix(".log", "")
          |> String.to_integer()
        end)
        |> Enum.max(fn -> 0 end)
      
      {:error, _} -> 0
    end
  end
end
```

I'll continue with the remaining components in the next message to avoid hitting token limits. The file is being created successfully!


### Component 5: Health Monitor

**Create `lib/o_supervisor/health_monitor.ex`:**

```elixir
defmodule OSupervisor.HealthMonitor do
  @moduledoc """
  Monitors health and performance metrics of Gerbil instances.
  
  Collects metrics via Telemetry and provides comparison
  functionality for shadow testing.
  """
  
  use GenServer
  require Logger

  defstruct [:metrics_table, :history_table]

  # Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def record_metrics(instance_id, metrics) do
    GenServer.cast(__MODULE__, {:record, instance_id, metrics})
  end

  def get_metrics(instance_id) do
    GenServer.call(__MODULE__, {:get_metrics, instance_id})
  end

  def compare_instances(instance_a, instance_b) do
    GenServer.call(__MODULE__, {:compare, instance_a, instance_b})
  end

  # Server Callbacks

  @impl true
  def init(_) do
    # Create ETS tables for fast metric access
    metrics_table = :ets.new(:health_metrics, [:set, :public, :named_table])
    history_table = :ets.new(:health_history, [:ordered_set, :public, :named_table])
    
    Logger.info("HealthMonitor initialized")
    
    {:ok, %__MODULE__{
      metrics_table: metrics_table,
      history_table: history_table
    }}
  end

  @impl true
  def handle_cast({:record, instance_id, metrics}, state) do
    timestamp = System.system_time(:millisecond)
    
    # Update current metrics
    :ets.insert(state.metrics_table, {instance_id, metrics, timestamp})
    
    # Add to history
    :ets.insert(state.history_table, {{instance_id, timestamp}, metrics})
    
    # Emit telemetry event
    :telemetry.execute(
      [:o_supervisor, :health, :metrics],
      metrics,
      %{instance_id: instance_id}
    )
    
    {:noreply, state}
  end

  @impl true
  def handle_call({:get_metrics, instance_id}, _from, state) do
    case :ets.lookup(state.metrics_table, instance_id) do
      [{^instance_id, metrics, timestamp}] ->
        {:reply, {:ok, metrics, timestamp}, state}
      
      [] ->
        {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call({:compare, instance_a, instance_b}, _from, state) do
    with {:ok, metrics_a, _} <- get_metrics_internal(state, instance_a),
         {:ok, metrics_b, _} <- get_metrics_internal(state, instance_b) do
      
      comparison = %{
        latency_diff: calculate_diff(metrics_a[:avg_latency], metrics_b[:avg_latency]),
        error_rate_diff: calculate_diff(metrics_a[:error_rate], metrics_b[:error_rate]),
        memory_diff: calculate_diff(metrics_a[:memory_usage], metrics_b[:memory_usage]),
        throughput_diff: calculate_diff(metrics_b[:throughput], metrics_a[:throughput])
      }
      
      {:reply, {:ok, comparison}, state}
    else
      error -> {:reply, error, state}
    end
  end

  # Private Functions

  defp get_metrics_internal(state, instance_id) do
    case :ets.lookup(state.metrics_table, instance_id) do
      [{^instance_id, metrics, timestamp}] -> {:ok, metrics, timestamp}
      [] -> {:error, :not_found}
    end
  end

  defp calculate_diff(a, b) when is_number(a) and is_number(b) do
    if b != 0, do: (a - b) / b, else: 0.0
  end
  defp calculate_diff(_, _), do: 0.0
end
```

### Component 6: Evolution Arbiter

**Create `lib/o_supervisor/evolution_arbiter.ex`:**

```elixir
defmodule OSupervisor.EvolutionArbiter do
  @moduledoc """
  Orchestrates shadow testing and evolution experiments.
  
  Manages the lifecycle of shadow instances, evaluates their
  performance, and decides whether to promote or reject changes.
  """
  
  use GenServer
  require Logger

  defstruct [:active_shadows, :evolution_history]

  # Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def start_shadow_test(hypothesis, checkpoint_id) do
    GenServer.cast(__MODULE__, {:shadow_test, hypothesis, checkpoint_id})
  end

  def get_active_shadows do
    GenServer.call(__MODULE__, :get_active_shadows)
  end

  # Server Callbacks

  @impl true
  def init(_) do
    Logger.info("EvolutionArbiter initialized")
    
    {:ok, %__MODULE__{
      active_shadows: %{},
      evolution_history: []
    }}
  end

  @impl true
  def handle_cast({:shadow_test, hypothesis, checkpoint_id}, state) do
    Logger.info("Starting shadow test for hypothesis: #{inspect(hypothesis)}")
    
    # Start shadow instance
    {:ok, shadow_pid} = DynamicSupervisor.start_child(
      OSupervisor.ShadowSupervisor,
      {OSupervisor.GerbilManager, [
        role: :shadow,
        name: :"shadow_#{UUID.uuid4()}",
        checkpoint_id: checkpoint_id
      ]}
    )
    
    # Load new code into shadow
    :ok = OSupervisor.GerbilManager.hot_reload(shadow_pid, hypothesis["new_code"])
    
    # Enable traffic splitting
    test_duration = Application.get_env(:o_supervisor, :shadow_test_duration, 300_000)
    OSupervisor.TrafficSplitter.enable_shadow(shadow_pid, 0.1)
    
    # Schedule evaluation
    Process.send_after(self(), {:evaluate_shadow, shadow_pid, hypothesis}, test_duration)
    
    shadow_data = %{
      hypothesis: hypothesis,
      start_time: System.monotonic_time(:millisecond),
      checkpoint_id: checkpoint_id
    }
    
    {:noreply, put_in(state.active_shadows[shadow_pid], shadow_data)}
  end

  @impl true
  def handle_call(:get_active_shadows, _from, state) do
    {:reply, state.active_shadows, state}
  end

  @impl true
  def handle_info({:evaluate_shadow, shadow_pid, hypothesis}, state) do
    Logger.info("Evaluating shadow instance: #{inspect(shadow_pid)}")
    
    shadow_data = state.active_shadows[shadow_pid]
    
    # Get metrics
    {:ok, main_metrics, _} = OSupervisor.HealthMonitor.get_metrics(:main_gerbil)
    {:ok, shadow_metrics, _} = OSupervisor.HealthMonitor.get_metrics(shadow_pid)
    
    # Compare performance
    {:ok, comparison} = OSupervisor.HealthMonitor.compare_instances(:main_gerbil, shadow_pid)
    
    decision = make_decision(comparison)
    
    case decision do
      :promote ->
        Logger.info("Shadow instance outperformed main, promoting...")
        promote_shadow(shadow_pid, hypothesis)
        
      :reject ->
        Logger.info("Shadow instance underperformed, rejecting...")
        reject_shadow(shadow_pid)
    end
    
    # Record in history
    history_entry = %{
      hypothesis: hypothesis,
      decision: decision,
      comparison: comparison,
      timestamp: System.system_time(:second)
    }
    
    new_history = [history_entry | state.evolution_history]
    
    {:noreply, %{state | 
      active_shadows: Map.delete(state.active_shadows, shadow_pid),
      evolution_history: Enum.take(new_history, 100)  # Keep last 100
    }}
  end

  # Private Functions

  defp make_decision(comparison) do
    # Decision criteria:
    # - Latency improvement > 10% OR
    # - Error rate improvement > 20% AND
    # - No significant memory increase (< 50%)
    
    latency_improved = comparison.latency_diff < -0.1
    error_rate_improved = comparison.error_rate_diff < -0.2
    memory_acceptable = comparison.memory_diff < 0.5
    
    if (latency_improved or error_rate_improved) and memory_acceptable do
      :promote
    else
      :reject
    end
  end

  defp promote_shadow(shadow_pid, hypothesis) do
    # Disable traffic splitting
    OSupervisor.TrafficSplitter.disable_shadow()
    
    # Hot reload main instance with new code
    :ok = OSupervisor.GerbilManager.hot_reload(:main_gerbil, hypothesis["new_code"])
    
    # Terminate shadow
    DynamicSupervisor.terminate_child(OSupervisor.ShadowSupervisor, shadow_pid)
    
    Logger.info("Shadow promoted successfully")
  end

  defp reject_shadow(shadow_pid) do
    # Disable traffic splitting
    OSupervisor.TrafficSplitter.disable_shadow()
    
    # Terminate shadow
    DynamicSupervisor.terminate_child(OSupervisor.ShadowSupervisor, shadow_pid)
    
    Logger.info("Shadow rejected")
  end
end
```

### Component 7: Traffic Splitter

**Create `lib/o_supervisor/traffic_splitter.ex`:**

```elixir
defmodule OSupervisor.TrafficSplitter do
  @moduledoc """
  Routes traffic between main and shadow instances for A/B testing.
  """
  
  use GenServer
  require Logger

  defstruct [
    :main_pid,
    :shadow_pid,
    :split_ratio,
    :request_count,
    :shadow_count
  ]

  # Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def enable_shadow(shadow_pid, ratio \\ 0.1) do
    GenServer.call(__MODULE__, {:enable_shadow, shadow_pid, ratio})
  end

  def disable_shadow do
    GenServer.call(__MODULE__, :disable_shadow)
  end

  def route_request(request) do
    GenServer.call(__MODULE__, {:route, request}, 30_000)
  end

  # Server Callbacks

  @impl true
  def init(_) do
    {:ok, %__MODULE__{
      main_pid: :main_gerbil,
      shadow_pid: nil,
      split_ratio: 0.0,
      request_count: 0,
      shadow_count: 0
    }}
  end

  @impl true
  def handle_call({:enable_shadow, shadow_pid, ratio}, _from, state) do
    Logger.info("Enabling shadow with #{ratio * 100}% traffic")
    {:reply, :ok, %{state | shadow_pid: shadow_pid, split_ratio: ratio}}
  end

  @impl true
  def handle_call(:disable_shadow, _from, state) do
    Logger.info("Disabling shadow")
    {:reply, :ok, %{state | shadow_pid: nil, split_ratio: 0.0}}
  end

  @impl true
  def handle_call({:route, request}, _from, state) do
    new_count = state.request_count + 1
    
    {response, new_shadow_count} = 
      if state.shadow_pid && :rand.uniform() < state.split_ratio do
        # Send to both (shadow for testing, main for actual response)
        {main_response, shadow_response} = send_to_both(
          state.main_pid,
          state.shadow_pid,
          request
        )
        
        # Record comparison
        record_comparison(main_response, shadow_response)
        
        {main_response, state.shadow_count + 1}
      else
        # Send only to main
        response = send_to_instance(state.main_pid, request)
        {response, state.shadow_count}
      end
    
    {:reply, response, %{state | 
      request_count: new_count,
      shadow_count: new_shadow_count
    }}
  end

  # Private Functions

  defp send_to_both(main_pid, shadow_pid, request) do
    # Parallel execution
    main_task = Task.async(fn ->
      {time, response} = :timer.tc(fn -> send_to_instance(main_pid, request) end)
      {response, time}
    end)
    
    shadow_task = Task.async(fn ->
      {time, response} = :timer.tc(fn -> send_to_instance(shadow_pid, request) end)
      {response, time}
    end)
    
    {main_response, main_time} = Task.await(main_task, 30_000)
    {shadow_response, shadow_time} = Task.await(shadow_task, 30_000)
    
    {{main_response, main_time}, {shadow_response, shadow_time}}
  end

  defp send_to_instance(pid, request) do
    message = %{
      type: "process_request",
      data: request,
      timestamp: System.system_time(:millisecond)
    }
    
    OSupervisor.GerbilManager.send_message(pid, message)
  end

  defp record_comparison({main_response, main_time}, {shadow_response, shadow_time}) do
    # Record metrics for comparison
    :telemetry.execute(
      [:o_supervisor, :traffic_splitter, :comparison],
      %{
        main_latency: main_time,
        shadow_latency: shadow_time,
        latency_diff: shadow_time - main_time
      },
      %{}
    )
  end
end
```

### Component 8: Telemetry Setup

**Create `lib/o_supervisor/telemetry.ex`:**

```elixir
defmodule OSupervisor.Telemetry do
  @moduledoc """
  Telemetry setup for metrics collection and monitoring.
  """
  
  use Supervisor
  require Logger

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    children = [
      {:telemetry_poller, measurements: periodic_measurements(), period: 10_000}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp periodic_measurements do
    [
      # VM measurements
      {OSupervisor.Telemetry, :measure_memory, []},
      {OSupervisor.Telemetry, :measure_process_count, []}
    ]
  end

  def measure_memory do
    memory = :erlang.memory()
    
    :telemetry.execute(
      [:vm, :memory],
      %{
        total: memory[:total],
        processes: memory[:processes],
        ets: memory[:ets],
        binary: memory[:binary]
      },
      %{}
    )
  end

  def measure_process_count do
    count = length(Process.list())
    
    :telemetry.execute(
      [:vm, :process_count],
      %{count: count},
      %{}
    )
  end
end
```

---

## Communication Protocol

### Message Types

#### From Gerbil to Elixir

```elixir
# Heartbeat
%{
  "type" => "heartbeat",
  "timestamp" => 1234567890
}

# Checkpoint request
%{
  "type" => "checkpoint",
  "data" => %{
    "agent_state" => "...",
    "memory_blocks" => [...],
    "tool_registry" => %{...}
  }
}

# WAL entry
%{
  "type" => "wal_entry",
  "data" => %{
    "operation" => "memory_add",
    "params" => %{...},
    "timestamp" => 1234567890
  }
}

# Evolution intent
%{
  "type" => "evolution_intent",
  "hypothesis" => %{
    "reason" => "Optimize memory search",
    "target" => "memory_system",
    "new_code" => "..."
  }
}

# Metrics
%{
  "type" => "metrics",
  "data" => %{
    "avg_latency" => 10.5,
    "error_rate" => 0.01,
    "memory_usage" => 80_000_000,
    "throughput" => 1000
  }
}
```

#### From Elixir to Gerbil

```elixir
# Checkpoint acknowledgment
%{
  "type" => "checkpoint_ack",
  "checkpoint_id" => "uuid-here",
  "timestamp" => 1234567890
}

# Restore command
%{
  "type" => "restore",
  "checkpoint_id" => "uuid-here",
  "wal_entries" => [...]
}

# Hot reload
%{
  "type" => "hot_reload",
  "code" => "...",
  "timestamp" => 1234567890
}

# Evolution result
%{
  "type" => "evolution_result",
  "success" => true,
  "metrics" => %{...}
}
```

---

## Checkpoint & Recovery

### Creating a Checkpoint

**Gerbil side:**

```scheme
;; gerbil/agent/elixir-bridge.ss

(def (create-checkpoint! agent)
  (let ((snapshot (serialize-agent-state agent)))
    ;; Send to Elixir
    (elixir-send "checkpoint" snapshot)
    
    ;; Wait for acknowledgment
    (let ((ack (wait-for-message "checkpoint_ack" timeout: 30)))
      (hash-ref ack 'checkpoint_id))))
```

**Elixir side:**

```elixir
# Automatically handled by GerbilManager
# Checkpoint saved to MemoryVault
# Acknowledgment sent back to Gerbil
```

### Recovering from Crash

**Automatic recovery flow:**

1. Elixir detects heartbeat timeout
2. Supervisor restarts GerbilManager
3. GerbilManager starts new Gerbil process with `--restore checkpoint_id`
4. Gerbil loads checkpoint from Elixir
5. Gerbil replays WAL entries
6. Agent resumes from last known state

**Manual recovery:**

```elixir
# List available checkpoints
checkpoints = OSupervisor.MemoryVault.list_checkpoints()

# Restore specific checkpoint
{:ok, data} = OSupervisor.MemoryVault.restore_checkpoint("checkpoint-id")

# Start Gerbil with checkpoint
{:ok, pid} = OSupervisor.GerbilManager.start_link(
  role: :main,
  checkpoint_id: "checkpoint-id"
)
```

---

## Shadow Testing

### Starting a Shadow Test

```elixir
# Define hypothesis
hypothesis = %{
  "reason" => "Optimize vector search with SIMD",
  "target" => "memory_system",
  "new_code" => File.read!("optimized_memory.ss")
}

# Start shadow test
OSupervisor.EvolutionArbiter.start_shadow_test(hypothesis, checkpoint_id)

# Shadow will run for configured duration (default 5 minutes)
# Elixir will automatically evaluate and decide
```

### Monitoring Shadow Performance

```elixir
# Get active shadows
shadows = OSupervisor.EvolutionArbiter.get_active_shadows()

# Get metrics for specific shadow
{:ok, metrics, _timestamp} = OSupervisor.HealthMonitor.get_metrics(shadow_pid)

# Compare with main
{:ok, comparison} = OSupervisor.HealthMonitor.compare_instances(:main_gerbil, shadow_pid)

IO.inspect(comparison)
# %{
#   latency_diff: -0.15,      # 15% faster
#   error_rate_diff: -0.05,   # 5% fewer errors
#   memory_diff: 0.10,        # 10% more memory
#   throughput_diff: 0.20     # 20% higher throughput
# }
```

---

## Multi-Threaded Evolution

### Genetic Algorithm Evolution

**Create `lib/o_supervisor/genetic_evolution.ex`:**

```elixir
defmodule OSupervisor.GeneticEvolution do
  @moduledoc """
  Genetic algorithm-based evolution for agent code.
  """
  
  require Logger

  @population_size 50
  @mutation_rate 0.1
  @crossover_rate 0.7
  @generations 10

  def evolve(base_hypothesis) do
    Logger.info("Starting genetic evolution with population: #{@population_size}")
    
    # Initialize population
    population = initialize_population(base_hypothesis, @population_size)
    
    # Evolve for N generations
    final_population = Enum.reduce(1..@generations, population, fn gen, pop ->
      Logger.info("Generation #{gen}/#{@generations}")
      
      # Evaluate fitness
      evaluated = evaluate_fitness(pop)
      
      # Selection
      selected = tournament_selection(evaluated, div(@population_size, 2))
      
      # Crossover
      offspring = crossover(selected, @crossover_rate)
      
      # Mutation
      mutated = mutate(offspring, @mutation_rate)
      
      # Elitism (keep top 5)
      elite = Enum.take(evaluated, 5)
      
      elite ++ mutated
    end)
    
    # Return best individual
    Enum.max_by(final_population, & &1.fitness)
  end

  defp initialize_population(base, size) do
    Enum.map(1..size, fn _ ->
      %{
        code: mutate_code(base["new_code"]),
        fitness: 0.0,
        id: UUID.uuid4()
      }
    end)
  end

  defp evaluate_fitness(population) do
    # Parallel evaluation
    population
    |> Task.async_stream(
      fn individual ->
        # Start shadow instance
        {:ok, shadow_pid} = start_shadow_instance(individual.code)
        
        # Run benchmark
        metrics = run_benchmark(shadow_pid, 100)
        
        # Calculate fitness
        fitness = calculate_fitness(metrics)
        
        # Cleanup
        stop_shadow_instance(shadow_pid)
        
        %{individual | fitness: fitness}
      end,
      max_concurrency: 10,
      timeout: 60_000
    )
    |> Enum.map(fn {:ok, result} -> result end)
    |> Enum.sort_by(& &1.fitness, :desc)
  end

  defp calculate_fitness(metrics) do
    # Multi-objective optimization
    speed_score = 1.0 / max(metrics.avg_latency, 0.001)
    accuracy_score = 1.0 - metrics.error_rate
    resource_score = 1.0 / max(metrics.memory_usage / 1_000_000, 1.0)
    
    0.5 * speed_score + 0.3 * accuracy_score + 0.2 * resource_score
  end

  defp tournament_selection(population, count) do
    Enum.map(1..count, fn _ ->
      population
      |> Enum.take_random(3)
      |> Enum.max_by(& &1.fitness)
    end)
  end

  defp crossover(parents, rate) do
    parents
    |> Enum.chunk_every(2)
    |> Enum.flat_map(fn
      [p1, p2] ->
        if :rand.uniform() < rate do
          [crossover_code(p1, p2), crossover_code(p2, p1)]
        else
          [p1, p2]
        end
      
      [p1] -> [p1]
    end)
  end

  defp mutate(population, rate) do
    Enum.map(population, fn individual ->
      if :rand.uniform() < rate do
        %{individual | code: mutate_code(individual.code)}
      else
        individual
      end
    end)
  end

  defp mutate_code(code) do
    # Simple mutation: randomly modify parameters
    # In practice, this would use AST manipulation
    code
  end

  defp crossover_code(p1, p2) do
    # Simple crossover: mix code from both parents
    # In practice, this would use AST manipulation
    %{p1 | code: p1.code}
  end

  defp start_shadow_instance(code) do
    DynamicSupervisor.start_child(
      OSupervisor.ShadowSupervisor,
      {OSupervisor.GerbilManager, [
        role: :shadow,
        name: :"shadow_#{UUID.uuid4()}"
      ]}
    )
  end

  defp stop_shadow_instance(pid) do
    DynamicSupervisor.terminate_child(OSupervisor.ShadowSupervisor, pid)
  end

  defp run_benchmark(pid, request_count) do
    # Send test requests and collect metrics
    %{
      avg_latency: 10.0,
      error_rate: 0.01,
      memory_usage: 80_000_000
    }
  end
end
```

---

## Deployment

### Development Mode

```bash
# Terminal 1: Start Elixir supervisor
cd o_supervisor
mix deps.get
iex -S mix

# Terminal 2: Monitor logs
tail -f data/logs/o_supervisor.log
```

### Production Mode

```bash
# Build release
cd o_supervisor
MIX_ENV=prod mix release

# Run release
_build/prod/rel/o_supervisor/bin/o_supervisor start

# Or as daemon
_build/prod/rel/o_supervisor/bin/o_supervisor daemon
```

### Docker Deployment

See `docker-compose.yml` for containerized deployment.

---

## Troubleshooting

### Issue: Gerbil process won't start

**Symptoms:** GerbilManager fails to start, port errors

**Solutions:**
1. Check Gerbil installation: `gerbil version`
2. Verify GERBIL_HOME environment variable
3. Check file permissions on gerbil executable
4. Review logs: `tail -f data/logs/gerbil_manager.log`

### Issue: Heartbeat timeout

**Symptoms:** Constant restarts, "heartbeat timeout" in logs

**Solutions:**
1. Increase timeout in config: `heartbeat_timeout: 10000`
2. Check Gerbil process CPU usage
3. Verify network connectivity (if using remote Gerbil)
4. Check for infinite loops in Gerbil code

### Issue: Checkpoint restore fails

**Symptoms:** "Checkpoint not found" errors

**Solutions:**
1. Verify checkpoint directory exists and is writable
2. Check DETS file integrity: `ls -lh data/checkpoints/`
3. Try file-based restore as fallback
4. Check disk space

### Issue: Shadow instances consume too much memory

**Symptoms:** System OOM, slow performance

**Solutions:**
1. Reduce `max_concurrent_shadows` in config
2. Implement resource limits per shadow
3. Use COW (copy-on-write) for shadow creation
4. Monitor with: `Observer.start()`

---

## Next Steps

1. **Read** `IMPLEMENTATION_CHECKLIST.md` for step-by-step implementation guide
2. **Review** `docs/adr/` for architecture decision records
3. **Explore** example code in `examples/`
4. **Run** tests: `mix test`
5. **Deploy** to production following deployment guide

---

**Version:** 1.0  
**Last Updated:** 2026-01-16  
**Status:** Ready for implementation
