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

  def append_entry_sync(entry) do
    GenServer.call(__MODULE__, {:append, entry})
  end

  def append_batch(entries) do
    GenServer.cast(__MODULE__, {:append_batch, entries})
  end

  def append_batch_sync(entries) do
    GenServer.call(__MODULE__, {:append_batch, entries})
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
  def handle_call({:append, entry}, _from, state) do
    new_state = write_entry(entry, state)
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:append_batch, entries}, _from, state) do
    new_state = Enum.reduce(entries, state, &write_entry/2)
    {:reply, :ok, new_state}
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
