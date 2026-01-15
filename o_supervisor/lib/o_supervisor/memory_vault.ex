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
