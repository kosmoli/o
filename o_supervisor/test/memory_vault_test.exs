defmodule OSupervisor.MemoryVaultTest do
  use ExUnit.Case
  
  setup do
    # Start MemoryVault for testing
    {:ok, pid} = OSupervisor.MemoryVault.start_link([])
    
    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf!("test/tmp/checkpoints")
      File.mkdir_p!("test/tmp/checkpoints")
    end)
    
    {:ok, vault: pid}
  end

  test "save and restore checkpoint", %{vault: _vault} do
    checkpoint_id = "test-checkpoint-1"
    snapshot = %{"agent_state" => %{"id" => "test-agent"}}
    wal_entries = [%{"operation" => "test", "sequence" => 1}]
    
    # Save checkpoint
    assert :ok = OSupervisor.MemoryVault.save_checkpoint(
      checkpoint_id,
      snapshot,
      wal_entries
    )
    
    # Restore checkpoint
    assert {:ok, result} = OSupervisor.MemoryVault.restore_checkpoint(checkpoint_id)
    assert result.snapshot == snapshot
    assert result.wal_entries == wal_entries
  end

  test "list checkpoints", %{vault: _vault} do
    # Save multiple checkpoints
    OSupervisor.MemoryVault.save_checkpoint("cp1", %{}, [])
    OSupervisor.MemoryVault.save_checkpoint("cp2", %{}, [])
    
    # List checkpoints
    checkpoints = OSupervisor.MemoryVault.list_checkpoints()
    
    assert length(checkpoints) == 2
    assert Enum.any?(checkpoints, fn cp -> cp.id == "cp1" end)
    assert Enum.any?(checkpoints, fn cp -> cp.id == "cp2" end)
  end

  test "delete checkpoint", %{vault: _vault} do
    checkpoint_id = "test-checkpoint-delete"
    
    # Save checkpoint
    OSupervisor.MemoryVault.save_checkpoint(checkpoint_id, %{}, [])
    
    # Delete checkpoint
    assert :ok = OSupervisor.MemoryVault.delete_checkpoint(checkpoint_id)
    
    # Verify deleted
    assert {:error, :not_found} = 
      OSupervisor.MemoryVault.restore_checkpoint(checkpoint_id)
  end

  test "restore non-existent checkpoint returns error", %{vault: _vault} do
    assert {:error, :not_found} = 
      OSupervisor.MemoryVault.restore_checkpoint("non-existent")
  end
end
