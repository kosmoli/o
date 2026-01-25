defmodule OSupervisor.MemoryVaultTest do
  use ExUnit.Case, async: false

  setup do
    # Clear existing checkpoints
    File.rm_rf!("test/tmp/checkpoints")
    File.mkdir_p!("test/tmp/checkpoints")

    on_exit(fn ->
      File.rm_rf!("test/tmp/checkpoints")
    end)

    :ok
  end

  test "save and restore checkpoint" do
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

  test "list checkpoints" do
    # Save multiple checkpoints
    OSupervisor.MemoryVault.save_checkpoint("cp1", %{}, [])
    OSupervisor.MemoryVault.save_checkpoint("cp2", %{}, [])

    # List checkpoints
    checkpoints = OSupervisor.MemoryVault.list_checkpoints()

    assert length(checkpoints) >= 2
    assert Enum.any?(checkpoints, fn cp -> cp.id == "cp1" end)
    assert Enum.any?(checkpoints, fn cp -> cp.id == "cp2" end)
  end

  test "delete checkpoint" do
    checkpoint_id = "test-checkpoint-delete"

    # Save checkpoint
    OSupervisor.MemoryVault.save_checkpoint(checkpoint_id, %{}, [])

    # Delete checkpoint
    assert :ok = OSupervisor.MemoryVault.delete_checkpoint(checkpoint_id)

    # Verify deleted
    assert {:error, :not_found} =
      OSupervisor.MemoryVault.restore_checkpoint(checkpoint_id)
  end

  test "restore non-existent checkpoint returns error" do
    assert {:error, :not_found} =
      OSupervisor.MemoryVault.restore_checkpoint("non-existent")
  end
end
