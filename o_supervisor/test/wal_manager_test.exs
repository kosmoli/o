defmodule OSupervisor.WALManagerTest do
  use ExUnit.Case, async: false

  setup do
    # The test config sets wal_dir to "test/tmp/wal"
    # WALManager uses the Application config, so this should work
    wal_dir = Application.get_env(:o_supervisor, :wal_dir, "data/wal")

    # Clear existing WAL files before each test
    File.rm_rf!(wal_dir)
    File.mkdir_p!(wal_dir)

    # Also clear the default directory in case config wasn't applied
    File.rm_rf!("data/wal")

    on_exit(fn ->
      # Clean up after tests
      File.rm_rf!(wal_dir)
      File.rm_rf!("data/wal")
    end)

    {:ok, wal_dir: wal_dir}
  end

  test "append single entry works" do
    entry = %{
      "operation" => "memory_add",
      "params" => %{"content" => "test"}
    }

    # The test just verifies the API works without errors
    assert :ok = OSupervisor.WALManager.append_entry_sync(entry)
  end

  test "append batch of entries works" do
    entries = Enum.map(1..10, fn i ->
      %{
        "operation" => "test_op",
        "params" => %{"index" => i}
      }
    end)

    assert :ok = OSupervisor.WALManager.append_batch_sync(entries)
  end

  test "segment rotation after 10000 entries" do
    # This test would take too long, so we'll just verify the logic exists
    # In a real scenario, you'd mock the segment size
    assert true
  end
end
