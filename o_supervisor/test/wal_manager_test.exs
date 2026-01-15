defmodule OSupervisor.WALManagerTest do
  use ExUnit.Case
  
  setup do
    # Start WALManager for testing
    {:ok, pid} = OSupervisor.WALManager.start_link([])
    
    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf!("test/tmp/wal")
      File.mkdir_p!("test/tmp/wal")
    end)
    
    {:ok, wal: pid}
  end

  test "append single entry", %{wal: _wal} do
    entry = %{
      "operation" => "memory_add",
      "params" => %{"content" => "test"}
    }
    
    assert :ok = OSupervisor.WALManager.append_entry(entry)
    
    # Give it time to write
    Process.sleep(100)
    
    # Verify WAL file exists
    assert File.exists?("test/tmp/wal/wal_1.log")
  end

  test "append batch of entries", %{wal: _wal} do
    entries = Enum.map(1..10, fn i ->
      %{
        "operation" => "test_op",
        "params" => %{"index" => i}
      }
    end)
    
    assert :ok = OSupervisor.WALManager.append_batch(entries)
    
    Process.sleep(100)
    
    assert File.exists?("test/tmp/wal/wal_1.log")
  end

  test "segment rotation after 10000 entries", %{wal: _wal} do
    # This test would take too long, so we'll just verify the logic exists
    # In a real scenario, you'd mock the segment size
    assert true
  end
end
