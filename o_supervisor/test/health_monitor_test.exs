defmodule OSupervisor.HealthMonitorTest do
  use ExUnit.Case
  
  setup do
    {:ok, pid} = OSupervisor.HealthMonitor.start_link([])
    
    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
    end)
    
    {:ok, monitor: pid}
  end

  test "record and retrieve metrics", %{monitor: _monitor} do
    metrics = %{
      avg_latency: 10.5,
      error_rate: 0.01,
      memory_usage: 80_000_000,
      throughput: 1000
    }
    
    OSupervisor.HealthMonitor.record_metrics(:test_instance, metrics)
    
    assert {:ok, retrieved_metrics, _timestamp} = 
      OSupervisor.HealthMonitor.get_metrics(:test_instance)
    
    assert retrieved_metrics == metrics
  end

  test "compare two instances", %{monitor: _monitor} do
    metrics_a = %{
      avg_latency: 10.0,
      error_rate: 0.01,
      memory_usage: 80_000_000,
      throughput: 1000
    }
    
    metrics_b = %{
      avg_latency: 8.0,
      error_rate: 0.005,
      memory_usage: 85_000_000,
      throughput: 1200
    }
    
    OSupervisor.HealthMonitor.record_metrics(:instance_a, metrics_a)
    OSupervisor.HealthMonitor.record_metrics(:instance_b, metrics_b)
    
    assert {:ok, comparison} = 
      OSupervisor.HealthMonitor.compare_instances(:instance_a, :instance_b)
    
    # Instance B should be faster (negative latency_diff)
    assert comparison.latency_diff < 0
    # Instance B should have fewer errors
    assert comparison.error_rate_diff < 0
    # Instance B uses more memory
    assert comparison.memory_diff > 0
  end

  test "get metrics for non-existent instance returns error", %{monitor: _monitor} do
    assert {:error, :not_found} = 
      OSupervisor.HealthMonitor.get_metrics(:non_existent)
  end
end
