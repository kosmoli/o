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
