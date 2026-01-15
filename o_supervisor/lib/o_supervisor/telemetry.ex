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
