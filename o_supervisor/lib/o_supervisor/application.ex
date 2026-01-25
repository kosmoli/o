defmodule OSupervisor.Application do
  @moduledoc """
  The OSupervisor Application.

  Root supervisor managing all components of the Elixir supervision layer.
  """

  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    Logger.info("Starting O Supervisor Application v#{OSupervisor.version()}...")

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

      # Main Racket process manager (only if enabled)
      if Application.get_env(:o_supervisor, :start_racket_manager, true) do
        {OSupervisor.RacketManager, [role: :main, name: :main_racket]}
      else
        nil
      end,

      # Traffic splitter for shadow testing
      {OSupervisor.TrafficSplitter, []}
    ]

    # Filter out nil children
    children = Enum.reject(children, &is_nil/1)

    opts = [strategy: :one_for_one, name: OSupervisor.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp ensure_data_directories! do
    config = Application.get_all_env(:o_supervisor)

    [config[:checkpoint_dir], config[:wal_dir]]
    |> Enum.each(fn dir ->
      if dir, do: File.mkdir_p!(dir)
      Logger.info("Ensured directory exists: #{dir}")
    end)
  end
end
