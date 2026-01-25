defmodule OSupervisor.EvolutionArbiter do
  @moduledoc """
  Orchestrates shadow testing and evolution experiments.

  Manages the lifecycle of shadow instances, evaluates their
  performance, and decides whether to promote or reject changes.
  """

  use GenServer
  require Logger

  defstruct [:active_shadows, :evolution_history]

  # Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def start_shadow_test(hypothesis, checkpoint_id) do
    GenServer.cast(__MODULE__, {:shadow_test, hypothesis, checkpoint_id})
  end

  def get_active_shadows do
    GenServer.call(__MODULE__, :get_active_shadows)
  end

  # Server Callbacks

  @impl true
  def init(_) do
    Logger.info("EvolutionArbiter initialized")

    {:ok, %__MODULE__{
      active_shadows: %{},
      evolution_history: []
    }}
  end

  @impl true
  def handle_cast({:shadow_test, hypothesis, checkpoint_id}, state) do
    Logger.info("Starting shadow test for hypothesis: #{inspect(hypothesis)}")

    # Start shadow instance
    {:ok, shadow_pid} = DynamicSupervisor.start_child(
      OSupervisor.ShadowSupervisor,
      {OSupervisor.RacketManager, [
        role: :shadow,
        name: :"shadow_#{UUID.uuid4()}",
        checkpoint_id: checkpoint_id
      ]}
    )

    # Load new code into shadow
    :ok = OSupervisor.RacketManager.hot_reload(shadow_pid, hypothesis["new_code"])

    # Enable traffic splitting
    test_duration = Application.get_env(:o_supervisor, :shadow_test_duration, 300_000)
    OSupervisor.TrafficSplitter.enable_shadow(shadow_pid, 0.1)

    # Schedule evaluation
    Process.send_after(self(), {:evaluate_shadow, shadow_pid, hypothesis}, test_duration)

    shadow_data = %{
      hypothesis: hypothesis,
      start_time: System.monotonic_time(:millisecond),
      checkpoint_id: checkpoint_id
    }

    {:noreply, put_in(state.active_shadows[shadow_pid], shadow_data)}
  end

  @impl true
  def handle_call(:get_active_shadows, _from, state) do
    {:reply, state.active_shadows, state}
  end

  @impl true
  def handle_info({:evaluate_shadow, shadow_pid, hypothesis}, state) do
    Logger.info("Evaluating shadow instance: #{inspect(shadow_pid)}")

    shadow_data = state.active_shadows[shadow_pid]

    # Get metrics
    {:ok, _main_metrics, _} = OSupervisor.HealthMonitor.get_metrics(:main_racket)
    {:ok, _shadow_metrics, _} = OSupervisor.HealthMonitor.get_metrics(shadow_pid)

    # Compare performance
    {:ok, comparison} = OSupervisor.HealthMonitor.compare_instances(:main_racket, shadow_pid)

    decision = make_decision(comparison)

    case decision do
      :promote ->
        Logger.info("Shadow instance outperformed main, promoting...")
        promote_shadow(shadow_pid, hypothesis)

      :reject ->
        Logger.info("Shadow instance underperformed, rejecting...")
        reject_shadow(shadow_pid)
    end

    # Record in history
    history_entry = %{
      hypothesis: hypothesis,
      decision: decision,
      comparison: comparison,
      timestamp: System.system_time(:second)
    }

    new_history = [history_entry | state.evolution_history]

    {:noreply, %{state |
      active_shadows: Map.delete(state.active_shadows, shadow_pid),
      evolution_history: Enum.take(new_history, 100)  # Keep last 100
    }}
  end

  # Private Functions

  defp make_decision(comparison) do
    # Decision criteria:
    # - Latency improvement > 10% OR
    # - Error rate improvement > 20% AND
    # - No significant memory increase (< 50%)

    latency_improved = comparison.latency_diff < -0.1
    error_rate_improved = comparison.error_rate_diff < -0.2
    memory_acceptable = comparison.memory_diff < 0.5

    if (latency_improved or error_rate_improved) and memory_acceptable do
      :promote
    else
      :reject
    end
  end

  defp promote_shadow(shadow_pid, hypothesis) do
    # Disable traffic splitting
    OSupervisor.TrafficSplitter.disable_shadow()

    # Hot reload main instance with new code
    :ok = OSupervisor.RacketManager.hot_reload(:main_racket, hypothesis["new_code"])

    # Terminate shadow
    DynamicSupervisor.terminate_child(OSupervisor.ShadowSupervisor, shadow_pid)

    Logger.info("Shadow promoted successfully")
  end

  defp reject_shadow(shadow_pid) do
    # Disable traffic splitting
    OSupervisor.TrafficSplitter.disable_shadow()

    # Terminate shadow
    DynamicSupervisor.terminate_child(OSupervisor.ShadowSupervisor, shadow_pid)

    Logger.info("Shadow rejected")
  end
end
