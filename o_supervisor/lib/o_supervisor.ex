defmodule OSupervisor do
  @moduledoc """
  OSupervisor - Elixir/OTP supervision layer for O self-evolving agent system.

  Provides industrial-grade fault tolerance, state persistence, and evolution
  management for Racket-based agents.
  """

  @doc """
  Returns the current version of OSupervisor.
  """
  def version, do: "0.1.0"

  @doc """
  Returns the status of the main Racket instance.
  """
  def status do
    # Check if RacketManager is running before calling it
    case Process.whereis(:main_racket) do
      nil -> {:error, :not_running}
      _pid ->
        case OSupervisor.RacketManager.get_state(:main_racket) do
          state when is_map(state) ->
            {:ok, %{
              role: state.role,
              uptime: System.system_time(:second) - state.start_time,
              checkpoint_id: state.checkpoint_id,
              last_heartbeat: state.last_heartbeat
            }}

          _ ->
            {:error, :not_running}
        end
    end
  end

  @doc """
  Lists all active shadow instances.
  """
  def list_shadows do
    OSupervisor.EvolutionArbiter.get_active_shadows()
  end

  @doc """
  Lists all available checkpoints.
  """
  def list_checkpoints do
    OSupervisor.MemoryVault.list_checkpoints()
  end
end
