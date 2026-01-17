defmodule OSupervisor do
  @moduledoc """
  OSupervisor - Elixir/OTP supervision layer for O self-evolving agent system.
  
  Provides industrial-grade fault tolerance, state persistence, and evolution
  management for Gerbil-based agents.
  """

  @doc """
  Returns the current version of OSupervisor.
  """
  def version, do: "0.1.0"

  @doc """
  Returns the status of the main Gerbil instance.
  """
  def status do
    try do
      case OSupervisor.GerbilManager.get_state(:main_gerbil) do
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
    catch
      :exit, _ -> {:error, :not_running}
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
