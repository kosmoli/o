defmodule OSupervisor.TrafficSplitter do
  @moduledoc """
  Routes traffic between main and shadow instances for A/B testing.
  """
  
  use GenServer
  require Logger

  defstruct [
    :main_pid,
    :shadow_pid,
    :split_ratio,
    :request_count,
    :shadow_count
  ]

  # Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def enable_shadow(shadow_pid, ratio \\ 0.1) do
    GenServer.call(__MODULE__, {:enable_shadow, shadow_pid, ratio})
  end

  def disable_shadow do
    GenServer.call(__MODULE__, :disable_shadow)
  end

  def route_request(request) do
    GenServer.call(__MODULE__, {:route, request}, 30_000)
  end

  # Server Callbacks

  @impl true
  def init(_) do
    {:ok, %__MODULE__{
      main_pid: :main_gerbil,
      shadow_pid: nil,
      split_ratio: 0.0,
      request_count: 0,
      shadow_count: 0
    }}
  end

  @impl true
  def handle_call({:enable_shadow, shadow_pid, ratio}, _from, state) do
    Logger.info("Enabling shadow with #{ratio * 100}% traffic")
    {:reply, :ok, %{state | shadow_pid: shadow_pid, split_ratio: ratio}}
  end

  @impl true
  def handle_call(:disable_shadow, _from, state) do
    Logger.info("Disabling shadow")
    {:reply, :ok, %{state | shadow_pid: nil, split_ratio: 0.0}}
  end

  @impl true
  def handle_call({:route, request}, _from, state) do
    new_count = state.request_count + 1
    
    {response, new_shadow_count} = 
      if state.shadow_pid && :rand.uniform() < state.split_ratio do
        # Send to both (shadow for testing, main for actual response)
        {main_response, shadow_response} = send_to_both(
          state.main_pid,
          state.shadow_pid,
          request
        )
        
        # Record comparison
        record_comparison(main_response, shadow_response)
        
        {main_response, state.shadow_count + 1}
      else
        # Send only to main
        response = send_to_instance(state.main_pid, request)
        {response, state.shadow_count}
      end
    
    {:reply, response, %{state | 
      request_count: new_count,
      shadow_count: new_shadow_count
    }}
  end

  # Private Functions

  defp send_to_both(main_pid, shadow_pid, request) do
    # Parallel execution
    main_task = Task.async(fn ->
      {time, response} = :timer.tc(fn -> send_to_instance(main_pid, request) end)
      {response, time}
    end)
    
    shadow_task = Task.async(fn ->
      {time, response} = :timer.tc(fn -> send_to_instance(shadow_pid, request) end)
      {response, time}
    end)
    
    {main_response, main_time} = Task.await(main_task, 30_000)
    {shadow_response, shadow_time} = Task.await(shadow_task, 30_000)
    
    {{main_response, main_time}, {shadow_response, shadow_time}}
  end

  defp send_to_instance(pid, request) do
    message = %{
      type: "process_request",
      data: request,
      timestamp: System.system_time(:millisecond)
    }
    
    OSupervisor.GerbilManager.send_message(pid, message)
  end

  defp record_comparison({_main_response, main_time}, {_shadow_response, shadow_time}) do
    # Record metrics for comparison
    :telemetry.execute(
      [:o_supervisor, :traffic_splitter, :comparison],
      %{
        main_latency: main_time,
        shadow_latency: shadow_time,
        latency_diff: shadow_time - main_time
      },
      %{}
    )
  end
end
