defmodule OSupervisorTest do
  use ExUnit.Case
  doctest OSupervisor

  test "returns version" do
    assert OSupervisor.version() == "0.1.0"
  end

  test "status returns error when not running" do
    assert {:error, :not_running} = OSupervisor.status()
  end
end
