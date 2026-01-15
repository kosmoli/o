defmodule OSupervisor.MixProject do
  use Mix.Project

  def project do
    [
      app: :o_supervisor,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :crypto],
      mod: {OSupervisor.Application, []}
    ]
  end

  defp deps do
    [
      {:msgpax, "~> 2.3"},           # MessagePack serialization
      {:jason, "~> 1.4"},            # JSON (for config)
      {:postgrex, "~> 0.17"},        # PostgreSQL driver
      {:telemetry, "~> 1.2"},        # Metrics
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:uuid, "~> 1.1"},             # UUID generation
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false},
      {:ex_doc, "~> 0.29", only: :dev, runtime: false}
    ]
  end
end
