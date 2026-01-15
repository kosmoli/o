import Config

config :o_supervisor,
  evolution_enabled: true,
  max_concurrent_shadows: 20

config :logger, level: :info
