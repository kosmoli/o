import Config

config :o_supervisor,
  evolution_enabled: true,
  max_concurrent_shadows: 5

config :logger, level: :debug
