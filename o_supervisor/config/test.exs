import Config

config :o_supervisor,
  evolution_enabled: false,
  max_concurrent_shadows: 2

config :logger, level: :warning
