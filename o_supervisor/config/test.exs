import Config

config :o_supervisor,
  evolution_enabled: false,
  max_concurrent_shadows: 2,
  racket_executable: "echo",
  checkpoint_dir: "test/tmp/checkpoints",
  wal_dir: "test/tmp/wal",
  heartbeat_timeout: 1000,
  start_racket_manager: false

config :logger, level: :warning
