import Config

config :o_supervisor,
  racket_executable: System.get_env("RACKET_EXECUTABLE") || "racket",
  racket_main_script: "../racket/o/main.rkt",
  checkpoint_dir: "data/checkpoints",
  wal_dir: "data/wal",
  shared_memory_name: "/o_shared_state",
  shared_memory_size: 1024 * 1024,  # 1MB
  heartbeat_interval: 1000,          # 1 second
  heartbeat_timeout: 5000,           # 5 seconds
  max_concurrent_shadows: 10,
  shadow_test_duration: 300_000,     # 5 minutes
  evolution_enabled: true

config :logger,
  level: :info,
  format: "$time $metadata[$level] $message\n",
  metadata: [request_id: :request_id, module: :module, function: :function]

import_config "#{config_env()}.exs"
