ExUnit.start()

# Configure test environment
Application.put_env(:o_supervisor, :gerbil_executable, "echo")
Application.put_env(:o_supervisor, :checkpoint_dir, "test/tmp/checkpoints")
Application.put_env(:o_supervisor, :wal_dir, "test/tmp/wal")
Application.put_env(:o_supervisor, :heartbeat_timeout, 1000)
Application.put_env(:o_supervisor, :evolution_enabled, false)

# Ensure test directories exist
File.mkdir_p!("test/tmp/checkpoints")
File.mkdir_p!("test/tmp/wal")

# Clean up after tests
ExUnit.after_suite(fn _ ->
  File.rm_rf!("test/tmp")
end)
