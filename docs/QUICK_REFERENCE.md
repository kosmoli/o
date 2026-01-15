# Quick Reference Guide

Fast lookup for common commands, patterns, and configurations.

---

## Table of Contents

1. [Commands](#commands)
2. [Configuration](#configuration)
3. [Message Types](#message-types)
4. [Code Patterns](#code-patterns)
5. [Troubleshooting](#troubleshooting)

---

## Commands

### Makefile Commands

```bash
# Setup & Installation
make setup          # Initial project setup
make install        # Install dependencies
make init           # Full initialization (setup + install + compile)

# Development
make compile        # Compile code
make test           # Run tests
make test-watch     # Run tests in watch mode
make format         # Format code
make lint           # Run linters
make clean          # Clean build artifacts

# Running
make dev            # Start development server
make iex            # Start interactive shell
make release        # Build production release

# Docker
make docker-build   # Build Docker images
make docker-up      # Start services
make docker-down    # Stop services
make docker-logs    # View logs

# Utilities
make check          # Run all checks
make ci             # Run CI pipeline locally
make docs           # Generate documentation
make version        # Show version info
make status         # Show project status
```

### Mix Commands

```bash
# Dependencies
mix deps.get        # Install dependencies
mix deps.update     # Update dependencies
mix deps.clean      # Clean dependencies

# Compilation
mix compile         # Compile project
mix compile --force # Force recompilation

# Testing
mix test                    # Run all tests
mix test test/file_test.exs # Run specific file
mix test --cover            # With coverage
mix test --only integration # Only integration tests
mix test.watch              # Watch mode

# Code Quality
mix format                      # Format code
mix format --check-formatted    # Check formatting
mix credo                       # Run linter
mix credo --strict              # Strict mode
mix dialyzer                    # Type checking

# Running
mix run                 # Run application
mix run --no-halt       # Run without halting
iex -S mix              # Interactive shell

# Release
MIX_ENV=prod mix release        # Build release
mix release.init                # Initialize release config

# Documentation
mix docs                # Generate docs
mix hex.docs open       # Open docs in browser
```

### Gerbil Commands

```bash
# Version
gerbil version          # Show version

# REPL
gerbil repl             # Start REPL

# Compilation
gerbil compile file.ss  # Compile file
gerbil build            # Build project

# Running
gerbil run file.ss      # Run file
gerbil eval '(+ 1 2)'   # Evaluate expression

# Package Management
gxpkg install package   # Install package
gxpkg update            # Update packages
```

### Docker Commands

```bash
# Compose
docker-compose up -d            # Start services
docker-compose down             # Stop services
docker-compose ps               # List services
docker-compose logs -f          # Follow logs
docker-compose logs service     # Logs for specific service
docker-compose restart          # Restart all
docker-compose restart service  # Restart specific service

# Images
docker-compose build            # Build images
docker-compose build --no-cache # Build without cache
docker-compose pull             # Pull images

# Cleanup
docker-compose down -v          # Stop and remove volumes
docker system prune -a          # Clean all unused data
```

### Git Commands

```bash
# Branching
git checkout -b feature/name    # Create feature branch
git checkout main               # Switch to main
git branch -d feature/name      # Delete branch

# Committing
git add .                       # Stage all changes
git commit -m "feat: message"   # Commit with message
git commit --amend              # Amend last commit

# Syncing
git pull upstream main          # Pull from upstream
git push origin feature/name    # Push to origin
git fetch --all                 # Fetch all remotes

# Stashing
git stash                       # Stash changes
git stash pop                   # Apply stashed changes
git stash list                  # List stashes
```

---

## Configuration

### Elixir Config

**config/dev.exs:**
```elixir
import Config

config :o_supervisor,
  gerbil_executable: "/usr/local/bin/gerbil",
  gerbil_main_script: "../gerbil/main.ss",
  checkpoint_dir: "data/checkpoints",
  wal_dir: "data/wal",
  heartbeat_interval: 1000,      # 1 second
  heartbeat_timeout: 5000,       # 5 seconds
  max_concurrent_shadows: 5,
  shadow_test_duration: 300_000, # 5 minutes
  evolution_enabled: true

config :logger, level: :debug
```

**config/prod.exs:**
```elixir
import Config

config :o_supervisor,
  evolution_enabled: true,
  max_concurrent_shadows: 20

config :logger, level: :info
```

### Environment Variables

```bash
# Gerbil
export GERBIL_HOME=/usr/local/gerbil
export PATH=$GERBIL_HOME/bin:$PATH

# Elixir
export MIX_ENV=prod
export PORT=4000

# Database
export DATABASE_URL=postgresql://user:pass@localhost/o_db
export REDIS_URL=redis://localhost:6379
```

### Docker Environment

**.env file:**
```bash
# Application
MIX_ENV=prod
PORT=4000

# Database
POSTGRES_USER=o_user
POSTGRES_PASSWORD=o_password
POSTGRES_DB=o_database

# Redis
REDIS_PASSWORD=redis_password

# Monitoring
GRAFANA_ADMIN_PASSWORD=admin
```

---

## Message Types

### Gerbil → Elixir

```elixir
# Heartbeat
%{"type" => "heartbeat", "timestamp" => 1705392000}

# Checkpoint
%{
  "type" => "checkpoint",
  "timestamp" => 1705392000,
  "data" => %{"agent_state" => %{}, "memory_blocks" => []}
}

# WAL Entry
%{
  "type" => "wal_entry",
  "timestamp" => 1705392000,
  "data" => %{"operation" => "memory_add", "params" => %{}}
}

# Evolution Intent
%{
  "type" => "evolution_intent",
  "timestamp" => 1705392000,
  "hypothesis" => %{"reason" => "...", "new_code" => "..."}
}

# Metrics
%{
  "type" => "metrics",
  "timestamp" => 1705392000,
  "data" => %{"avg_latency" => 10.5, "error_rate" => 0.01}
}
```

### Elixir → Gerbil

```elixir
# Checkpoint Ack
%{
  "type" => "checkpoint_ack",
  "timestamp" => 1705392000,
  "checkpoint_id" => "uuid"
}

# Restore
%{
  "type" => "restore",
  "timestamp" => 1705392000,
  "checkpoint_id" => "uuid",
  "snapshot" => %{},
  "wal_entries" => []
}

# Hot Reload
%{
  "type" => "hot_reload",
  "timestamp" => 1705392000,
  "code" => "...",
  "module" => "memory"
}

# Evolution Result
%{
  "type" => "evolution_result",
  "timestamp" => 1705392000,
  "success" => true,
  "decision" => "promoted"
}
```

---

## Code Patterns

### Elixir GenServer

```elixir
defmodule MyModule do
  use GenServer
  require Logger

  # Client API
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def do_something(arg) do
    GenServer.call(__MODULE__, {:do_something, arg})
  end

  # Server Callbacks
  @impl true
  def init(opts) do
    {:ok, %{state: opts}}
  end

  @impl true
  def handle_call({:do_something, arg}, _from, state) do
    result = process(arg)
    {:reply, result, state}
  end

  @impl true
  def handle_cast({:async_operation, arg}, state) do
    process_async(arg)
    {:noreply, state}
  end

  @impl true
  def handle_info(:timeout, state) do
    {:noreply, state}
  end

  # Private Functions
  defp process(arg), do: arg
  defp process_async(arg), do: spawn(fn -> process(arg) end)
end
```

### Gerbil Module

```scheme
;;; my-module.ss - Module description

(export #t
  public-function-1
  public-function-2)

(import :std/format
        :std/sugar
        :agent/elixir-bridge)

;;; Public API

(def (public-function-1 arg1 arg2)
  "Function documentation."
  (let ((result (helper-function arg1)))
    (process-result result arg2)))

(def (public-function-2 arg)
  "Another function."
  (elixir-send "message_type" (hash 'data arg)))

;;; Private helpers

(def (helper-function arg)
  (+ arg 1))

(def (process-result result arg)
  (* result arg))
```

### Elixir Test

```elixir
defmodule MyModuleTest do
  use ExUnit.Case
  
  setup do
    {:ok, pid} = MyModule.start_link([])
    on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)
    {:ok, module: pid}
  end

  test "does something", %{module: _pid} do
    result = MyModule.do_something("input")
    assert result == :expected
  end

  test "handles errors" do
    assert {:error, :invalid} = MyModule.do_something("bad")
  end
end
```

### Checkpoint Creation

**Gerbil:**
```scheme
(import :agent/elixir-bridge)

;; Create checkpoint
(def (save-agent-state! agent)
  (let ((checkpoint-id (elixir-checkpoint! agent)))
    (displayln "Checkpoint created: " checkpoint-id)
    checkpoint-id))
```

**Elixir:**
```elixir
# Save checkpoint
OSupervisor.MemoryVault.save_checkpoint(
  checkpoint_id,
  snapshot,
  wal_entries
)

# Restore checkpoint
{:ok, data} = OSupervisor.MemoryVault.restore_checkpoint(checkpoint_id)
```

### WAL Logging

**Gerbil:**
```scheme
(import :agent/elixir-bridge)

;; Log operation
(def (memory-add! memory content)
  ;; Log to WAL first
  (elixir-wal-log! 'memory-add (hash 'content content))
  
  ;; Then execute
  (add-to-memory! memory content))
```

**Elixir:**
```elixir
# Append WAL entry
OSupervisor.WALManager.append_entry(%{
  "operation" => "memory_add",
  "params" => %{"content" => "..."}
})

# Replay WAL
{:ok, entries} = OSupervisor.WALManager.replay_from_checkpoint(checkpoint_id)
```

---

## Troubleshooting

### Common Errors

#### Port Already in Use

```bash
# Find process
lsof -i :4000

# Kill process
kill -9 <PID>

# Or change port
PORT=4001 iex -S mix
```

#### DETS Corruption

```bash
# Remove corrupted file
rm data/checkpoints/checkpoints.dets

# Restart
iex -S mix
```

#### Dependency Issues

```bash
# Clean and reinstall
mix deps.clean --all
mix deps.get
mix compile
```

#### Gerbil Not Found

```bash
# Check installation
which gerbil

# Set path
export GERBIL_HOME=/usr/local/gerbil
export PATH=$GERBIL_HOME/bin:$PATH

# Or update config
vim config/dev.exs
# Set: gerbil_executable: "/full/path/to/gerbil"
```

### Debug Commands

**Elixir:**
```elixir
# In IEx
:observer.start()                           # GUI observer
:sys.get_state(OSupervisor.MemoryVault)    # Get state
:sys.trace(OSupervisor.GerbilManager, true) # Trace messages
Process.list() |> length()                  # Process count
:recon.proc_count(:memory, 10)              # Top memory users
```

**Gerbil:**
```scheme
;; Debug prints
(displayln "Debug: " variable)

;; Inspect value
(pretty-print value)

;; Trace
(import :std/debug/trace)
(trace-call function args)
```

### Performance Profiling

**Elixir:**
```elixir
# fprof
:fprof.trace([:start])
# Run operation
:fprof.trace([:stop])
:fprof.profile()
:fprof.analyse()

# eprof
:eprof.start()
:eprof.profile(fn -> MyModule.operation() end)
:eprof.analyze()
```

### Log Locations

```bash
# Development
data/logs/o_supervisor.log

# Docker
docker-compose logs -f o_supervisor

# Production
/var/log/o_supervisor/
```

---

## Useful Snippets

### Check System Status

```elixir
# In IEx
OSupervisor.status()
OSupervisor.list_checkpoints()
OSupervisor.list_shadows()
```

### Manual Checkpoint

```elixir
# Create checkpoint
checkpoint_id = UUID.uuid4()
OSupervisor.MemoryVault.save_checkpoint(
  checkpoint_id,
  %{"state" => "..."},
  []
)
```

### Start Shadow Test

```elixir
hypothesis = %{
  "reason" => "Test optimization",
  "new_code" => "..."
}

OSupervisor.EvolutionArbiter.start_shadow_test(
  hypothesis,
  "checkpoint_id"
)
```

### View Metrics

```elixir
{:ok, metrics, _timestamp} = 
  OSupervisor.HealthMonitor.get_metrics(:main_gerbil)

IO.inspect(metrics)
```

### Compare Instances

```elixir
{:ok, comparison} = 
  OSupervisor.HealthMonitor.compare_instances(
    :main_gerbil,
    :shadow_instance
  )

IO.inspect(comparison)
```

---

## Keyboard Shortcuts

### IEx

```
Ctrl+C Ctrl+C  - Exit IEx
Ctrl+C         - Show menu
Ctrl+G         - User switch command
h()            - Help
i(term)        - Info about term
v(n)           - Get value from history
recompile()    - Recompile project
```

### Observer

```
Ctrl+R         - Refresh
Ctrl+F         - Find
Ctrl+Q         - Quit
```

---

## Links

- [Full Documentation](../README.md)
- [Architecture](ARCHITECTURE_V2.md)
- [Getting Started](../GETTING_STARTED.md)
- [FAQ](FAQ.md)
- [Contributing](../CONTRIBUTING.md)

---

**Last Updated**: 2026-01-16  
**Version**: 1.0
