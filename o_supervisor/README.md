# OSupervisor

Elixir/OTP supervision layer for the O self-evolving agent system.

## Overview

OSupervisor provides industrial-grade fault tolerance and lifecycle management for Gerbil-based self-evolving agents. It prevents catastrophic self-destruction during evolution by acting as an external guardian.

## Features

- **Process Supervision**: Automatic restart on crash with state recovery
- **State Persistence**: Checkpoints and Write-Ahead Logging (WAL)
- **Shadow Testing**: Safe evolution testing in isolated instances
- **Multi-Threaded Evolution**: Parallel evolution experiments with genetic algorithms
- **Health Monitoring**: Real-time metrics and performance tracking
- **Traffic Splitting**: A/B testing for shadow instances

## Installation

```bash
# Install dependencies
mix deps.get

# Compile
mix compile

# Run tests
mix test

# Start in development
iex -S mix

# Build release
MIX_ENV=prod mix release
```

## Configuration

Edit `config/config.exs`:

```elixir
config :o_supervisor,
  gerbil_executable: "gerbil",
  checkpoint_dir: "data/checkpoints",
  wal_dir: "data/wal",
  heartbeat_interval: 1000,
  heartbeat_timeout: 5000,
  max_concurrent_shadows: 10
```

## Usage

### Starting the Supervisor

```elixir
# Automatically starts with the application
{:ok, _} = Application.ensure_all_started(:o_supervisor)
```

### Creating Checkpoints

```elixir
# Checkpoints are created automatically by Gerbil
# Or manually trigger:
OSupervisor.MemoryVault.save_checkpoint(id, snapshot, wal_entries)
```

### Shadow Testing

```elixir
hypothesis = %{
  "reason" => "Optimize memory search",
  "new_code" => File.read!("optimized.ss")
}

OSupervisor.EvolutionArbiter.start_shadow_test(hypothesis, checkpoint_id)
```

### Monitoring

```elixir
# Get metrics
{:ok, metrics, _} = OSupervisor.HealthMonitor.get_metrics(:main_gerbil)

# Compare instances
{:ok, comparison} = OSupervisor.HealthMonitor.compare_instances(:main, :shadow)
```

## Architecture

```
OSupervisor.Application
├── OSupervisor.Telemetry
├── OSupervisor.MemoryVault (DETS persistence)
├── OSupervisor.WALManager (Write-Ahead Log)
├── OSupervisor.HealthMonitor (Metrics)
├── OSupervisor.EvolutionArbiter (Shadow testing)
├── OSupervisor.ShadowSupervisor (Dynamic supervisor)
├── OSupervisor.GerbilManager (Main instance)
└── OSupervisor.TrafficSplitter (A/B testing)
```

## Documentation

- [Architecture V2](../docs/ARCHITECTURE_V2.md)
- [Elixir Integration Guide](../docs/ELIXIR_INTEGRATION.md)
- [Implementation Checklist](../docs/IMPLEMENTATION_CHECKLIST.md)

## License

See LICENSE file in project root.
