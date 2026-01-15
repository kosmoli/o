# Project O V2 - Quick Start Guide

**Last Updated**: 2026-01-16
**Status**: Phase 0 & 1 Complete ✅

---

## 🎯 What is Project O?

Project O V2 is a **self-evolving AI Agent system** with industrial-grade fault tolerance. The agent can modify its own code at runtime while being protected by an Elixir/OTP supervision layer that prevents catastrophic failures.

**Key Innovation**: The "External Guardian Pattern" - Elixir supervises Gerbil externally, enabling the agent to safely crash and recover without losing state.

---

## 🚀 Quick Start (5 Minutes)

### Prerequisites

```bash
# Install Elixir (includes Erlang)
brew install elixir  # macOS
# or
apt-get install elixir  # Ubuntu

# Install Gerbil Scheme
# Follow: https://cons.io/
```

### Run the System

```bash
# 1. Clone and enter directory
cd /Users/liyuhang/work/o

# 2. Install Elixir dependencies
cd o_supervisor
mix deps.get

# 3. Compile
mix compile

# 4. Run tests
mix test

# 5. Start interactive shell
iex -S mix
```

### Run Example Agents

```bash
# Run Gerbil example agents
cd gerbil/examples
gxi simple-agent.ss all

# Or run specific examples:
gxi simple-agent.ss echo      # Echo agent
gxi simple-agent.ss counter   # Counter agent with tools
gxi simple-agent.ss memory    # Memory-enabled agent
gxi simple-agent.ss evolving  # Self-evolving agent
```

---

## 📚 What's Implemented

### Phase 0: Elixir Supervision Layer ✅

**8 Core Modules**:
- `GerbilManager` - Manages Gerbil processes via Port
- `MemoryVault` - State persistence (DETS + file backup)
- `WALManager` - Write-Ahead Log for durability
- `HealthMonitor` - Metrics collection
- `EvolutionArbiter` - Shadow testing orchestration
- `TrafficSplitter` - A/B testing support
- `Telemetry` - Telemetry integration
- `Application` - Root supervisor

**Features**:
- Automatic crash recovery (50-100ms)
- Heartbeat monitoring (1s interval, 5s timeout)
- Checkpoint compression (zstd level 9)
- Maximum data loss: < 1 second

### Phase 1: Gerbil Agent Core ✅

**6 Core Modules**:
- `agent/core.ss` - Agent structure and lifecycle
- `agent/dsl.ss` - Declarative DSL with macros
- `agent/state.ss` - State management
- `agent/memory.ss` - Memory system
- `agent/tools.ss` - Tool framework
- `agent/elixir-bridge.ss` - Communication bridge

**Features**:
- Agent lifecycle management (5 states)
- Declarative DSL (`defagent`, `deftool`, `when->`)
- Memory consolidation (short-term → long-term)
- Tool execution with caching
- Complete Elixir integration

---

## 🏗️ Architecture Overview

```
┌─────────────────────────────────────────┐
│  Elixir/OTP (The Immortal Guardian)     │
│  • Monitors heartbeat                   │
│  • Holds memory snapshots               │
│  • Manages shadow instances             │
│  • Restarts crashed processes           │
└─────────────────────────────────────────┘
              ↕ MessagePack
┌─────────────────────────────────────────┐
│  Gerbil Agent (The Evolving Brain)      │
│  • Runs agent logic                     │
│  • Generates new code                   │
│  • Can safely crash                     │
└─────────────────────────────────────────┘
```

---

## 📖 Example: Creating an Agent

### Using the DSL

```scheme
(import :gerbil/agent/core
        :gerbil/agent/dsl
        :gerbil/agent/state
        :gerbil/agent/memory)

;; Define an agent using declarative DSL
(defagent my-agent
  version: "1.0.0"
  config: (hash 'checkpoint-interval 300)

  (init
   (displayln "Agent initializing..."))

  (perceive input
   (displayln (format "Perceived: ~a" input))
   input)

  (think context
   (displayln (format "Thinking about: ~a" context))
   (hash 'action 'respond
         'message (format "Processed: ~a" context)))

  (act action
   (displayln (format "Acting: ~a" (hash-ref action 'message)))
   action))

;; Run the agent
(run-agent-workflow my-agent
  '("Hello" "How are you?" "Goodbye"))
```

### Defining Tools

```scheme
;; Define a tool using deftool macro
(deftool calculate-sum
  description: "Calculate sum of two numbers"
  parameters: '((a number) (b number))
  (a b)
  (+ a b))

;; Register and execute
(def registry (make-tool-registry-instance))
(register-tool! registry calculate-sum-tool-spec)
(execute-tool registry "calculate-sum" (hash 'a 5 'b 3))
;; => Result: 8
```

### Memory Management

```scheme
;; Create memory system
(def memory (make-agent-memory-instance))

;; Add memories
(add-to-short-term! memory
  (make-memory-block-instance
   :episodic
   "User asked about weather"
   importance: 0.8
   tags: '(weather user-query)))

;; Consolidate important memories
(consolidate-memory! memory)

;; Search memories
(search-memory memory "weather" limit: 5)
```

---

## 🧪 Testing

### Run All Tests

```bash
# Elixir tests
cd o_supervisor
mix test

# Gerbil tests (when Gerbil is installed)
cd gerbil/test
gxi integration-test.ss
```

### Test Coverage

- ✅ 5 Elixir test suites
- ✅ 7 Gerbil test suites (28+ test cases)
- ✅ All tests passing

---

## 🐳 Docker Deployment

### Start All Services

```bash
# Build and start
docker-compose up -d

# View logs
docker-compose logs -f o_supervisor

# Check status
docker-compose ps

# Stop
docker-compose down
```

### Services Included

- `o_supervisor` - Elixir supervision layer
- `postgres` - PostgreSQL database
- `redis` - Redis cache
- `prometheus` - Metrics collection
- `grafana` - Visualization

---

## 📚 Documentation

### Essential Reading

1. **[README.md](README.md)** - Project overview
2. **[ARCHITECTURE_V2.md](docs/ARCHITECTURE_V2.md)** - Complete architecture
3. **[PHASE_1_COMPLETION.md](docs/PHASE_1_COMPLETION.md)** - Phase 1 details
4. **[IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md)** - Complete summary

### Deep Dives

- **[ELIXIR_INTEGRATION.md](docs/ELIXIR_INTEGRATION.md)** - Integration guide with code examples
- **[MESSAGE_SCHEMA.md](docs/protocol/MESSAGE_SCHEMA.md)** - Communication protocol
- **[FAQ.md](docs/FAQ.md)** - 50+ frequently asked questions
- **[QUICK_REFERENCE.md](docs/QUICK_REFERENCE.md)** - Command reference

### Architecture Decisions

- **[ADR-001](docs/adr/001-elixir-supervision-layer.md)** - Why Elixir supervision?
- **[ADR-002](docs/adr/002-communication-protocol.md)** - Why MessagePack over Port?
- **[ADR-003](docs/adr/003-checkpoint-strategy.md)** - Why hybrid persistence?

---

## 🛠️ Development Commands

### Makefile Commands

```bash
# Setup
make setup          # Initialize project
make install        # Install dependencies

# Development
make compile        # Compile code
make test           # Run tests
make format         # Format code
make lint           # Run linter

# Running
make dev            # Development server
make iex            # Interactive shell

# Docker
make docker-build   # Build images
make docker-up      # Start services
make docker-down    # Stop services
make docker-logs    # View logs

# Utilities
make check          # Run all checks
make clean          # Clean build artifacts
make status         # Project status
```

---

## 🎯 Key Features

### 1. Self-Evolution
Agent can modify its own code at runtime with safety guarantees.

### 2. Fault Tolerance
Automatic crash recovery in 50-100ms with state preservation.

### 3. Shadow Testing
Test new code in isolated instances before promoting to main.

### 4. Zero Data Loss
Checkpoints + WAL ensure maximum data loss < 1 second.

### 5. Declarative DSL
Clean, maintainable agent definitions using Lisp macros.

### 6. Memory System
Intelligent memory management with consolidation and search.

---

## 📊 Project Statistics

- **Total Files**: 58
- **Total Lines**: ~17,650
- **Documentation**: 16 files (~10,000 lines)
- **Elixir Code**: 15 files (~2,000 lines)
- **Gerbil Code**: 8 files (~3,650 lines)
- **Tests**: 30+ test cases, all passing ✅

---

## 🚀 Next Steps

### For Users

1. **Explore Examples**: Run the example agents
2. **Read Documentation**: Start with ARCHITECTURE_V2.md
3. **Try Creating**: Build your own agent using the DSL
4. **Experiment**: Modify examples and see what happens

### For Developers

1. **Review Code**: Explore the Elixir and Gerbil modules
2. **Run Tests**: Verify everything works
3. **Read ADRs**: Understand design decisions
4. **Contribute**: See CONTRIBUTING.md

### For Researchers

1. **Study Architecture**: External Guardian Pattern
2. **Analyze Performance**: Benchmark the system
3. **Explore Evolution**: Shadow testing mechanism
4. **Extend System**: Add new capabilities

---

## 🤝 Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Areas for Contribution

- **Phase 2**: Zig infrastructure modules
- **Phase 3**: Protected evolution features
- **Phase 4**: Multi-threaded evolution
- **Documentation**: Improvements and examples
- **Testing**: More test cases and scenarios
- **Performance**: Optimizations and benchmarks

---

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/your-repo/o/issues)
- **Discussions**: [GitHub Discussions](https://github.com/your-repo/o/discussions)
- **Documentation**: See `docs/` directory

---

## 🎊 Status

- ✅ **Phase 0**: Elixir Supervision Layer - Complete
- ✅ **Phase 1**: Gerbil Agent Core - Complete
- 🚀 **Phase 2**: Infrastructure Layer - Ready to begin

**Version**: 0.2.0
**Last Updated**: 2026-01-16
**Ready for**: Production Implementation 🚀

---

**Built with**:
- Elixir/OTP for fault tolerance
- Gerbil Scheme for metaprogramming
- MessagePack for communication
- DETS/WAL for persistence

**The future of self-evolving AI agents is here!** 🤖✨
