# O - Self-Evolving Agent System V2

**Project O V2** is a self-evolving AI Agent system with industrial-grade fault tolerance. Building upon the original Gerbil-based architecture, V2 introduces an Elixir/OTP supervision layer that prevents catastrophic self-destruction during evolution.

---

## 🎯 Key Features

- **Self-Evolution**: Agent can modify its own code at runtime
- **Fault Tolerance**: Automatic crash recovery with state preservation
- **Shadow Testing**: Safe evolution testing in isolated instances
- **Multi-Threaded Evolution**: Parallel evolution experiments with genetic algorithms
- **Zero Data Loss**: Checkpoints + WAL ensure durability
- **Hot Reload**: Update code without downtime

---

## 🏗️ Architecture

```
┌──────────────────────────────────────────────────────────┐
│  Elixir/OTP Supervisor (The Immortal Guardian)           │
│  • Monitors Gerbil process heartbeat                     │
│  • Holds memory snapshots in ETS/DETS                   │
│  • Manages shadow instances for testing                 │
│  • Restarts crashed processes with last known state     │
└──────────────────────────────────────────────────────────┘
                    ↕ Port Communication
┌──────────────────────────────────────────────────────────┐
│  Gerbil Agent (The Evolving Brain)                       │
│  • Runs main agent logic                                 │
│  • Generates and tests new code                         │
│  • Sends checkpoints before risky operations            │
│  • Can crash without consequences                       │
└──────────────────────────────────────────────────────────┘
                    ↓ FFI
┌──────────────────────────────────────────────────────────┐
│  Zig/Rust (The High-Performance Muscle)                  │
│  • HTTP, databases, vector operations                    │
│  • Stateless, cannot corrupt agent memory               │
└──────────────────────────────────────────────────────────┘
```

### Technology Stack

| Layer | Technology | Responsibility | Portion |
|-------|-----------|----------------|---------|
| **Supervision** | **Elixir/OTP** | **Process lifecycle, state persistence** | **10%** |
| **Meta** | Gerbil Scheme | Self-modification engine | 12% |
| **Application** | Gerbil Scheme | Agent logic, DSL, memory, tools | 53% |
| **Infrastructure** | Zig | HTTP, WebSocket, databases | 15% |
| **Compute** | Rust | Vector operations, ML inference | 8% |
| **Foundation** | C Libraries | PostgreSQL, SQLite, OpenSSL | 2% |

---

## 🚀 Quick Start

### Prerequisites

```bash
# Elixir & Erlang
brew install elixir  # macOS
# or
apt-get install elixir  # Ubuntu

# Gerbil Scheme
# Follow: https://cons.io/

# Zig
brew install zig

# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Installation

```bash
# Clone repository
git clone <repository-url>
cd o

# Install Elixir dependencies
cd o_supervisor
mix deps.get

# Compile
mix compile

# Run tests
mix test

# Start in development
iex -S mix
```

### Docker Deployment

```bash
# Build and start all services
docker-compose up -d

# View logs
docker-compose logs -f o_supervisor

# Stop services
docker-compose down
```

---

## 📚 Documentation

### Core Documentation
- **[Architecture V2](docs/ARCHITECTURE_V2.md)** - Complete architecture overview
- **[Elixir Integration Guide](docs/ELIXIR_INTEGRATION.md)** - Detailed implementation guide
- **[Implementation Checklist](docs/IMPLEMENTATION_CHECKLIST.md)** - Step-by-step implementation
- **[Message Schema](docs/protocol/MESSAGE_SCHEMA.md)** - Communication protocol

### Phase Completion Reports
- **[Phase 0 Completion](COMPLETION_SUMMARY.md)** - Elixir foundation complete
- **[Phase 1 Completion](docs/PHASE_1_COMPLETION.md)** - Gerbil agent core complete

### Architecture Decision Records
- **[ADR-001: Elixir Supervision](docs/adr/001-elixir-supervision-layer.md)** - Supervision layer
- **[ADR-002: Communication Protocol](docs/adr/002-communication-protocol.md)** - MessagePack over Port
- **[ADR-003: Checkpoint Strategy](docs/adr/003-checkpoint-strategy.md)** - Hybrid persistence

---

## 🧪 Testing

```bash
# Run all tests
cd o_supervisor
mix test

# Run specific test
mix test test/memory_vault_test.exs

# Run with coverage
mix test --cover

# Run integration tests
mix test --only integration

# Run stress tests
mix test --only stress
```

---

## 📊 Project Structure

```
o/
├── README.md                          # This file
├── docs/                              # Documentation
│   ├── ARCHITECTURE_V2.md             # Architecture overview
│   ├── ELIXIR_INTEGRATION.md          # Integration guide
│   ├── IMPLEMENTATION_CHECKLIST.md    # Implementation steps
│   ├── adr/                           # Architecture Decision Records
│   │   ├── 001-elixir-supervision-layer.md
│   │   ├── 002-communication-protocol.md
│   │   └── 003-checkpoint-strategy.md
│   └── protocol/                      # Protocol specifications
│       └── MESSAGE_SCHEMA.md
├── o_supervisor/                      # Elixir supervision layer
│   ├── mix.exs                        # Elixir project config
│   ├── config/                        # Configuration files
│   ├── lib/                           # Elixir source code
│   │   └── o_supervisor/
│   │       ├── application.ex         # Application supervisor
│   │       ├── gerbil_manager.ex      # Gerbil process manager
│   │       ├── memory_vault.ex        # State persistence
│   │       ├── wal_manager.ex         # Write-Ahead Log
│   │       ├── health_monitor.ex      # Metrics collection
│   │       ├── evolution_arbiter.ex   # Shadow testing
│   │       ├── traffic_splitter.ex    # A/B testing
│   │       └── telemetry.ex           # Telemetry setup
│   └── test/                          # Tests
├── gerbil/                            # Gerbil agent code
│   ├── agent/
│   │   ├── core.ss                    # Agent structure and lifecycle
│   │   ├── dsl.ss                     # DSL macros
│   │   ├── state.ss                   # State management
│   │   ├── memory.ss                  # Memory system
│   │   ├── tools.ss                   # Tool framework
│   │   └── elixir-bridge.ss           # Elixir communication bridge
│   ├── test/
│   │   └── integration-test.ss        # Integration tests
│   └── examples/
│       └── simple-agent.ss            # Example agents
├── zig/                               # Zig infrastructure modules
├── rust/                              # Rust compute modules
├── docker-compose.yml                 # Docker deployment
└── data/                              # Runtime data
    ├── checkpoints/                   # State checkpoints
    ├── wal/                           # Write-Ahead Logs
    └── logs/                          # Application logs
```

---

## 🎯 Roadmap

### Phase 0: Elixir Foundation ✅
- [x] Elixir project setup
- [x] Port communication
- [x] MemoryVault (DETS + file backup)
- [x] HealthMonitor (heartbeat + metrics)

### Phase 1: Gerbil Core ✅
- [x] Agent core with Elixir bridge
- [x] DSL and state management
- [x] Memory system with checkpoints
- [x] Tool framework
- [x] Integration tests
- [x] Example agents

### Phase 2: Infrastructure
- [ ] Zig HTTP client module
- [ ] Zig database modules
- [ ] Gerbil FFI layer
- [ ] Integration tests

### Phase 3: Protected Evolution
- [ ] Shadow testing infrastructure
- [ ] WAL system integration
- [ ] Checkpoint/recovery mechanism
- [ ] First protected evolution demo

### Phase 4: Multi-Threaded Evolution
- [ ] Parallel shadow spawning
- [ ] Genetic algorithm evolution
- [ ] Adversarial evolution (Red/Blue)
- [ ] Performance optimization

### Phase 5: Advanced Features
- [ ] Agent modifies its own DSL
- [ ] Agent generates new Zig modules
- [ ] Agent optimizes memory strategy
- [ ] Agent learns from feedback
- [ ] Agent discovers and fixes bugs

---

## 🤝 Contributing

Contributions are welcome! Please read our contributing guidelines before submitting PRs.

### Development Workflow

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Code Style

- **Elixir**: Follow [Elixir Style Guide](https://github.com/christopheradams/elixir_style_guide)
- **Gerbil**: Follow Scheme conventions
- **Zig**: Follow [Zig Style Guide](https://ziglang.org/documentation/master/#Style-Guide)
- **Rust**: Use `rustfmt`

---

## 📈 Performance

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| Request latency (p50) | < 15ms | TBD | ⏳ |
| Request latency (p99) | < 50ms | TBD | ⏳ |
| Throughput | > 5000 QPS | TBD | ⏳ |
| Crash recovery time | < 100ms | TBD | ⏳ |
| Memory per instance | < 150MB | TBD | ⏳ |

---

## 🔒 Security

- Input validation on all messages
- Sandboxed code execution
- Resource limits per shadow instance
- Encrypted data at rest and in transit

---

## 📝 License

[Specify your license here]

---

## 🙏 Acknowledgments

- **Elixir/OTP**: For the battle-tested supervision trees
- **Gerbil Scheme**: For powerful metaprogramming capabilities
- **Zig**: For fast, safe infrastructure code
- **Rust**: For high-performance compute operations

---

## 📞 Contact

- **Issues**: [GitHub Issues](https://github.com/your-repo/o/issues)
- **Discussions**: [GitHub Discussions](https://github.com/your-repo/o/discussions)

---

**Status**: Phase 0 & 1 Complete ✅, Phase 2 Ready
**Version**: 0.2.0
**Last Updated**: 2026-01-16

---

## 🎉 Recent Updates

### Phase 1 Complete! (2026-01-16)

Phase 1 (Gerbil Agent Core) has been successfully completed with:

- ✅ **6 core modules** (~2,350 lines of Gerbil Scheme)
  - `agent/core.ss` - Agent structure and lifecycle management
  - `agent/dsl.ss` - Declarative DSL with macros (defagent, deftool, when->)
  - `agent/state.ss` - State management with context tracking
  - `agent/memory.ss` - Memory system (short-term, long-term, episodic, semantic)
  - `agent/tools.ss` - Tool framework with registry and execution
  - `agent/elixir-bridge.ss` - Communication bridge to Elixir

- ✅ **Comprehensive testing** (~600 lines)
  - 7 test suites with 28+ test cases
  - Full integration tests for checkpoint/restore
  - All tests passing

- ✅ **Example agents** (~550 lines)
  - Echo agent (simple I/O)
  - Counter agent (with custom tools)
  - Memory agent (using memory system)
  - DSL agent (using macros)
  - Evolving agent (self-evolution triggers)

**Key Features Implemented**:
- Agent lifecycle management (initializing → running → evolving → suspended → terminated)
- Automatic checkpointing with Elixir integration
- Memory consolidation (short-term → long-term)
- Tool execution with caching and validation
- Declarative DSL for agent definition
- Complete serialization/deserialization

See **[Phase 1 Completion Report](docs/PHASE_1_COMPLETION.md)** for full details.
