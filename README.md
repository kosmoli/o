# O - Stateful AI Agent Platform in Gerbil Scheme

**Project O** is a **memos-compatible AI agent platform** built in Gerbil Scheme with Elixir/OTP supervision. It provides stateful agents with long-term memory, multi-provider LLM support, and custom tool execution.

**Based on**: [memos](https://github.com/cpacker/memos) (Letta fork with unified provider system)

---

## 🎯 Key Features

- **Stateful Memory**: Core memory blocks, archival memory, conversation history
- **Multi-Provider LLM**: OpenAI, Anthropic, Groq, Ollama, and more
- **Custom Tools**: User-defined functions with sandbox execution
- **Fault Tolerance**: Automatic crash recovery via Elixir/OTP supervision
- **REST API**: Full-featured API for agent management
- **PostgreSQL**: Persistent storage with pgvector for semantic search

---

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────┐
│              Elixir Supervision Layer                   │
│  • Fault tolerance & crash recovery                     │
│  • State persistence (checkpoints + WAL)                │
│  • Shadow testing for safe evolution                    │
│  • Database operations (PostgreSQL)                     │
└─────────────────────────────────────────────────────────┘
                        ↕ MessagePack
┌─────────────────────────────────────────────────────────┐
│              Gerbil Agent Application                   │
│                                                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐ │
│  │   Agent      │  │   Memory     │  │   Tools      │ │
│  │   Core       │  │   System     │  │   System     │ │
│  └──────────────┘  └──────────────┘  └──────────────┘ │
│                                                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐ │
│  │   LLM        │  │   Message    │  │   HTTP       │ │
│  │   Clients    │  │   Manager    │  │   Server     │ │
│  └──────────────┘  └──────────────┘  └──────────────┘ │
└─────────────────────────────────────────────────────────┘
```

### Technology Stack

| Layer | Technology | Responsibility | Status |
|-------|-----------|----------------|--------|
| **Supervision** | **Elixir/OTP** | Fault tolerance, persistence | ✅ Done |
| **Agent Core** | **Gerbil Scheme** | Agent logic, DSL, lifecycle | ✅ Done |
| **LLM Clients** | **Gerbil Scheme** | OpenAI, Anthropic, Groq, etc. | ⏳ Todo |
| **HTTP Server** | **Gerbil Scheme** | REST API endpoints | ⏳ Todo |
| **Memory System** | **Gerbil Scheme** | Blocks, archival, search | 🟡 Partial |
| **Tool System** | **Gerbil Scheme** | Tool execution, sandbox | 🟡 Partial |
| **Database** | **PostgreSQL + Elixir** | Persistent storage | 🟡 Partial |
| **Vector Search** | **pgvector (optional)** | Semantic search | ⏳ Todo |

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

# PostgreSQL (optional, for local development)
brew install postgresql@16
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

### Getting Started
- **[README.md](README.md)** - This file (project overview)
- **[QUICKSTART.md](QUICKSTART.md)** - Quick start guide
- **[GETTING_STARTED.md](GETTING_STARTED.md)** - Detailed setup instructions

### Architecture & Design
- **[Memos Alignment Analysis](docs/MEMOS_ALIGNMENT_ANALYSIS.md)** - 🔥 **NEW**: Alignment with memos
- **[Revised Roadmap](docs/REVISED_ROADMAP.md)** - 🔥 **NEW**: 20-week implementation plan
- **[Architecture V2](docs/ARCHITECTURE_V2.md)** - Complete architecture overview
- **[Elixir Integration Guide](docs/ELIXIR_INTEGRATION.md)** - Detailed implementation guide

### Implementation Status
- **[Phase 0 Completion](COMPLETION_SUMMARY.md)** - Elixir foundation ✅
- **[Phase 1 Completion](docs/PHASE_1_COMPLETION.md)** - Gerbil agent core ✅
- **[Implementation Summary](IMPLEMENTATION_SUMMARY.md)** - Complete status

### Reference
- **[FAQ](docs/FAQ.md)** - Frequently asked questions
- **[Quick Reference](docs/QUICK_REFERENCE.md)** - Command reference
- **[Glossary](docs/GLOSSARY.md)** - Terminology
- **[ADRs](docs/adr/)** - Architecture Decision Records

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

### Phase 0: Elixir Foundation ✅ (Complete)
- [x] Elixir supervision layer (8 modules)
- [x] Fault tolerance & crash recovery
- [x] Checkpoint + WAL persistence
- [x] Shadow testing infrastructure
- [x] Docker deployment + CI/CD

### Phase 1: Gerbil Agent Core ✅ (Complete)
- [x] Agent lifecycle management
- [x] DSL (defagent, deftool, when->)
- [x] State management with context
- [x] Basic memory system
- [x] Tool framework
- [x] Integration tests (28+ cases)
- [x] Example agents (5 demos)

### Phase 2: LLM Integration & HTTP Server (Weeks 1-4) 🔥 **Current**
- [ ] OpenAI & Anthropic clients
- [ ] Groq & Ollama clients
- [ ] Unified LLM client interface
- [ ] HTTP server with routing
- [ ] REST API endpoints (agents, messages)

### Phase 3: Database & Message System (Weeks 5-8)
- [ ] PostgreSQL schema (memos-compatible)
- [ ] Gerbil-Elixir database protocol
- [ ] Message persistence & retrieval
- [ ] Message streaming (SSE)

### Phase 4: Advanced Memory System (Weeks 9-12)
- [ ] Memory blocks (persona, human, custom)
- [ ] Core memory operations
- [ ] Archival memory with search
- [ ] Semantic search (pgvector)

### Phase 5: Tool System Enhancement (Weeks 13-16)
- [ ] Core tools (send_message, search, etc.)
- [ ] Memory tools (append, replace, patch)
- [ ] Tool execution sandbox
- [ ] Tool rules & approval workflow

### Phase 6: Agent Execution Loop (Weeks 17-20)
- [ ] Step-based execution
- [ ] LLM inference with tool calls
- [ ] Context window management
- [ ] Streaming execution
- [ ] Performance optimization

**See [Revised Roadmap](docs/REVISED_ROADMAP.md) for detailed week-by-week plan.**

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

**Status**: Phase 0 & 1 Complete ✅, Phase 2 Starting 🔥
**Version**: 0.2.0
**Last Updated**: 2026-01-16

---

## 🎉 Recent Updates

### 🔥 Project Realignment (2026-01-16)

**Major Update**: Project O is now aligned with [memos](https://github.com/cpacker/memos) functionality!

**Key Changes**:
- 🎯 **Goal**: Build memos-compatible agent in Gerbil Scheme
- 🔧 **Strategy**: Gerbil-first (HTTP, LLM clients, database in Gerbil)
- 📋 **Roadmap**: New 20-week plan with weekly milestones
- 📚 **Resources**: Leveraging gerbil_scheme_book examples

**What This Means**:
- ✅ Phase 0 & 1 work remains valid (solid foundation)
- 🔄 Phase 2+ refocused on memos functionality
- 🚀 Prioritizing practical agent features over generic evolution
- 📖 Clear path forward with available Gerbil examples

See:
- **[Memos Alignment Analysis](docs/MEMOS_ALIGNMENT_ANALYSIS.md)** - Detailed analysis
- **[Revised Roadmap](docs/REVISED_ROADMAP.md)** - 20-week implementation plan

### Phase 1 Complete! (2026-01-16)

Phase 1 (Gerbil Agent Core) completed with:
- ✅ 6 core modules (~3,650 lines)
- ✅ 7 test suites (28+ test cases)
- ✅ 5 example agents
- ✅ Complete documentation

See **[Phase 1 Completion Report](docs/PHASE_1_COMPLETION.md)** for details.
