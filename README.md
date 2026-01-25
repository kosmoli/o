# O - Self-Evolving AI Agent with Stateful Memory

**Project O** is a **self-evolving AI agent** that uses [Letta](https://github.com/letta-ai/letta)'s memory architecture to achieve true autonomous evolution. Built in **Racket** with Elixir/OTP supervision, it can autonomously improve itself while maintaining memory of its evolution history.

**Core Innovation**: Leverages Letta's stateful memory model and extends it with autonomous self-evolution capabilities. Note: [memos](https://github.com/cpacker/memos) is a fork of Letta.

---

## 🎯 Key Features

### Self-Evolution Capabilities
- **Autonomous Evolution**: Agent can modify its own code during sleep-time compute
- **Stateful Memory**: Remembers evolution history and reflects on changes
- **Shadow Testing**: Safe evolution experiments via Elixir supervision
- **Evolution Tools**: Self-modification, performance analysis, rollback

### Letta-Based Memory System
- **Core Memory Blocks**: Stable identity with editable persona/human memory
- **Archival Memory**: Long-term storage with semantic search for evolution history
- **Autonomous Compute**: Self-directed thinking and evolution without human intervention
- **Active Memory Management**: Agent can modify its own knowledge base

### Infrastructure
- **Multi-Provider LLM**: OpenAI, Anthropic, Groq, Ollama, and more
- **Custom Tools**: User-defined functions with sandbox execution
- **Fault Tolerance**: Automatic crash recovery via Elixir/OTP supervision
- **REST API**: Full-featured API for agent management
- **PostgreSQL**: Persistent storage with pgvector for semantic search

---

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────┐
│              ELIXIR SUPERVISION LAYER                   │
│  • Fault tolerance & crash recovery                     │
│  • State persistence (checkpoints + WAL)                │
│  • Shadow testing for safe evolution                    │
│  • Database operations (PostgreSQL)                     │
└─────────────────────────────────────────────────────────┘
                        ↕ MessagePack
┌─────────────────────────────────────────────────────────┐
│               RACKET AGENT APPLICATION                  │
│                                                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐ │
│  │   Agent      │  │   Memory     │  │   Tools      │ │
│  │   Core       │  │   System     │  │   System     │ │
│  └──────────────┘  └──────────────┘  └──────────────┘ │
│                                                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐ │
│  │   LLM        │  │   Message    │  │   HTTP       │ │
│  │   Clients    │  │   Manager    │   │   Server     │ │
│  └──────────────┘  └──────────────┘  └──────────────┘ │
└─────────────────────────────────────────────────────────┘
```

### Technology Stack

| Layer | Technology | Responsibility | Status |
|-------|-----------|----------------|--------|
| **Supervision** | **Elixir/OTP** | Fault tolerance, persistence | ✅ Done |
| **Agent Core** | **Racket** | Agent logic, DSL, lifecycle | ✅ Done |
| **LLM Clients** | **Racket** | OpenAI, Anthropic, Groq, Ollama | ✅ Done |
| **HTTP Server** | **Racket** | REST API endpoints | ✅ Done |
| **Memory System** | **Racket** | Blocks, archival, semantic search | ✅ Done |
| **Tool System** | **Racket** | Tool execution, sandbox, rules | ✅ Done |
| **Database** | **PostgreSQL + Elixir** | Persistent storage | ✅ Done |
| **Vector Search** | **pgvector** | Semantic search | ✅ Done |
| **Agent Execution** | **Racket** | Step-based execution, streaming | ✅ Done |

---

## 🚀 Quick Start

### Prerequisites

```bash
# Elixir & Erlang (27+)
# Via asdf:
asdf plugin-add erlang
asdf plugin-add elixir
asdf install erlang 27.2.1
asdf install elixir 1.18.2-otp-27

# Racket
# Download from: https://racket-lang.org/

# PostgreSQL (optional, for local development)
brew install postgresql@16  # macOS
# or
apt-get install postgresql-16  # Ubuntu
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
```

### Running the System

```bash
# Start the Elixir supervisor (will start Racket agent)
cd o_supervisor
iex -S mix

# Or run Racket agent directly (for development)
cd racket/o
racket main.rkt
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
- **[Architecture V2](docs/ARCHITECTURE_V2.md)** - Complete architecture overview
- **[Elixir Integration Guide](docs/ELIXIR_INTEGRATION.md)** - Detailed implementation guide

### Implementation Status
- **[Phase 0 Completion](COMPLETION_SUMMARY.md)** - Elixir foundation ✅
- **[Phase 1 Completion](docs/PHASE_1_COMPLETION.md)** - Racket agent core ✅
- **[Implementation Summary](IMPLEMENTATION_SUMMARY.md)** - Complete status

### Reference
- **[FAQ](docs/FAQ.md)** - Frequently asked questions
- **[Glossary](docs/GLOSSARY.md)** - Terminology
- **[ADRs](docs/adr/)** - Architecture Decision Records

---

## 🧪 Testing

```bash
# Elixir tests
cd o_supervisor
mix test

# Racket tests
cd racket
racket o/test/tests.rkt

# Run specific test
mix test test/memory_vault_test.exs

# Run with coverage
mix test --cover
```

---

## 📊 Project Structure

```
o/
├── README.md                          # This file
├── docs/                              # Documentation
│   ├── ARCHITECTURE_V2.md             # Architecture overview
│   ├── ELIXIR_INTEGRATION.md          # Integration guide
│   └── adr/                           # Architecture Decision Records
├── o_supervisor/                      # Elixir supervision layer
│   ├── mix.exs                        # Elixir project config
│   ├── config/                        # Configuration files
│   ├── lib/                           # Elixir source code
│   │   └── o_supervisor/
│   │       ├── application.ex         # Application supervisor
│   │       ├── racket_manager.ex     # Racket process manager
│   │       ├── memory_vault.ex      # State persistence
│   │       ├── wal_manager.ex         # Write-Ahead Log
│   │       ├── health_monitor.ex      # Metrics collection
│   │       ├── evolution_arbiter.ex   # Shadow testing
│   │       ├── traffic_splitter.ex    # A/B testing
│   │       └── telemetry.ex           # Telemetry setup
│   └── test/                          # Tests
├── racket/                            # Racket agent code
│   └── o/
│       ├── agent/                      # Agent core
│       │   ├── benchmark.rkt
│       │   ├── context.rkt
│       │   ├── dsl.rkt
│       │   ├── executor.rkt
│       │   ├── state.rkt
│       │   ├── streaming.rkt
│       │   ├── tools.rkt
│       │   └── types.rkt
│       ├── database/                   # Database client
│       │   ├── client.rkt
│       │   └── msgpack.rkt
│       ├── llm/                        # LLM clients
│       │   ├── anthropic.rkt
│       │   ├── client.rkt
│       │   ├── openai.rkt
│       │   ├── stream.rkt
│       │   └── types.rkt
│       ├── message/                    # Message system
│       │   ├── handler.rkt
│       │   ├── manager.rkt
│       │   ├── queue.rkt
│       │   └── stream.rkt
│       ├── memory/                     # Memory system
│       │   ├── archival.rkt
│       │   ├── blocks.rkt
│       │   ├── core.rkt
│       │   ├── semantic.rkt
│       │   └── types.rkt
│       ├── server/                     # HTTP server
│       │   ├── http.rkt
│       │   └── web.rkt
│       ├── tools/                      # Tool system
│       │   ├── core.rkt
│       │   ├── rules.rkt
│       │   ├── sandbox.rkt
│       │   ├── types.rkt
│       │   └── utils.rkt
│       ├── test/                       # Racket tests
│       │   ├── tests.rkt
│       │   └── simple-test.rkt
│       ├── elixir-bridge.rkt           # Elixir communication
│       └── main.rkt                    # Entry point
├── database/                           # Database schema
│   ├── schema.sql                     # PostgreSQL schema
│   └── migrations/                    # Database migrations
└── docker-compose.yml                 # Docker deployment
```

---

## 🔄 Migration History

### Gerbil → Racket Migration (2026-01-25)

**Why Racket?**
- Better documentation and learning resources
- More active community and ecosystem
- Built-in package manager (`raco`)
- Excellent metaprogramming support
- Racket AI Book best practices

**What Changed:**
- ✅ All 34 Gerbil Scheme files migrated to Racket
- ✅ Elixir supervision layer updated to use RacketManager
- ✅ Communication protocol adapted (MessagePack maintained)
- ✅ All 41 tests passing (29 Racket + 12 Elixir)

**Migration Details:**
| Gerbil Component | Racket Equivalent | Lines | Status |
|------------------|-------------------|-------|--------|
| agent/core.ss | agent/*.rkt | ~2,500 | ✅ |
| agent/dsl.ss | agent/dsl.rkt | ~500 | ✅ |
| agent/state.ss | agent/state.rkt | ~520 | ✅ |
| agent/tools.ss | agent/tools.rkt | ~550 | ✅ |
| llm/*.ss | llm/*.rkt | ~1,200 | ✅ |
| memory/*.ss | memory/*.rkt | ~1,800 | ✅ |
| database/client.ss | database/client.rkt | ~270 + msgpack | ✅ |

---

## 🎯 Roadmap

### Phase 0: Elixir Foundation ✅ (Complete)
- [x] Elixir supervision layer (8 modules)
- [x] Fault tolerance & crash recovery
- [x] Checkpoint + WAL persistence
- [x] Shadow testing infrastructure
- [x] Docker deployment + CI/CD

### Phase 1: Racket Agent Core ✅ (Complete)
- [x] Agent lifecycle management
- [x] DSL (defagent, deftool, when->)
- [x] State management with context
- [x] Basic memory system
- [x] Tool framework
- [x] Integration tests (41+ cases)

### Phase 2: LLM Integration & HTTP Server ✅ (Complete)
- [x] OpenAI & Anthropic clients
- [x] Groq & Ollama clients
- [x] Unified LLM client interface
- [x] HTTP server with routing
- [x] REST API endpoints (agents, messages)

### Phase 3: Database & Message System ✅ (Complete)
- [x] PostgreSQL schema (Letta-compatible)
- [x] Racket-Elixir database protocol
- [x] Message persistence & retrieval
- [x] Message streaming (SSE)

### Phase 4: Advanced Memory System ✅ (Complete)
- [x] Memory blocks (persona, human, custom)
- [x] Core memory operations
- [x] Archival memory with search
- [x] Semantic search (pgvector)

### Phase 5: Tool System Enhancement ✅ (Complete)
- [x] Core tools (send_message, search, etc.)
- [x] Memory tools (append, replace, patch)
- [x] Tool execution sandbox
- [x] Tool rules & approval workflow

### Phase 6: Agent Execution Loop ✅ (Complete)
- [x] Step-based execution
- [x] LLM inference with tool calls
- [x] Context window management
- [x] Streaming execution
- [x] Performance optimization

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
- **Racket**: Follow [Racket Style Guide](https://racket-lang.org/style/)
- Use `raco fmt` for formatting

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
- **Racket**: For powerful metaprogramming capabilities and excellent documentation
- **Letta**: For the stateful memory architecture that inspires us

---

## 📞 Contact

- **Issues**: [GitHub Issues](https://github.com/your-repo/o/issues)
- **Discussions**: [GitHub Discussions](https://github.com/your-repo/o/discussions)

---

**Status**: All Phases Complete ✅ (Phases 0-6)
**Version**: 2.0.0 (Racket Edition)
**Last Updated**: 2026-01-25

---

## 🎉 Recent Updates

### 🎊 Gerbil → Racket Migration Complete! (2026-01-25)

**Project O has successfully migrated from Gerbil Scheme to Racket!**

**What Changed:**
- ✅ All 34 Gerbil files migrated to Racket (following Racket AI Book best practices)
- ✅ Elixir supervision updated (GerbilManager → RacketManager)
- ✅ MessagePack communication protocol maintained
- ✅ All 41 tests passing (29 Racket + 12 Elixir)

**Why Racket?**
- Better documentation and learning resources (Racket AI Book)
- More active community and ecosystem
- Built-in package manager (`raco`)
- Excellent metaprogramming support

**Key Improvements:**
- 📚 Better inline documentation practices
- 🔧 More maintainable code structure
- 🛠️ Built-in testing framework (rackunit)
- 📦 Superior package management

See **[IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md)** for complete details.

### 🔥 Strategic Vision (2026-01-25)

**Core Mission**: Project O aims to achieve **autonomous self-evolution** through stateful memory!

**Why Letta's Architecture?**
- 🧠 **Stateful Memory**: Agent remembers its evolution history and can reflect
- 💤 **Autonomous Compute**: Agent can evolve independently without human intervention
- 🔄 **Active Memory Management**: Agent can modify its own knowledge base
- 📚 **Archival Memory**: Agent learns from past evolution attempts

**Our Goal - Evolution, Not Imitation**:
- 🎯 **Beyond Letta**: We're not copying Letta - we're building on its foundation
- 🧬 **Autonomous Evolution**: Agent can modify its own code and improve itself
- 🔬 **Safe Experimentation**: Elixir supervision enables risk-free evolution testing
- 📈 **Continuous Improvement**: Agent learns from each evolution attempt

**Implementation Foundation**:
- ✅ Phases 0-1: Fault tolerance foundation (enables safe evolution)
- ✅ Phases 2-3: Letta-compatible infrastructure (memory, tools, API)
- ✅ Phase 4: Advanced memory system (semantic search, archival)
- ✅ Phase 5: Tool system (self-modification capabilities)
- ✅ Phase 6: Agent execution loop (autonomous operation)

**Note**: [memos](https://github.com/cpacker/memos) is a fork of [Letta](https://github.com/letta-ai/letta). We build on Letta's architecture.
