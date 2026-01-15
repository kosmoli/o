# Project O - Revised Implementation Roadmap

**Date**: 2026-01-16
**Goal**: Build memos-compatible agent in Gerbil Scheme
**Strategy**: Gerbil-first, leverage existing resources, incremental delivery

---

## 🎯 Vision

**Build a stateful AI agent platform in Gerbil Scheme that replicates memos functionality**, with:
- Multiple LLM provider support (OpenAI, Anthropic, Groq, Ollama, etc.)
- Structured memory system (core, archival, recall)
- Custom tool execution
- REST API for agent management
- PostgreSQL persistence
- Fault tolerance via Elixir supervision

---

## 📊 Current Status

### ✅ Completed (Phase 0 & 1)

**Phase 0: Elixir Supervision Layer** (~2,000 lines)
- GerbilManager, MemoryVault, WALManager
- HealthMonitor, EvolutionArbiter, TrafficSplitter
- Docker deployment, CI/CD pipeline

**Phase 1: Gerbil Agent Core** (~3,650 lines)
- Agent lifecycle management
- DSL (defagent, deftool, when->)
- State management with context
- Basic memory system
- Tool framework
- Integration tests

**Total**: ~5,650 lines, 40% foundation complete

### ❌ Missing (Memos Functionality)

- LLM provider integration (0%)
- HTTP server & REST API (0%)
- Database persistence (20% - only checkpoints)
- Message management (10% - basic conversation)
- Advanced memory (30% - missing archival, blocks)
- Core tools (0%)
- Agent execution loop (20% - basic structure)

---

## 🗺️ Revised Roadmap (20 Weeks)

### Phase 2: LLM Integration & HTTP Server (Weeks 1-4)

#### Week 1: OpenAI & Anthropic Clients

**Goal**: Support OpenAI and Anthropic APIs

**Tasks**:
1. Create `gerbil/llm/` directory structure
2. Adapt OpenAI client from gerbil_scheme_book
3. Implement `llm/openai.ss`:
   - Chat completions
   - Streaming support
   - Tool call parsing
   - Error handling
4. Implement `llm/anthropic.ss`:
   - Messages API
   - Streaming support
   - Tool use parsing
5. Create `llm/types.ss` (shared types)
6. Add tests for both clients

**Deliverables**:
- `gerbil/llm/openai.ss` (~300 lines)
- `gerbil/llm/anthropic.ss` (~300 lines)
- `gerbil/llm/types.ss` (~100 lines)
- Tests (~200 lines)

**Dependencies**: None (uses gerbil_scheme_book examples)

#### Week 2: Additional LLM Providers

**Goal**: Support Groq, Ollama, and provider abstraction

**Tasks**:
1. Implement `llm/groq.ss` (OpenAI-compatible)
2. Implement `llm/ollama.ss` (local models)
3. Create `llm/client.ss` (unified interface):
   ```scheme
   (def (llm-chat-completion provider model messages tools)
     (case provider
       ((openai) (openai-chat-completion ...))
       ((anthropic) (anthropic-messages ...))
       ((groq) (groq-chat-completion ...))
       ((ollama) (ollama-generate ...))))
   ```
4. Add provider configuration management
5. Implement provider registry
6. Add integration tests

**Deliverables**:
- `gerbil/llm/groq.ss` (~200 lines)
- `gerbil/llm/ollama.ss` (~250 lines)
- `gerbil/llm/client.ss` (~400 lines)
- `gerbil/llm/config.ss` (~150 lines)
- Tests (~300 lines)

**Dependencies**: Week 1 completion

#### Week 3: HTTP Server Foundation

**Goal**: Basic HTTP server with routing

**Tasks**:
1. Research Gerbil HTTP server libraries:
   - `:std/net/httpd` (standard library)
   - Third-party options
2. Implement `server/http.ss`:
   - HTTP server setup
   - Request parsing
   - Response generation
   - JSON serialization
3. Implement `server/router.ss`:
   - Route registration
   - Path matching
   - Method handling
4. Create basic health check endpoint
5. Add request logging
6. Add error handling middleware

**Deliverables**:
- `gerbil/server/http.ss` (~400 lines)
- `gerbil/server/router.ss` (~300 lines)
- `gerbil/server/middleware.ss` (~200 lines)
- Tests (~200 lines)

**Dependencies**: None (uses standard library)

#### Week 4: REST API Endpoints

**Goal**: Core API endpoints for agent management

**Tasks**:
1. Implement `server/routes/agents.ss`:
   - `POST /v1/agents` - Create agent
   - `GET /v1/agents/:id` - Get agent
   - `PATCH /v1/agents/:id` - Update agent
   - `DELETE /v1/agents/:id` - Delete agent
   - `GET /v1/agents` - List agents
2. Implement `server/routes/messages.ss`:
   - `POST /v1/agents/:id/messages` - Send message
   - `GET /v1/agents/:id/messages` - Get messages
3. Add request validation
4. Add response serialization
5. Create API documentation
6. Add integration tests

**Deliverables**:
- `gerbil/server/routes/agents.ss` (~500 lines)
- `gerbil/server/routes/messages.ss` (~400 lines)
- `gerbil/server/validation.ss` (~200 lines)
- API documentation (~50 lines)
- Tests (~400 lines)

**Dependencies**: Week 3 completion

**Milestone**: Basic agent API working, can create agents and send messages

---

### Phase 3: Database & Message System (Weeks 5-8)

#### Week 5: Database Schema & Elixir Extension

**Goal**: PostgreSQL schema and Elixir database layer

**Tasks**:
1. Design database schema (memos-compatible):
   ```sql
   CREATE TABLE agents (
     id UUID PRIMARY KEY,
     name TEXT NOT NULL,
     created_at TIMESTAMP,
     ...
   );

   CREATE TABLE messages (
     id UUID PRIMARY KEY,
     agent_id UUID REFERENCES agents(id),
     role TEXT NOT NULL,
     content TEXT NOT NULL,
     created_at TIMESTAMP,
     ...
   );

   CREATE TABLE memory_blocks (
     id UUID PRIMARY KEY,
     agent_id UUID REFERENCES agents(id),
     label TEXT NOT NULL,
     value TEXT NOT NULL,
     is_template BOOLEAN,
     ...
   );
   ```
2. Create Alembic migrations
3. Extend Elixir `MemoryVault` for structured storage
4. Implement database operations in Elixir:
   - Agent CRUD
   - Message CRUD
   - Memory block CRUD
5. Add database connection pooling
6. Add transaction support

**Deliverables**:
- Database migrations (~300 lines SQL)
- Extended Elixir modules (~800 lines)
- Tests (~300 lines)

**Dependencies**: None (extends Phase 0)

#### Week 6: Gerbil Database Client

**Goal**: Gerbil-Elixir database protocol

**Tasks**:
1. Design message protocol for database operations:
   ```scheme
   ;; Gerbil -> Elixir
   (elixir-send "db_query"
     (hash 'operation 'insert
           'table 'agents
           'data (hash 'name "MyAgent" ...)))

   ;; Elixir -> Gerbil
   (hash 'type "db_result"
         'success #t
         'data (hash 'id "uuid" ...))
   ```
2. Implement `database/client.ss`:
   - Query execution
   - Result parsing
   - Error handling
   - Connection management
3. Implement `database/agents.ss` (agent operations)
4. Implement `database/messages.ss` (message operations)
5. Add caching layer
6. Add integration tests

**Deliverables**:
- `gerbil/database/client.ss` (~400 lines)
- `gerbil/database/agents.ss` (~300 lines)
- `gerbil/database/messages.ss` (~300 lines)
- Tests (~400 lines)

**Dependencies**: Week 5 completion

#### Week 7: Message Manager

**Goal**: Message persistence and retrieval

**Tasks**:
1. Implement `message/manager.ss`:
   - Message creation
   - Message retrieval
   - Message search (text-based)
   - Conversation history
   - Message pagination
2. Integrate with database client
3. Add message validation
4. Implement message filtering
5. Add message statistics
6. Create message utilities

**Deliverables**:
- `gerbil/message/manager.ss` (~600 lines)
- `gerbil/message/types.ss` (~200 lines)
- Tests (~300 lines)

**Dependencies**: Week 6 completion

#### Week 8: Message Streaming

**Goal**: Streaming message responses

**Tasks**:
1. Implement `message/stream.ss`:
   - Server-Sent Events (SSE)
   - Streaming response generation
   - Chunk buffering
   - Error handling in streams
2. Integrate with LLM streaming
3. Add stream lifecycle management
4. Implement stream cancellation
5. Add streaming tests

**Deliverables**:
- `gerbil/message/stream.ss` (~400 lines)
- Streaming integration (~200 lines)
- Tests (~200 lines)

**Dependencies**: Week 7 completion

**Milestone**: Full message system working with persistence and streaming

---

### Phase 4: Advanced Memory System (Weeks 9-12)

#### Week 9: Memory Blocks

**Goal**: Structured memory blocks (memos-compatible)

**Tasks**:
1. Implement `memory/blocks.ss`:
   - Memory block types (persona, human, custom)
   - Block CRUD operations
   - Read-only protection
   - Block templates
   - Block validation
2. Integrate with database
3. Add block versioning
4. Implement block search
5. Create block utilities

**Deliverables**:
- `gerbil/memory/blocks.ss` (~500 lines)
- Database integration (~200 lines)
- Tests (~300 lines)

**Dependencies**: Week 6 completion

#### Week 10: Core Memory Operations

**Goal**: Core memory manipulation tools

**Tasks**:
1. Implement `memory/core.ss`:
   - `core_memory_append` - Append to memory block
   - `core_memory_replace` - Replace memory block content
   - `memory_apply_patch` - Apply JSON patch to memory
   - Memory validation
   - Memory constraints
2. Integrate with agent core
3. Add memory change tracking
4. Implement memory rollback
5. Add memory tests

**Deliverables**:
- `gerbil/memory/core.ss` (~400 lines)
- Integration (~200 lines)
- Tests (~300 lines)

**Dependencies**: Week 9 completion

#### Week 11: Archival Memory

**Goal**: Long-term memory with semantic search

**Tasks**:
1. Implement `memory/archival.ss`:
   - Archival memory storage
   - Memory insertion
   - Memory retrieval
   - Text-based search (initial)
2. Add embedding generation (via LLM API):
   ```scheme
   (def (generate-embedding text provider)
     (case provider
       ((openai) (openai-embeddings text))
       ((ollama) (ollama-embeddings text))))
   ```
3. Integrate with database (text storage)
4. Implement memory pagination
5. Add memory statistics

**Deliverables**:
- `gerbil/memory/archival.ss` (~600 lines)
- Embedding generation (~200 lines)
- Tests (~300 lines)

**Dependencies**: Week 10 completion

#### Week 12: Semantic Search (Optional)

**Goal**: Vector-based semantic search

**Options**:
1. **Option A**: Use pgvector via Elixir
2. **Option B**: Implement in Zig/Rust
3. **Option C**: Use external service (Qdrant, Weaviate)

**Recommended**: Option A (leverage PostgreSQL)

**Tasks**:
1. Add pgvector extension to PostgreSQL
2. Extend Elixir database layer for vector operations
3. Implement `memory/search.ss`:
   - Vector similarity search
   - Hybrid search (text + vector)
   - Search ranking
   - Search filtering
4. Add search optimization
5. Benchmark search performance

**Deliverables**:
- `gerbil/memory/search.ss` (~400 lines)
- Elixir vector operations (~300 lines)
- Tests (~200 lines)

**Dependencies**: Week 11 completion

**Milestone**: Complete memory system with blocks, core operations, and archival

---

### Phase 5: Tool System Enhancement (Weeks 13-16)

#### Week 13: Core Tools

**Goal**: Memos-compatible core tools

**Tasks**:
1. Implement `tools/core.ss`:
   - `send_message` - Send message to user
   - `conversation_search` - Search conversation history
   - `conversation_search_date` - Search by date
   - `archival_memory_insert` - Insert to archival
   - `archival_memory_search` - Search archival
2. Integrate with message manager
3. Integrate with memory system
4. Add tool validation
5. Add tool tests

**Deliverables**:
- `gerbil/tools/core.ss` (~500 lines)
- Integration (~200 lines)
- Tests (~300 lines)

**Dependencies**: Week 12 completion

#### Week 14: Memory Tools

**Goal**: Memory manipulation tools

**Tasks**:
1. Implement `tools/memory.ss`:
   - `core_memory_append` - Append to core memory
   - `core_memory_replace` - Replace core memory
   - `memory_apply_patch` - Apply JSON patch
   - Memory validation in tools
   - Memory constraints enforcement
2. Add tool documentation
3. Add tool examples
4. Implement tool helpers

**Deliverables**:
- `gerbil/tools/memory.ss` (~400 lines)
- Documentation (~100 lines)
- Tests (~300 lines)

**Dependencies**: Week 13 completion

#### Week 15: Tool Execution Sandbox

**Goal**: Isolated tool execution

**Tasks**:
1. Implement `tools/sandbox.ss`:
   - Process isolation
   - Resource limits (CPU, memory, time)
   - Filesystem restrictions
   - Network restrictions
2. Add sandbox configuration
3. Implement sandbox monitoring
4. Add sandbox cleanup
5. Test sandbox security

**Deliverables**:
- `gerbil/tools/sandbox.ss` (~600 lines)
- Configuration (~100 lines)
- Tests (~300 lines)

**Dependencies**: Week 14 completion

#### Week 16: Tool Rules & Approval

**Goal**: Tool execution constraints

**Tasks**:
1. Implement `tools/rules.ss`:
   - `run_first` - Tool must run first
   - `exit_loop` - Tool exits agent loop
   - `requires_approval` - Tool needs user approval
   - `terminal` - Tool is terminal
   - Rule validation
   - Rule enforcement
2. Add approval workflow
3. Implement rule configuration
4. Add rule tests

**Deliverables**:
- `gerbil/tools/rules.ss` (~400 lines)
- Approval workflow (~200 lines)
- Tests (~300 lines)

**Dependencies**: Week 15 completion

**Milestone**: Complete tool system with core tools, memory tools, sandbox, and rules

---

### Phase 6: Agent Execution Loop (Weeks 17-20)

#### Week 17: Step Execution

**Goal**: Memos-compatible agent step execution

**Tasks**:
1. Implement `agent/executor.ss`:
   - Step-based execution
   - LLM inference with tools
   - Tool call parsing
   - Tool execution
   - Memory updates
   - Response generation
2. Add step tracking
3. Implement step persistence
4. Add step statistics
5. Create execution tests

**Deliverables**:
- `gerbil/agent/executor.ss` (~800 lines)
- Step tracking (~200 lines)
- Tests (~400 lines)

**Dependencies**: Week 16 completion

#### Week 18: Context Window Management

**Goal**: Automatic context management

**Tasks**:
1. Implement `agent/context.ss`:
   - Context window calculation
   - Message truncation
   - Automatic summarization
   - Context optimization
   - Token counting
2. Add summarization triggers
3. Implement summarization via LLM
4. Add context caching
5. Test context management

**Deliverables**:
- `gerbil/agent/context.ss` (~500 lines)
- Summarization (~300 lines)
- Tests (~300 lines)

**Dependencies**: Week 17 completion

#### Week 19: Streaming Execution

**Goal**: Streaming agent responses

**Tasks**:
1. Implement `agent/stream.ss`:
   - Streaming step execution
   - Streaming tool calls
   - Streaming memory updates
   - Streaming response generation
   - Stream error handling
2. Integrate with message streaming
3. Add stream lifecycle management
4. Implement stream cancellation
5. Test streaming execution

**Deliverables**:
- `gerbil/agent/stream.ss` (~600 lines)
- Integration (~200 lines)
- Tests (~300 lines)

**Dependencies**: Week 18 completion

#### Week 20: Performance & Optimization

**Goal**: Production-ready performance

**Tasks**:
1. Profile agent execution
2. Optimize hot paths:
   - Message serialization
   - Database queries
   - LLM API calls
   - Memory operations
3. Add caching layers
4. Implement connection pooling
5. Add performance monitoring
6. Create performance benchmarks
7. Write optimization guide

**Deliverables**:
- Performance optimizations (~500 lines)
- Benchmarks (~200 lines)
- Documentation (~100 lines)

**Dependencies**: Week 19 completion

**Milestone**: Complete agent execution loop with streaming and optimization

---

## 📊 Deliverables Summary

### Code Deliverables (by Phase)

| Phase | Component | Lines | Status |
|-------|-----------|-------|--------|
| 0 | Elixir Supervision | ~2,000 | ✅ Done |
| 1 | Agent Core | ~3,650 | ✅ Done |
| 2 | LLM & HTTP | ~4,300 | ❌ Todo |
| 3 | Database & Messages | ~4,200 | ❌ Todo |
| 4 | Advanced Memory | ~3,100 | ❌ Todo |
| 5 | Tool System | ~3,500 | ❌ Todo |
| 6 | Execution Loop | ~3,500 | ❌ Todo |
| **Total** | **All Components** | **~24,250** | **24% Done** |

### Documentation Deliverables

- ✅ Architecture documentation (Phase 0 & 1)
- ⏳ API documentation (Phase 2)
- ⏳ Database schema documentation (Phase 3)
- ⏳ Memory system guide (Phase 4)
- ⏳ Tool development guide (Phase 5)
- ⏳ Agent execution guide (Phase 6)
- ⏳ Deployment guide (Phase 6)

### Testing Deliverables

- ✅ Unit tests (Phase 0 & 1): 30+ test cases
- ⏳ Integration tests (Phase 2-6): ~100+ test cases
- ⏳ Performance benchmarks (Phase 6)
- ⏳ End-to-end tests (Phase 6)

---

## 🎯 Success Criteria

### Phase 2 Success Criteria
- [ ] Can call OpenAI, Anthropic, Groq, Ollama APIs
- [ ] HTTP server running on port 8283
- [ ] Can create agent via REST API
- [ ] Can send message to agent via REST API

### Phase 3 Success Criteria
- [ ] PostgreSQL database with schema
- [ ] Can persist agents to database
- [ ] Can persist messages to database
- [ ] Can retrieve conversation history
- [ ] Streaming responses working

### Phase 4 Success Criteria
- [ ] Memory blocks working (persona, human, custom)
- [ ] Core memory operations working
- [ ] Archival memory with search
- [ ] Semantic search (optional)

### Phase 5 Success Criteria
- [ ] Core tools working (send_message, search, etc.)
- [ ] Memory tools working (append, replace, patch)
- [ ] Tool sandbox working
- [ ] Tool rules enforced

### Phase 6 Success Criteria
- [ ] Agent step execution working
- [ ] LLM inference with tool calls
- [ ] Context window management
- [ ] Streaming execution
- [ ] Performance benchmarks met

---

## 📈 Progress Tracking

### Weekly Milestones

| Week | Milestone | Deliverable |
|------|-----------|-------------|
| 1 | OpenAI & Anthropic clients | LLM integration |
| 2 | Additional providers | Provider abstraction |
| 3 | HTTP server | Basic server |
| 4 | REST API | Agent API |
| 5 | Database schema | PostgreSQL setup |
| 6 | Database client | Gerbil-Elixir protocol |
| 7 | Message manager | Message persistence |
| 8 | Message streaming | Streaming support |
| 9 | Memory blocks | Structured memory |
| 10 | Core memory ops | Memory tools |
| 11 | Archival memory | Long-term storage |
| 12 | Semantic search | Vector search |
| 13 | Core tools | Basic tools |
| 14 | Memory tools | Memory manipulation |
| 15 | Tool sandbox | Isolated execution |
| 16 | Tool rules | Execution constraints |
| 17 | Step execution | Agent loop |
| 18 | Context management | Auto-summarization |
| 19 | Streaming execution | Streaming loop |
| 20 | Performance | Optimization |

### Monthly Goals

**Month 1 (Weeks 1-4)**: LLM integration + HTTP server
**Month 2 (Weeks 5-8)**: Database + Message system
**Month 3 (Weeks 9-12)**: Advanced memory
**Month 4 (Weeks 13-16)**: Tool system
**Month 5 (Weeks 17-20)**: Execution loop

---

## 🚀 Getting Started (Week 1)

### Immediate Tasks

1. **Setup Development Environment**
   ```bash
   cd /Users/liyuhang/work/o
   mkdir -p gerbil/llm gerbil/server gerbil/database gerbil/message gerbil/memory gerbil/tools
   ```

2. **Copy Gerbil Examples**
   ```bash
   cp ../gerbil_scheme_book/OpenAI_API_demo/openai.ss gerbil/llm/
   cp ../gerbil_scheme_book/Anthropic_API_demo/anthropic.ss gerbil/llm/
   ```

3. **Create Week 1 Branch**
   ```bash
   git checkout -b phase2-week1-llm-clients
   ```

4. **Start Implementation**
   - Adapt OpenAI client
   - Adapt Anthropic client
   - Create unified types
   - Write tests

---

## 💡 Key Principles

### 1. Incremental Delivery
- Each week delivers working functionality
- No big-bang integration
- Continuous testing

### 2. Gerbil-First
- Use Gerbil for everything possible
- Only use Zig/Rust for performance-critical operations
- Leverage gerbil_scheme_book examples

### 3. Memos Compatibility
- Follow memos API design
- Use memos database schema
- Replicate memos functionality

### 4. Quality Over Speed
- Write tests for everything
- Document as you go
- Refactor when needed

### 5. Leverage Existing Work
- Use Phase 0 & 1 foundation
- Adapt gerbil_scheme_book examples
- Extend Elixir supervision layer

---

## 🎊 Conclusion

**This roadmap provides a clear, achievable path to building a memos-compatible agent in Gerbil Scheme.**

**Key Advantages**:
- ✅ Leverages existing Phase 0 & 1 work
- ✅ Uses available Gerbil examples
- ✅ Incremental weekly milestones
- ✅ Clear success criteria
- ✅ Realistic timeline (20 weeks)

**Next Steps**:
1. Review and approve this roadmap
2. Start Week 1 implementation
3. Track progress weekly
4. Adjust as needed

**Estimated Completion**: ~5 months from now
**Confidence**: ⭐⭐⭐⭐⭐ (5/5)

---

**Let's build the future of AI agents in Gerbil Scheme!** 🚀
