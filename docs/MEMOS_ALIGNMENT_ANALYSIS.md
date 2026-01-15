# Project O - Memos Alignment Analysis

**Date**: 2026-01-16
**Purpose**: Realign Project O to match memos agent functionality
**Priority**: Gerbil Scheme first, Zig/Rust only when necessary

---

## 🎯 Core Realization

**Project O should replicate memos agent functionality**, not just be a generic self-evolving agent. The previous architecture (Phase 0 & 1) provides the **foundation**, but now we need to build the **actual agent capabilities** that memos provides.

---

## 📊 Memos Core Functionality Analysis

### What Memos Does (Must Replicate)

#### 1. **Stateful AI Agent with Memory** ✅ (Partially done in Phase 1)
- **Core Memory**: Editable memory blocks (persona, human, custom)
- **Archival Memory**: Long-term storage with semantic search
- **Recall Memory**: Conversation history
- **Memory Blocks**: Structured memory with read-only protection

**Status in Project O**:
- ✅ Basic memory system (short-term, long-term, episodic, semantic)
- ❌ Missing: Memory blocks with read-only protection
- ❌ Missing: Archival memory with semantic search
- ❌ Missing: Structured persona/human memory

#### 2. **Multi-Provider LLM Support** ❌ (Not implemented)
- OpenAI, Anthropic, Azure, Groq, Ollama, Mistral, DeepSeek, Google Vertex, etc.
- Unified provider management (memos innovation)
- Dynamic model discovery
- API key encryption

**Status in Project O**:
- ❌ No LLM provider integration
- ❌ No API client implementations
- ❌ No provider management

#### 3. **Tool System** ✅ (Basic framework done in Phase 1)
- Core tools: `send_message`, `conversation_search`, `memory` operations
- Memory tools: `core_memory_append`, `core_memory_replace`
- Custom tools: User-defined functions
- Tool execution sandbox

**Status in Project O**:
- ✅ Tool registry and execution framework
- ✅ Parameter validation
- ❌ Missing: Core memory manipulation tools
- ❌ Missing: Conversation search tools
- ❌ Missing: Sandbox execution

#### 4. **Message Management** ❌ (Not implemented)
- Message creation and retrieval
- Message search with semantic capabilities
- Conversation history tracking
- Tool return messages
- Message streaming

**Status in Project O**:
- ✅ Basic conversation tracking in state
- ❌ Missing: Persistent message storage
- ❌ Missing: Message search
- ❌ Missing: Streaming support

#### 5. **Agent Execution Loop** ❌ (Not implemented)
- Step-based execution
- LLM inference with tool calls
- Tool execution and result handling
- Memory updates
- Response generation

**Status in Project O**:
- ✅ Basic agent loop structure in DSL
- ❌ Missing: LLM integration
- ❌ Missing: Tool call parsing
- ❌ Missing: Step tracking

#### 6. **API Layer** ❌ (Not implemented)
- REST API for agent management
- Agent CRUD operations
- Message endpoints
- Tool endpoints
- Provider endpoints

**Status in Project O**:
- ❌ No HTTP server
- ❌ No REST API
- ❌ Only Elixir supervision layer

#### 7. **Database Persistence** ❌ (Partially done)
- PostgreSQL with pgvector
- Agent state persistence
- Message history
- Tool definitions
- Provider configurations

**Status in Project O**:
- ✅ Checkpoint/WAL persistence (Elixir)
- ❌ Missing: PostgreSQL integration
- ❌ Missing: Structured database schema
- ❌ Missing: pgvector for embeddings

---

## 🔄 Architecture Realignment

### Current Architecture (Phase 0 & 1)

```
┌─────────────────────────────────────┐
│  Elixir Supervision Layer           │  ✅ Done
│  (Fault tolerance, checkpoints)     │
└─────────────────────────────────────┘
              ↕
┌─────────────────────────────────────┐
│  Gerbil Agent Core                  │  ✅ Done (Basic)
│  (Lifecycle, DSL, Memory, Tools)    │
└─────────────────────────────────────┘
              ↕
┌─────────────────────────────────────┐
│  Infrastructure (Zig/Rust)          │  ❌ Not started
│  (HTTP, Database, etc.)             │
└─────────────────────────────────────┘
```

### Target Architecture (Memos-Aligned)

```
┌─────────────────────────────────────────────────────────┐
│              Elixir Supervision Layer                   │
│  (Fault tolerance, crash recovery, shadow testing)      │
└─────────────────────────────────────────────────────────┘
                        ↕
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
│                                                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐ │
│  │  Provider    │  │  Database    │  │   API        │ │
│  │  Manager     │  │  Client      │  │   Routes     │ │
│  └──────────────┘  └──────────────┘  └──────────────┘ │
└─────────────────────────────────────────────────────────┘
```

**Key Changes**:
1. **Gerbil does more**: HTTP server, database client, LLM clients
2. **Zig/Rust only for**: Performance-critical operations (embeddings, vector search)
3. **Focus on**: Replicating memos functionality, not generic evolution

---

## 🛠️ Technology Stack Realignment

### Previous Plan (Generic Evolution Focus)

| Layer | Technology | Reason |
|-------|-----------|--------|
| Supervision | Elixir/OTP | Fault tolerance |
| Agent Core | Gerbil Scheme | Metaprogramming |
| Infrastructure | **Zig** | HTTP, databases |
| Compute | **Rust** | Vector operations |

### New Plan (Memos-Aligned, Gerbil-First)

| Layer | Technology | Reason | Priority |
|-------|-----------|--------|----------|
| Supervision | Elixir/OTP | Fault tolerance | ✅ Done |
| Agent Core | Gerbil Scheme | Agent logic | ✅ Done (Basic) |
| **HTTP Server** | **Gerbil Scheme** | REST API | 🔥 High |
| **LLM Clients** | **Gerbil Scheme** | OpenAI, Anthropic, etc. | 🔥 High |
| **Database** | **Gerbil Scheme** | PostgreSQL client | 🔥 High |
| **Message System** | **Gerbil Scheme** | Message management | 🔥 High |
| **Provider System** | **Gerbil Scheme** | Provider management | 🔥 High |
| Vector Search | Zig/Rust (optional) | pgvector operations | 🔵 Low |
| Embeddings | Zig/Rust (optional) | Fast computation | 🔵 Low |

**Rationale**:
- **Gerbil has HTTP libraries**: Can build REST API in Gerbil
- **Gerbil has database libraries**: Can connect to PostgreSQL
- **Gerbil has JSON/HTTP**: Can call LLM APIs
- **Zig/Rust only needed**: For performance-critical operations

---

## 📚 Gerbil Scheme Resources Available

### From gerbil_scheme_book

#### 1. **HTTP Client** ✅ Available
- **Files**: `OpenAI_API_demo`, `Gemini_API_demo`, `Groq_API_demo`, `Ollama_API_demo`
- **Capabilities**: HTTP GET/POST, JSON parsing, API authentication
- **Reusable**: ~200 lines per provider

**Example**:
```scheme
(import :std/net/request :std/text/json)

(def (openai-chat-completion messages model api-key)
  (let* ((url "https://api.openai.com/v1/chat/completions")
         (headers `(("Authorization" . ,(string-append "Bearer " api-key))
                    ("Content-Type" . "application/json")))
         (body (hash ("model" model) ("messages" messages)))
         (response (http-post url headers: headers json: body)))
    (if (= (request-status response) 200)
        (request-json response)
        (error "API request failed"))))
```

#### 2. **Database Client** ⚠️ Limited
- **Files**: `SparqlRdfStore` (RDF database via FFI)
- **Capabilities**: SPARQL queries, RDF triples
- **Limitation**: No direct PostgreSQL client
- **Solution**: Use FFI to wrap libpq or use HTTP API

#### 3. **Command-Line Parsing** ✅ Available
- **Files**: `command_line_utilities_first_demo_START_HERE`
- **Capabilities**: Argument parsing, command dispatch
- **Reusable**: ~100 lines

#### 4. **JSON Processing** ✅ Available
- **Standard Library**: `:std/text/json`
- **Capabilities**: Parse, generate, manipulate JSON
- **Used in**: All API demo projects

#### 5. **String Processing** ✅ Available
- **Files**: NLP utilities
- **Capabilities**: Tokenization, parsing, manipulation
- **Reusable**: ~500 lines

#### 6. **FFI Examples** ✅ Available
- **Files**: `RaptorRDF_FFI`, `SparqlRdfStore`
- **Capabilities**: C library integration
- **Pattern**: Can wrap libpq for PostgreSQL

---

## 🎯 Revised Implementation Roadmap

### Phase 2: Memos Core Functionality (Gerbil-First)

#### Week 1-2: LLM Provider Integration
**Goal**: Support OpenAI, Anthropic, Groq, Ollama

**Tasks**:
1. Create `gerbil/llm/` directory
2. Implement `llm/openai.ss` (adapt from gerbil_scheme_book)
3. Implement `llm/anthropic.ss`
4. Implement `llm/groq.ss`
5. Implement `llm/ollama.ss`
6. Create `llm/client.ss` (unified interface)
7. Add provider configuration management

**Deliverables**:
- 5 LLM client modules (~1,000 lines)
- Unified LLM client interface
- Provider configuration system

#### Week 3-4: HTTP Server & REST API
**Goal**: REST API for agent management

**Tasks**:
1. Research Gerbil HTTP server libraries
2. Implement `server/http.ss` (HTTP server)
3. Implement `server/routes.ss` (API routes)
4. Create endpoints:
   - `POST /agents` - Create agent
   - `GET /agents/:id` - Get agent
   - `POST /agents/:id/messages` - Send message
   - `GET /agents/:id/messages` - Get messages
   - `POST /agents/:id/tools` - Attach tool
5. Add request/response serialization

**Deliverables**:
- HTTP server module (~500 lines)
- REST API routes (~800 lines)
- API documentation

#### Week 5-6: Database Integration
**Goal**: PostgreSQL persistence

**Options**:
1. **Option A**: FFI wrapper for libpq
2. **Option B**: HTTP API to PostgreSQL REST API
3. **Option C**: Use Elixir for database, Gerbil for logic

**Recommended**: Option C (leverage existing Elixir)

**Tasks**:
1. Extend Elixir `MemoryVault` for structured storage
2. Add database schema for:
   - Agents table
   - Messages table
   - Tools table
   - Providers table
   - Memory blocks table
3. Create Gerbil-Elixir protocol for database operations
4. Implement database client in Gerbil

**Deliverables**:
- Extended Elixir database layer (~1,000 lines)
- Gerbil database client (~300 lines)
- Database migrations

#### Week 7-8: Message System
**Goal**: Message management and search

**Tasks**:
1. Implement `message/manager.ss`
2. Add message persistence (via Elixir)
3. Implement conversation history
4. Add message search (text-based)
5. Implement message streaming

**Deliverables**:
- Message manager module (~600 lines)
- Message search functionality
- Streaming support

### Phase 3: Advanced Memory System

#### Week 9-10: Memory Blocks
**Goal**: Structured memory with read-only protection

**Tasks**:
1. Implement `memory/blocks.ss`
2. Add memory block types (persona, human, custom)
3. Implement read-only protection
4. Add memory block CRUD operations
5. Integrate with agent core

**Deliverables**:
- Memory blocks module (~500 lines)
- Memory block management API

#### Week 11-12: Archival Memory
**Goal**: Long-term memory with semantic search

**Tasks**:
1. Implement `memory/archival.ss`
2. Add embedding generation (via LLM API)
3. Implement vector storage (pgvector via Elixir)
4. Add semantic search
5. Implement memory consolidation

**Deliverables**:
- Archival memory module (~700 lines)
- Semantic search functionality

### Phase 4: Tool System Enhancement

#### Week 13-14: Core Tools
**Goal**: Memos-compatible core tools

**Tasks**:
1. Implement `tools/core.ss`
2. Add `send_message` tool
3. Add `conversation_search` tool
4. Add `core_memory_append` tool
5. Add `core_memory_replace` tool
6. Add `memory_apply_patch` tool

**Deliverables**:
- Core tools module (~400 lines)
- Tool documentation

#### Week 15-16: Tool Execution
**Goal**: Sandbox execution and tool rules

**Tasks**:
1. Implement `tools/executor.ss`
2. Add sandbox execution (process isolation)
3. Implement tool rules (run_first, exit_loop, etc.)
4. Add tool approval workflow
5. Implement tool result handling

**Deliverables**:
- Tool executor module (~600 lines)
- Sandbox implementation

### Phase 5: Agent Execution Loop

#### Week 17-18: Step Execution
**Goal**: Memos-compatible agent execution

**Tasks**:
1. Implement `agent/executor.ss`
2. Add step-based execution
3. Implement LLM inference with tool calls
4. Add tool call parsing
5. Implement memory updates
6. Add response generation
7. Implement step tracking

**Deliverables**:
- Agent executor module (~800 lines)
- Step tracking system

#### Week 19-20: Streaming & Optimization
**Goal**: Production-ready execution

**Tasks**:
1. Implement streaming responses
2. Add context window management
3. Implement automatic summarization
4. Add performance monitoring
5. Optimize memory usage

**Deliverables**:
- Streaming support (~300 lines)
- Performance optimizations

---

## 📊 Revised Statistics

### Target Implementation (Memos-Aligned)

| Component | Technology | Lines | Status |
|-----------|-----------|-------|--------|
| **Phase 0: Elixir Supervision** | Elixir | ~2,000 | ✅ Done |
| **Phase 1: Agent Core** | Gerbil | ~3,650 | ✅ Done |
| **Phase 2: LLM & HTTP** | Gerbil | ~2,300 | ❌ Todo |
| **Phase 3: Memory System** | Gerbil | ~1,200 | ❌ Todo |
| **Phase 4: Tool System** | Gerbil | ~1,000 | ❌ Todo |
| **Phase 5: Execution Loop** | Gerbil | ~1,100 | ❌ Todo |
| **Database Layer** | Elixir | ~1,000 | ❌ Todo |
| **Vector Operations** | Zig/Rust | ~500 | ⏳ Optional |
| **Total** | Mixed | **~12,750** | **40% Done** |

### Comparison with Memos

| Feature | Memos | Project O | Gap |
|---------|-------|-----------|-----|
| Agent Core | ✅ | ✅ | None |
| Memory System | ✅ | 🟡 | Partial |
| LLM Providers | ✅ (17+) | ❌ | Large |
| Tool System | ✅ | 🟡 | Partial |
| HTTP API | ✅ | ❌ | Large |
| Database | ✅ | 🟡 | Partial |
| Message System | ✅ | ❌ | Large |
| Streaming | ✅ | ❌ | Large |

---

## 🎯 Immediate Next Steps

### 1. Update Documentation (Today)
- ✅ Create this alignment analysis
- ⏳ Update README with memos-aligned goals
- ⏳ Revise IMPLEMENTATION_CHECKLIST
- ⏳ Update ARCHITECTURE_V2 with new focus

### 2. Prototype LLM Client (This Week)
- ⏳ Adapt OpenAI client from gerbil_scheme_book
- ⏳ Test with simple chat completion
- ⏳ Integrate with agent core

### 3. Design HTTP Server (This Week)
- ⏳ Research Gerbil HTTP server options
- ⏳ Design REST API endpoints
- ⏳ Create API specification

### 4. Plan Database Integration (Next Week)
- ⏳ Design database schema (memos-compatible)
- ⏳ Extend Elixir database layer
- ⏳ Create Gerbil-Elixir protocol

---

## 💡 Key Insights

### 1. Gerbil Can Do More Than Expected
- HTTP client: ✅ Available
- JSON processing: ✅ Available
- String manipulation: ✅ Available
- FFI for C libraries: ✅ Available

### 2. Zig/Rust Less Critical
- Only needed for: Vector operations, embeddings
- Most functionality: Can be done in Gerbil
- Performance: Gerbil is fast enough for most tasks

### 3. Leverage Existing Elixir
- Database operations: Use Elixir
- Fault tolerance: Already done
- Supervision: Already done

### 4. Focus on Memos Parity
- Goal: Replicate memos functionality
- Not: Generic self-evolution (that's Phase 6+)
- Priority: Working agent with memory and tools

---

## 🎊 Conclusion

**Project O needs to pivot from "generic self-evolving agent" to "memos-compatible agent in Gerbil Scheme".**

**Key Changes**:
1. ✅ Keep Phase 0 & 1 (foundation is solid)
2. 🔄 Rewrite Phase 2-5 (focus on memos functionality)
3. 🔥 Prioritize Gerbil over Zig/Rust
4. 🎯 Target memos feature parity

**Timeline**:
- **Weeks 1-4**: LLM clients + HTTP server
- **Weeks 5-8**: Database + Message system
- **Weeks 9-12**: Advanced memory
- **Weeks 13-16**: Tool system
- **Weeks 17-20**: Execution loop

**Estimated Completion**: 20 weeks (~5 months)

**Confidence**: ⭐⭐⭐⭐⭐ (5/5) - Clear path forward with available resources

---

**Next Document**: `REVISED_ROADMAP.md` - Detailed week-by-week plan
