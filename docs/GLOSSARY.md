# Glossary

Definitions of key terms and concepts used in Project O.

---

## A

**ADR (Architecture Decision Record)**
- Document that captures an important architectural decision
- Includes context, decision, consequences, and alternatives
- Located in `docs/adr/`

**Agent**
- The self-evolving AI system at the core of Project O
- Can modify its own code at runtime
- Composed of Gerbil Scheme code

**Application Layer**
- Layer containing agent logic, DSL, memory, and tools
- Implemented in Gerbil Scheme
- 53% of the codebase

---

## B

**BEAM VM**
- Erlang Virtual Machine that runs Elixir
- Known for fault tolerance and concurrency
- Supports millions of lightweight processes

---

## C

**Canary Deployment**
- Gradual rollout strategy for new code
- Routes small percentage of traffic to new version
- Monitors metrics before full deployment

**Checkpoint**
- Full snapshot of agent state
- Created every 5 minutes or on-demand
- Compressed with zstd level 9
- Stored in DETS + file backup

**COW (Copy-on-Write)**
- Memory optimization technique
- Shares read-only memory between processes
- Only copies modified pages

**Compute Layer**
- Layer for compute-intensive operations
- Implemented in Rust
- 8% of the codebase

---

## D

**DETS (Disk-based Erlang Term Storage)**
- Erlang's persistent key-value store
- Used for checkpoint storage
- Provides fast in-memory access with disk persistence

**DSL (Domain Specific Language)**
- Custom language for defining agent behaviors
- Implemented as Gerbil macros
- Examples: `defagent`, `deftool`, `when->`

---

## E

**Elixir**
- Functional programming language on BEAM VM
- Used for supervision layer
- 10% of the codebase

**ETS (Erlang Term Storage)**
- In-memory key-value store
- Used for metrics and fast lookups
- Supports concurrent access

**Evolution**
- Process of agent modifying its own code
- Includes generation, testing, and deployment
- Protected by shadow testing

**Evolution Arbiter**
- Elixir module that orchestrates evolution
- Manages shadow instances
- Makes promotion/rejection decisions

---

## F

**FFI (Foreign Function Interface)**
- Mechanism for calling code in other languages
- Gerbil uses FFI to call Zig/Rust functions
- Enables high-performance operations

**Foundation Layer**
- Layer of C libraries (PostgreSQL, SQLite, OpenSSL)
- 2% of the codebase

---

## G

**Gambit Scheme**
- Scheme implementation that Gerbil is built on
- Provides C FFI capabilities
- Compiles to native code

**GenServer**
- Elixir behavior for implementing server processes
- Provides standard callbacks (init, handle_call, etc.)
- Used by all core Elixir modules

**Gerbil Scheme**
- Lisp dialect used for agent implementation
- Supports metaprogramming and macros
- Compiles to native code via Gambit

---

## H

**Heartbeat**
- Periodic signal from Gerbil to Elixir
- Sent every 1 second
- Timeout after 5 seconds triggers restart

**Hot Reload**
- Loading new code without restarting the system
- Preserves running state
- Enabled by Lisp's dynamic nature

---

## I

**Infrastructure Layer**
- Layer for I/O operations (HTTP, databases)
- Implemented in Zig
- 15% of the codebase

---

## L

**LLM (Large Language Model)**
- AI model used for code generation
- Examples: Claude, GPT-4, Ollama
- Pluggable in O's architecture

---

## M

**MessagePack**
- Binary serialization format
- Used for Elixir-Gerbil communication
- 2-5x faster than JSON

**Meta Layer**
- Layer for self-modification engine
- Implemented in Gerbil Scheme
- 12% of the codebase

**Multi-Threaded Evolution**
- Running multiple evolution experiments in parallel
- Uses genetic algorithm approach
- Can spawn 50+ shadow instances

---

## O

**OTP (Open Telecom Platform)**
- Framework for building fault-tolerant systems
- Part of Erlang/Elixir ecosystem
- Provides supervision trees

---

## P

**Port**
- Erlang mechanism for communicating with external processes
- Used for Elixir-Gerbil communication
- Provides process isolation

---

## R

**Rollback**
- Reverting to a previous checkpoint
- Triggered on evolution failure
- Automatic via supervisor

---

## S

**Sandbox**
- Isolated environment for testing code
- Prevents damage from buggy code
- Used in shadow testing

**Shadow Instance**
- Isolated copy of agent for testing
- Runs new code without affecting main instance
- Receives subset of traffic for comparison

**Shadow Testing**
- Testing new code in isolated instance
- Compares performance with main instance
- Automatic promotion or rejection

**Shared Memory**
- Memory region accessible by multiple processes
- Used for hot path data (metrics, indexes)
- Avoids serialization overhead

**SIMD (Single Instruction, Multiple Data)**
- CPU instruction for parallel operations
- Used in Rust for vector operations
- Improves performance significantly

**Supervision Layer**
- Elixir/OTP layer that manages agent lifecycle
- Prevents permanent failure
- 10% of the codebase

**Supervisor**
- OTP behavior for monitoring processes
- Automatically restarts failed processes
- Implements fault tolerance strategies

---

## T

**Telemetry**
- System for collecting metrics
- Emits events for monitoring
- Integrated with Prometheus

**Traffic Splitter**
- Routes requests between main and shadow instances
- Supports A/B testing
- Configurable split ratio

---

## V

**Vector Embedding**
- Numerical representation of text
- Used for semantic search in memory
- Typically 1536 dimensions

---

## W

**WAL (Write-Ahead Log)**
- Log of operations before execution
- Ensures durability
- Used for crash recovery

**WAL Replay**
- Re-executing operations from WAL
- Part of recovery process
- Restores state after checkpoint

---

## Z

**Zig**
- Systems programming language
- Used for infrastructure layer
- Fast, safe, simple C interop

---

## Acronyms

| Acronym | Full Form | Description |
|---------|-----------|-------------|
| ADR | Architecture Decision Record | Document capturing design decisions |
| AOT | Ahead-of-Time | Compilation before runtime |
| API | Application Programming Interface | Interface for software interaction |
| BEAM | Bogdan/Björn's Erlang Abstract Machine | Erlang VM |
| CI/CD | Continuous Integration/Continuous Deployment | Automated build and deploy |
| COW | Copy-on-Write | Memory optimization technique |
| CPU | Central Processing Unit | Computer processor |
| DETS | Disk-based Erlang Term Storage | Persistent key-value store |
| DSL | Domain Specific Language | Custom language for specific domain |
| ETS | Erlang Term Storage | In-memory key-value store |
| FFI | Foreign Function Interface | Cross-language function calls |
| GAN | Generative Adversarial Network | ML architecture (used metaphorically) |
| GUI | Graphical User Interface | Visual interface |
| HTTP | Hypertext Transfer Protocol | Web protocol |
| I/O | Input/Output | Data transfer operations |
| JSON | JavaScript Object Notation | Data format |
| LLM | Large Language Model | AI language model |
| ML | Machine Learning | AI technique |
| NIF | Native Implemented Function | Erlang extension mechanism |
| OTP | Open Telecom Platform | Erlang framework |
| QPS | Queries Per Second | Throughput metric |
| RAM | Random Access Memory | Computer memory |
| REPL | Read-Eval-Print Loop | Interactive programming environment |
| REST | Representational State Transfer | API architecture |
| SIMD | Single Instruction, Multiple Data | Parallel processing |
| SQL | Structured Query Language | Database query language |
| SSH | Secure Shell | Remote access protocol |
| SSL/TLS | Secure Sockets Layer/Transport Layer Security | Encryption protocols |
| TDD | Test-Driven Development | Development methodology |
| UUID | Universally Unique Identifier | Unique ID format |
| VM | Virtual Machine | Abstraction layer |
| WAL | Write-Ahead Log | Durability mechanism |
| YAML | YAML Ain't Markup Language | Configuration format |

---

## Concepts

### Checkpoint Strategy
Hybrid approach combining:
- **Periodic checkpoints**: Full state snapshots every 5 minutes
- **WAL**: Operation log for durability
- **Shared memory**: Hot path optimization

### Evolution Cycle
1. **Perception**: Detect improvement opportunity
2. **Planning**: Generate new code
3. **Checkpoint**: Save current state
4. **Shadow Test**: Test in isolated instance
5. **Evaluation**: Compare performance
6. **Decision**: Promote or reject
7. **Deployment**: Hot reload if promoted

### Fault Tolerance
- **Supervision trees**: Hierarchical process monitoring
- **Let it crash**: Philosophy of failing fast and recovering
- **Isolation**: Failures don't propagate
- **Automatic restart**: Supervisor restarts failed processes

### Genetic Algorithm Evolution
1. **Population**: Multiple code variants
2. **Mutation**: Random modifications
3. **Crossover**: Mixing successful variants
4. **Selection**: Keeping best performers
5. **Iteration**: Repeat for N generations

### Self-Modification
Agent's ability to:
- Analyze its own performance
- Generate improved code
- Test changes safely
- Deploy updates automatically

---

## Related Terms

### Similar Systems
- **AutoML**: Automated machine learning
- **Meta-learning**: Learning to learn
- **Self-improving AI**: AI that improves itself
- **Genetic programming**: Evolving programs

### Related Technologies
- **Lisp machines**: Historical self-modifying systems
- **Smalltalk**: Image-based development
- **Erlang**: Fault-tolerant systems
- **Actor model**: Concurrent computation model

---

**Last Updated**: 2026-01-16  
**Version**: 1.0
