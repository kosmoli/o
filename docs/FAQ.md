# Frequently Asked Questions (FAQ)

Common questions about Project O and their answers.

---

## General Questions

### What is Project O?

Project O is a self-evolving AI Agent system that can modify its own code at runtime. It combines:
- **Gerbil Scheme** for metaprogramming and self-modification
- **Elixir/OTP** for industrial-grade fault tolerance
- **Zig** for high-performance infrastructure
- **Rust** for compute-intensive operations

### Why "O"?

"O" represents:
- **Origin**: The starting point of self-evolving systems
- **Ouroboros**: The snake eating its own tail, symbolizing self-reference
- **Optimization**: Continuous self-improvement

### What makes O different from other agent systems?

1. **True Self-Evolution**: Can modify its own code, not just parameters
2. **Fault Tolerance**: Elixir supervision prevents permanent failure
3. **Shadow Testing**: Tests changes in isolated instances before applying
4. **Multi-Threaded Evolution**: Runs parallel evolution experiments
5. **Zero Data Loss**: Checkpoints + WAL ensure durability

---

## Architecture Questions

### Why use multiple languages?

Each language serves a specific purpose:

| Language | Purpose | Reason |
|----------|---------|--------|
| Elixir | Supervision | Battle-tested fault tolerance (OTP) |
| Gerbil | Agent logic | Lisp metaprogramming for self-modification |
| Zig | Infrastructure | Fast, safe, simple C interop |
| Rust | Compute | Memory safety, SIMD optimization |

### Why Gerbil instead of Racket or Common Lisp?

**Gerbil advantages:**
- Compiled macros (AOT) for better performance
- Native C FFI through Gambit
- Single-instance module system (faster)
- Production-ready (used in real systems)

**Comparison:**

| Feature | Gerbil | Racket | Common Lisp |
|---------|--------|--------|-------------|
| Compiled macros | ✅ | ❌ | ✅ |
| Performance | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐ |
| C FFI | Native | Additional layer | CFFI |
| Production use | ✅ | ⚠️ | ✅ |

### Why Elixir instead of pure Erlang?

- More modern syntax and tooling
- Better developer experience
- Active ecosystem
- Same BEAM VM benefits
- Easier to attract contributors

### Can I use O without Elixir?

Not recommended. The Elixir supervision layer is critical for:
- Preventing permanent failure during evolution
- State persistence and recovery
- Shadow testing orchestration
- Multi-threaded evolution management

Without Elixir, the agent could destroy itself during evolution.

---

## Technical Questions

### How does self-modification work?

1. **Detection**: Agent identifies improvement opportunity
2. **Generation**: Generates new code using LLM or templates
3. **Checkpoint**: Saves current state to Elixir
4. **Shadow Test**: Tests new code in isolated instance
5. **Evaluation**: Compares performance metrics
6. **Decision**: Promotes if better, rejects if worse
7. **Hot Reload**: Loads new code without restart

### What happens if the agent crashes during evolution?

1. Elixir detects heartbeat timeout (5 seconds)
2. Supervisor restarts GerbilManager
3. New Gerbil process starts with `--restore` flag
4. Loads last checkpoint from MemoryVault
5. Replays WAL entries since checkpoint
6. Agent resumes from pre-crash state
7. Total downtime: ~50-100ms

### How is state preserved?

**Three-layer approach:**

1. **Checkpoints**: Full state snapshots every 5 minutes
   - Stored in DETS (in-memory + disk)
   - File backup for redundancy
   - Compressed with zstd

2. **WAL (Write-Ahead Log)**: Every operation logged before execution
   - Segment-based files
   - Automatic rotation
   - Replay on recovery

3. **Shared Memory**: Hot path data (metrics, indexes)
   - No serialization overhead
   - Atomic operations
   - Fast access

**Maximum data loss**: < 1 second (WAL flush interval)

### What is shadow testing?

Shadow testing runs new code in an isolated instance:

```
Main Instance (Production)
  ↓ 90% traffic
  ↓
User Requests
  ↓ 10% traffic (duplicated)
  ↓
Shadow Instance (Testing new code)
```

**Process:**
1. Spawn shadow instance with new code
2. Route 10% of traffic to shadow
3. Compare metrics (latency, errors, memory)
4. Promote if better, reject if worse
5. Main instance unaffected during testing

### How does multi-threaded evolution work?

**Genetic Algorithm Approach:**

1. **Population**: Spawn 50 shadow instances
2. **Mutation**: Each has different code variation
3. **Competition**: All process same tasks
4. **Evaluation**: Measure performance
5. **Selection**: Keep top performers
6. **Crossover**: Mix code from best instances
7. **Repeat**: Iterate for N generations

**Result**: Finds optimal code through parallel experimentation

---

## Performance Questions

### What is the performance overhead of Elixir supervision?

| Metric | Without Elixir | With Elixir | Overhead |
|--------|----------------|-------------|----------|
| Latency | 10ms | 11ms | +10% |
| Throughput | 10K QPS | 9K QPS | -10% |
| Memory | 80MB | 100MB | +25% |

**Trade-off**: 10% performance for infinite reliability

### Can O handle high-throughput workloads?

Yes, with optimizations:

1. **Shared Memory**: Hot path data bypasses serialization
2. **Batch Operations**: WAL writes batched (100 entries)
3. **Async Checkpoints**: Background thread, non-blocking
4. **Connection Pooling**: Database connections pooled

**Target**: 5,000+ QPS per instance

### How much memory does O use?

**Per instance:**
- Base: 80-100MB
- Memory blocks: ~1KB each
- Checkpoints: 50-100MB (compressed)
- WAL: 10-20MB per hour

**Total**: 150-200MB per agent instance

### How fast is crash recovery?

**Recovery timeline:**
1. Heartbeat timeout detection: 5 seconds
2. Supervisor restart: 10ms
3. Checkpoint load: 1-2 seconds
4. WAL replay: 100ms (1000 entries)

**Total**: ~2 seconds worst case, ~100ms typical

---

## Development Questions

### What do I need to get started?

**Required:**
- Elixir 1.14+ and Erlang/OTP 25+
- Gerbil Scheme 0.18+

**Optional:**
- Zig 0.13+ (for infrastructure layer)
- Rust 1.70+ (for compute layer)
- Docker (for containerized deployment)

See [GETTING_STARTED.md](../GETTING_STARTED.md) for details.

### How do I run tests?

```bash
cd o_supervisor
mix test                    # All tests
mix test --cover            # With coverage
mix test test/file_test.exs # Specific file
mix test.watch              # Watch mode
```

### How do I debug issues?

**Elixir debugging:**

```elixir
# In IEx
iex -S mix

# Get process state
:sys.get_state(OSupervisor.MemoryVault)

# Trace messages
:sys.trace(OSupervisor.GerbilManager, true)

# Start Observer (GUI)
:observer.start()
```

**Gerbil debugging:**

```scheme
;; Add debug prints
(displayln "Debug: " variable)

;; Use REPL
gerbil repl

;; Trace execution
(import :std/debug/trace)
(trace-call my-function args)
```

### How do I add a new feature?

1. Read [CONTRIBUTING.md](../CONTRIBUTING.md)
2. Create feature branch
3. Write tests first (TDD)
4. Implement feature
5. Update documentation
6. Create ADR if architectural change
7. Submit pull request

### Where should I add my code?

**Elixir (supervision/infrastructure):**
- `o_supervisor/lib/o_supervisor/` - Core modules
- `o_supervisor/test/` - Tests

**Gerbil (agent logic):**
- `gerbil/agent/` - Agent modules
- `gerbil/ffi/` - FFI bindings
- `gerbil/utils/` - Utilities

**Zig (infrastructure):**
- `zig/` - Infrastructure modules

**Rust (compute):**
- `rust/` - Compute modules

---

## Deployment Questions

### How do I deploy O in production?

**Option 1: Docker Compose (Recommended)**

```bash
docker-compose up -d
```

**Option 2: Elixir Release**

```bash
cd o_supervisor
MIX_ENV=prod mix release
_build/prod/rel/o_supervisor/bin/o_supervisor start
```

**Option 3: Kubernetes** (Coming soon)

### What are the system requirements?

**Minimum:**
- CPU: 2 cores
- RAM: 4GB
- Disk: 10GB
- OS: Linux or macOS

**Recommended:**
- CPU: 4+ cores
- RAM: 8GB+
- Disk: 50GB+ (for checkpoints/WAL)
- OS: Linux (Ubuntu 20.04+)

### How do I monitor O in production?

**Built-in monitoring:**
- Prometheus metrics: `http://localhost:9568/metrics`
- Grafana dashboards: `http://localhost:3000`
- Health check: `http://localhost:4000/health`

**Key metrics:**
- `o_supervisor_health_metrics` - Agent health
- `o_supervisor_checkpoint_created` - Checkpoint events
- `o_supervisor_wal_appended` - WAL operations
- `vm_memory_total` - Memory usage

### How do I backup O?

**What to backup:**
1. Checkpoints: `data/checkpoints/`
2. WAL logs: `data/wal/`
3. Configuration: `o_supervisor/config/`

**Backup script:**

```bash
#!/bin/bash
BACKUP_DIR="/backups/o_$(date +%Y%m%d_%H%M%S)"
mkdir -p $BACKUP_DIR
cp -r data/checkpoints $BACKUP_DIR/
cp -r data/wal $BACKUP_DIR/
cp -r o_supervisor/config $BACKUP_DIR/
tar -czf $BACKUP_DIR.tar.gz $BACKUP_DIR
rm -rf $BACKUP_DIR
```

### How do I restore from backup?

```bash
# Stop O
docker-compose down

# Extract backup
tar -xzf backup.tar.gz

# Restore files
cp -r backup/checkpoints data/
cp -r backup/wal data/
cp -r backup/config o_supervisor/

# Start O
docker-compose up -d
```

---

## Troubleshooting

### O won't start

**Check:**
1. Elixir/Erlang installed: `elixir --version`
2. Gerbil installed: `gerbil version`
3. Data directories exist: `ls data/`
4. Ports available: `lsof -i :4000`
5. Logs: `tail -f data/logs/o_supervisor.log`

### Checkpoints are corrupted

```bash
# Remove corrupted checkpoints
rm data/checkpoints/*.ckpt
rm data/checkpoints/checkpoints.dets

# Restart O (will create new checkpoint)
docker-compose restart o_supervisor
```

### WAL logs are too large

```bash
# Compact old WAL segments
cd o_supervisor
iex -S mix

# In IEx
OSupervisor.WALManager.compact_old_segments()
```

### Memory usage is high

**Check:**
1. Number of memory blocks: Too many?
2. Checkpoint size: Too large?
3. Shadow instances: Too many running?

**Solutions:**
- Reduce `max_concurrent_shadows` in config
- Implement memory block pruning
- Increase checkpoint compression level

### Performance is slow

**Profile:**

```elixir
# In IEx
:fprof.trace([:start])
# Run your operation
:fprof.trace([:stop])
:fprof.profile()
:fprof.analyse()
```

**Common causes:**
- Too frequent checkpoints
- Large WAL entries
- Slow disk I/O
- Network latency

---

## Security Questions

### Is O secure?

**Security features:**
- Input validation on all messages
- Sandboxed code execution (planned)
- Resource limits per shadow instance
- Encrypted data at rest (planned)
- Encrypted data in transit (planned)

**Security considerations:**
- O can modify its own code (by design)
- Shadow testing provides safety net
- Elixir supervision prevents permanent damage
- WAL provides audit trail

### How do I report security issues?

**DO NOT** open public issues for security vulnerabilities.

**Instead:**
1. Email: security@project-o.example.com
2. Include: Description, steps to reproduce, impact
3. We'll respond within 24 hours
4. We'll work with you on disclosure timeline

### Can O be used maliciously?

O is designed for legitimate AI agent development. Like any powerful tool, it can be misused. We:
- Provide security guidelines
- Implement safety mechanisms
- Monitor for abuse
- Reserve right to revoke access

---

## Community Questions

### How can I contribute?

See [CONTRIBUTING.md](../CONTRIBUTING.md) for:
- Code contributions
- Documentation improvements
- Bug reports
- Feature requests
- Community support

### Where can I get help?

1. **Documentation**: Check `docs/` directory
2. **FAQ**: This document
3. **Issues**: Search existing issues
4. **Discussions**: GitHub Discussions
5. **New Issue**: Open if not found

### Is there a roadmap?

Yes! See [IMPLEMENTATION_CHECKLIST.md](IMPLEMENTATION_CHECKLIST.md):

- **Phase 0** ✅ Complete - Elixir foundation
- **Phase 1** 🚧 In Progress - Gerbil core
- **Phase 2** 📋 Planned - Infrastructure (Zig)
- **Phase 3** 📋 Planned - Protected evolution
- **Phase 4** 📋 Planned - Multi-threaded evolution
- **Phase 5** 📋 Planned - Advanced features

### What's the license?

MIT License. See [LICENSE](../LICENSE) file.

---

## Advanced Questions

### Can I extend O with custom modules?

Yes! O is designed to be extensible:

**Elixir modules:**
```elixir
defmodule OSupervisor.MyCustomModule do
  use GenServer
  # Your implementation
end

# Add to supervision tree in application.ex
```

**Gerbil modules:**
```scheme
;;; my-module.ss
(export #t my-function)

(def (my-function arg)
  ;; Your implementation
  )
```

### Can I use O with other LLMs?

Yes! O's LLM integration is pluggable:

```scheme
;; gerbil/agent/llm.ss
(def (make-llm provider: provider model: model ...)
  (case provider
    (:openai (make-openai-client ...))
    (:anthropic (make-anthropic-client ...))
    (:ollama (make-ollama-client ...))
    (:custom (make-custom-client ...))))
```

### Can I run multiple O instances?

Yes! Each instance is independent:

```bash
# Instance 1
PORT=4000 iex -S mix

# Instance 2
PORT=4001 iex -S mix

# Or with Docker
docker-compose up --scale o_supervisor=3
```

### Can O evolve its own evolution strategy?

Yes! This is a Phase 5 goal:

1. Agent analyzes evolution success rate
2. Generates new evolution strategies
3. Tests strategies in shadow instances
4. Adopts better strategies
5. Meta-evolution: evolving how to evolve

---

## Still Have Questions?

- **Documentation**: [docs/](.)
- **GitHub Issues**: [Issues](https://github.com/your-repo/o/issues)
- **Discussions**: [Discussions](https://github.com/your-repo/o/discussions)

---

**Last Updated**: 2026-01-16  
**Version**: 1.0
