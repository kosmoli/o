# ADR-001: Introduce Elixir/OTP as Supervision Layer

## Status

**Accepted** - 2026-01-16

## Context

The original O architecture (V1) allowed the Gerbil agent to modify its own code at runtime through Lisp metaprogramming. However, this created a critical vulnerability: the agent could write buggy code that corrupts its memory system, leading to permanent system failure without recovery capability.

### The Self-Destruction Problem

```
Scenario: Agent Evolution Gone Wrong
1. Agent detects memory system is slow
2. Agent generates "optimized" memory code
3. Agent hot-loads new memory module
4. ❌ BUG: New code has infinite recursion
5. 💥 Memory system crashes
6. 🧠 Agent loses all memory (amnesia)
7. 🔄 Cannot rollback (rollback system needs memory)
8. ☠️ System permanently dead
```

**The Paradox**: The agent needs memory to rollback, but the memory system is what failed.

### Why Traditional Solutions Don't Work

| Approach | Problem |
|----------|---------|
| Git rollback | Requires the agent to remember the rollback command |
| Backup files | Requires the agent to know where backups are |
| Validation before load | Cannot catch all runtime bugs (race conditions, logic errors) |
| Sandboxing | Cannot prevent logic errors that corrupt state |

## Decision

We will introduce **Elixir/OTP** as an external supervision layer that manages the Gerbil agent's lifecycle, state persistence, and evolution safety.

### Architecture

```
┌──────────────────────────────────────────────────────────┐
│  Elixir Supervisor (The Immortal Guardian)               │
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
```

### Key Components

1. **GerbilManager**: Manages Gerbil process via Erlang Port
2. **MemoryVault**: Persistent state storage (DETS + file backup)
3. **WALManager**: Write-Ahead Log for operation durability
4. **HealthMonitor**: Heartbeat monitoring and metrics collection
5. **EvolutionArbiter**: Shadow testing and evolution orchestration
6. **TrafficSplitter**: A/B testing for shadow instances

## Consequences

### Positive

1. **Prevents Permanent Failure**
   - Elixir detects crashes within milliseconds
   - Automatic restart with last checkpoint
   - Total downtime: ~50ms vs ∞ (manual intervention)

2. **Enables Fearless Evolution**
   - Agent can try radical mutations without fear
   - Shadow testing validates changes before applying
   - Automatic rollback on failure

3. **Unlocks Parallel Evolution**
   - Spawn 50+ shadow instances concurrently
   - Genetic algorithm-based evolution
   - Adversarial evolution (Red Team vs Blue Team)

4. **Industrial-Grade Reliability**
   - OTP supervision trees (30+ years in telecom)
   - Battle-tested fault tolerance
   - Production-ready from day one

5. **State Durability**
   - Checkpoints + WAL ensure no data loss
   - Maximum data loss: 1 second (WAL interval)
   - Multiple backup layers (DETS + file + WAL)

### Negative

1. **Performance Overhead**
   - Request latency: +10% (10ms → 11ms)
   - Throughput: -10% (10K QPS → 9K QPS)
   - Memory: +25% (80MB → 100MB per instance)

2. **Increased Complexity**
   - Two languages to maintain (Elixir + Gerbil)
   - Port communication adds complexity
   - More moving parts to debug

3. **Learning Curve**
   - Team needs Elixir/OTP expertise
   - Understanding BEAM VM concepts
   - Debugging across language boundaries

4. **Deployment Complexity**
   - Need to deploy both Elixir and Gerbil
   - More configuration to manage
   - Docker images are larger

### Mitigation Strategies

**For Performance Overhead:**
- Use shared memory for hot path data (metrics, indexes)
- Batch WAL writes (100 entries or 1 second)
- Async checkpoints in background thread
- COW (copy-on-write) for shadow instances

**For Complexity:**
- Comprehensive documentation
- Clear separation of concerns
- Well-defined communication protocol
- Extensive integration tests

**For Learning Curve:**
- Detailed tutorials and examples
- Architecture decision records (ADRs)
- Code comments and inline documentation
- Pair programming sessions

## Alternatives Considered

### Alternative 1: Pure Gerbil with Git-based Rollback

**Approach**: Use Git commits for every change, rollback via `git reset`

**Rejected because**:
- Requires agent to remember Git commands after crash
- Cannot rollback if memory system is corrupted
- No protection against infinite loops or deadlocks
- Manual intervention still required

### Alternative 2: Racket with Custodians

**Approach**: Use Racket's custodian system for resource management

**Rejected because**:
- Custodians don't provide external supervision
- Still vulnerable to memory corruption
- Racket's performance is lower than Gerbil
- No built-in distributed systems support

### Alternative 3: Rust with Process Isolation

**Approach**: Write supervisor in Rust, use OS processes for isolation

**Rejected because**:
- Rust lacks lightweight process model (need OS threads)
- No built-in supervision trees
- More complex to implement than OTP
- Rust's learning curve is steeper than Elixir

### Alternative 4: Kubernetes with Health Checks

**Approach**: Let Kubernetes restart pods on failure

**Rejected because**:
- Restart time is too slow (seconds vs milliseconds)
- No state preservation mechanism
- Overkill for single-machine deployment
- Doesn't solve the memory corruption problem

## Implementation Plan

See `IMPLEMENTATION_CHECKLIST.md` for detailed steps.

**Phase 0** (2 weeks): Elixir foundation
**Phase 1** (4 weeks): Gerbil core with Elixir bridge
**Phase 2** (4 weeks): Infrastructure (Zig/Rust)
**Phase 3** (4 weeks): Protected evolution
**Phase 4** (4 weeks): Multi-threaded evolution

## Success Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| Crash recovery time | < 100ms | Time from crash to resumed operation |
| Data loss on crash | < 1 second | WAL replay coverage |
| Evolution success rate | > 80% | Successful promotions / total attempts |
| System uptime | > 99.9% | With automatic recovery |
| Performance overhead | < 15% | Latency increase vs V1 |

## References

- [ARCHITECTURE_V2.md](../ARCHITECTURE_V2.md)
- [ELIXIR_INTEGRATION.md](../ELIXIR_INTEGRATION.md)
- [Erlang/OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
- [The Zen of Erlang](https://ferd.ca/the-zen-of-erlang.html)

## Approval

- **Proposed by**: Architecture Team
- **Reviewed by**: Technical Lead
- **Approved by**: Project Owner
- **Date**: 2026-01-16

---

**Next ADR**: [002-communication-protocol.md](002-communication-protocol.md)
