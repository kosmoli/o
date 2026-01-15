# ADR-003: Checkpoint and WAL Strategy for State Durability

## Status

**Accepted** - 2026-01-16

## Context

The agent's state must survive crashes and be recoverable within milliseconds. We need a strategy that balances:

1. **Durability**: No data loss on crash
2. **Performance**: Minimal overhead on normal operations
3. **Recovery Speed**: Fast restoration after crash
4. **Storage Efficiency**: Reasonable disk usage

### State Characteristics

- **Size**: 50MB - 500MB per agent instance
- **Update Frequency**: 100-1000 operations/second
- **Critical Data**: Memory blocks, tool registry, conversation history
- **Recovery Time Target**: < 100ms

## Decision

We will use a **hybrid checkpoint + WAL (Write-Ahead Log)** strategy:

1. **Periodic Checkpoints**: Full state snapshots every 5 minutes or on-demand
2. **Continuous WAL**: Log every critical operation before execution
3. **Recovery**: Last checkpoint + WAL replay

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    State Durability                      │
└─────────────────────────────────────────────────────────┘

Timeline:
├─ Checkpoint 1 (t=0)
│  └─ WAL entries: [1, 2, 3, 4, 5]
├─ Checkpoint 2 (t=5min)
│  └─ WAL entries: [6, 7, 8, 9]
├─ CRASH (t=7min)
│
└─ Recovery:
   1. Load Checkpoint 2
   2. Replay WAL entries [6, 7, 8, 9]
   3. Resume operation

Maximum data loss: Time since last WAL flush (< 1 second)
```

### Checkpoint Format

```
Checkpoint File Structure:
┌────────────────────────────────────────────────────────┐
│ Header (256 bytes)                                     │
│  - Magic: "O_CKPT_V2"                                  │
│  - Version: 2                                          │
│  - Timestamp: 1234567890                               │
│  - Checkpoint ID: UUID                                 │
│  - Compressed size: N bytes                            │
│  - Uncompressed size: M bytes                          │
│  - SHA256 checksum                                     │
│  - WAL sequence number                                 │
├────────────────────────────────────────────────────────┤
│ Compressed Data (zstd level 9)                         │
│  - Agent state (serialized Gerbil structures)          │
│  - Memory blocks                                       │
│  - Tool registry                                       │
│  - LLM conversation history                            │
│  - Metrics                                             │
└────────────────────────────────────────────────────────┘
```

### WAL Format

```
WAL Entry:
{
  "sequence": 12345,
  "timestamp": 1234567890,
  "operation": "memory_add" | "tool_register" | "state_update",
  "data": { ... },
  "checksum": "sha256_hash"
}

WAL Segment File:
[Entry 1][Entry 2][Entry 3]...[Entry N]

Each entry: [size (4 bytes)][serialized entry]
```

## Consequences

### Positive

1. **Zero Data Loss**
   - WAL ensures all operations are logged
   - Maximum loss: 1 second (WAL flush interval)
   - Multiple redundancy layers

2. **Fast Recovery**
   - Load checkpoint: ~1 second
   - Replay WAL: ~100ms for 1000 entries
   - Total recovery: < 2 seconds

3. **Efficient Storage**
   - Compression reduces size by 70-80%
   - Old checkpoints can be deleted
   - WAL segments can be compacted

4. **Incremental Backup**
   - Only need to backup new WAL segments
   - Checkpoints provide restore points
   - Easy to implement point-in-time recovery

### Negative

1. **Write Amplification**
   - Every operation written twice (WAL + memory)
   - Checkpoint creation is I/O intensive
   - Disk writes increase by ~50%

2. **Storage Overhead**
   - Need to keep multiple checkpoints
   - WAL segments accumulate
   - Typical overhead: 2-3x state size

3. **Complexity**
   - WAL replay logic is complex
   - Need to handle corrupted entries
   - Checkpoint/WAL coordination

### Mitigation Strategies

**For Write Amplification:**
- Batch WAL writes (100 entries or 1 second)
- Async checkpoint creation
- Use SSD for WAL directory

**For Storage Overhead:**
- Delete checkpoints older than 24 hours
- Compact WAL segments after checkpoint
- Compress old WAL segments

**For Complexity:**
- Extensive testing of replay logic
- Checksums for corruption detection
- Clear error messages and recovery procedures

## Checkpoint Strategy Details

### When to Create Checkpoints

1. **Periodic**: Every 5 minutes (configurable)
2. **On-Demand**: Before risky operations (evolution)
3. **On Shutdown**: Graceful shutdown
4. **Size-Based**: When WAL exceeds 10,000 entries

### Checkpoint Creation Process

```elixir
# Elixir side
def create_checkpoint(agent_state) do
  checkpoint_id = UUID.uuid4()
  
  # 1. Serialize state
  serialized = :erlang.term_to_binary(agent_state)
  
  # 2. Compress
  compressed = :zstd.compress(serialized, level: 9)
  
  # 3. Calculate checksum
  checksum = :crypto.hash(:sha256, compressed)
  
  # 4. Write to DETS
  :dets.insert(:checkpoints, {checkpoint_id, compressed, checksum})
  
  # 5. Write to file (backup)
  File.write!("checkpoints/#{checkpoint_id}.ckpt", compressed)
  
  # 6. Record metadata
  metadata = %{
    id: checkpoint_id,
    timestamp: System.system_time(:second),
    size: byte_size(compressed),
    wal_sequence: get_current_wal_sequence()
  }
  
  {:ok, checkpoint_id, metadata}
end
```

### Checkpoint Restoration Process

```elixir
def restore_checkpoint(checkpoint_id) do
  # 1. Load from DETS
  [{^checkpoint_id, compressed, checksum}] = :dets.lookup(:checkpoints, checkpoint_id)
  
  # 2. Verify checksum
  ^checksum = :crypto.hash(:sha256, compressed)
  
  # 3. Decompress
  serialized = :zstd.decompress(compressed)
  
  # 4. Deserialize
  agent_state = :erlang.binary_to_term(serialized)
  
  # 5. Get WAL entries after checkpoint
  wal_entries = WALManager.get_entries_after(agent_state.wal_sequence)
  
  {:ok, agent_state, wal_entries}
end
```

## WAL Strategy Details

### WAL Operations

```scheme
;; Gerbil side - Log before operation
(def (memory-add! memory content)
  ;; 1. Log to WAL
  (wal-log! 'memory-add (hash 'content content))
  
  ;; 2. Execute operation
  (let ((blocks (split-into-blocks content)))
    (add-blocks-to-memory! memory blocks)))

;; WAL logging
(def (wal-log! operation data)
  (let ((entry (make-wal-entry
                op: operation
                data: data
                timestamp: (current-time)
                sequence: (next-sequence-number))))
    ;; Buffer locally
    (queue-push! *wal-buffer* entry)
    
    ;; Send to Elixir
    (elixir-send "wal_entry" entry)
    
    ;; Flush if buffer is full
    (when (> (queue-length *wal-buffer*) 100)
      (flush-wal-buffer!))))
```

### WAL Replay

```scheme
;; Replay WAL entries after checkpoint restore
(def (wal-replay! agent wal-entries)
  (for-each
   (lambda (entry)
     (match (wal-entry-op entry)
       ('memory-add
        (memory-add! (agent-memory agent)
                     (hash-ref (wal-entry-data entry) 'content)))
       
       ('tool-register
        (register-tool! (agent-tools agent)
                        (hash-ref (wal-entry-data entry) 'tool)))
       
       ('state-update
        (update-agent-state! agent
                             (hash-ref (wal-entry-data entry) 'new-state)))
       
       (else
        (log-warning "Unknown WAL operation" op: (wal-entry-op entry)))))
   wal-entries))
```

### WAL Segment Management

```elixir
# Rotate WAL segment when it reaches 10,000 entries
def maybe_rotate_segment(state) do
  if state.entry_count >= 10_000 do
    File.close(state.file)
    new_segment = state.current_segment + 1
    {:ok, new_file} = File.open(wal_path(new_segment), [:append, :binary])
    
    # Compact old segment
    Task.start(fn -> compact_segment(state.current_segment) end)
    
    %{state | current_segment: new_segment, file: new_file, entry_count: 0}
  else
    state
  end
end

# Compact WAL segment (remove entries before last checkpoint)
def compact_segment(segment_num) do
  last_checkpoint_seq = get_last_checkpoint_sequence()
  
  entries = read_segment(segment_num)
  |> Enum.filter(fn entry -> entry.sequence > last_checkpoint_seq end)
  
  if length(entries) < 1000 do
    # Delete segment if almost empty
    File.rm!(wal_path(segment_num))
  else
    # Rewrite with filtered entries
    write_segment(segment_num, entries)
  end
end
```

## Performance Characteristics

| Operation | Latency | Throughput |
|-----------|---------|------------|
| WAL append (single) | 50μs | 20K ops/sec |
| WAL append (batch 100) | 500μs | 200K ops/sec |
| Checkpoint create | 1-2s | N/A |
| Checkpoint restore | 1-2s | N/A |
| WAL replay (1000 entries) | 100ms | N/A |

## Storage Requirements

| Component | Size | Retention |
|-----------|------|-----------|
| Single checkpoint | 50-100MB (compressed) | 24 hours |
| WAL per hour | 10-20MB | Until next checkpoint |
| Total storage | 500MB - 1GB | Rolling window |

## Disaster Recovery Scenarios

### Scenario 1: Gerbil Crash

```
1. Elixir detects heartbeat timeout (5 seconds)
2. Supervisor restarts GerbilManager
3. GerbilManager starts new Gerbil process with --restore flag
4. Gerbil loads last checkpoint from Elixir
5. Gerbil replays WAL entries
6. Agent resumes operation
Total downtime: ~2 seconds
```

### Scenario 2: Elixir Crash

```
1. Supervisor restarts Elixir application
2. MemoryVault opens DETS file
3. WALManager opens WAL segments
4. GerbilManager starts Gerbil with last checkpoint
5. System resumes
Total downtime: ~5 seconds
```

### Scenario 3: Disk Corruption

```
1. Checksum verification fails
2. Try previous checkpoint
3. If all checksums fail, start from empty state
4. Log critical error
5. Alert operator
```

### Scenario 4: Complete System Failure

```
1. Restore from backup (S3, NFS, etc.)
2. Load last checkpoint
3. Replay WAL from backup
4. Resume operation
Recovery time: Depends on backup location
```

## Testing Strategy

1. **Unit Tests**
   - Checkpoint serialization/deserialization
   - WAL entry encoding/decoding
   - Checksum verification

2. **Integration Tests**
   - Full checkpoint/restore cycle
   - WAL replay correctness
   - Concurrent checkpoint + WAL writes

3. **Chaos Tests**
   - Kill Gerbil during checkpoint
   - Corrupt checkpoint file
   - Corrupt WAL entries
   - Disk full scenarios

4. **Performance Tests**
   - Checkpoint creation time
   - WAL append throughput
   - Recovery time benchmarks

## Monitoring

```elixir
# Metrics to track
:telemetry.execute([:o_supervisor, :checkpoint, :created], %{
  duration_ms: duration,
  size_bytes: size,
  compression_ratio: ratio
})

:telemetry.execute([:o_supervisor, :wal, :appended], %{
  entry_count: count,
  batch_size: size
})

:telemetry.execute([:o_supervisor, :recovery, :completed], %{
  duration_ms: duration,
  wal_entries_replayed: count
})
```

## References

- [Write-Ahead Logging (PostgreSQL)](https://www.postgresql.org/docs/current/wal-intro.html)
- [Checkpoint and Recovery (SQLite)](https://www.sqlite.org/wal.html)
- [DETS (Erlang)](https://www.erlang.org/doc/man/dets.html)

## Approval

- **Proposed by**: Architecture Team
- **Reviewed by**: Technical Lead
- **Approved by**: Project Owner
- **Date**: 2026-01-16

---

**Previous ADR**: [002-communication-protocol.md](002-communication-protocol.md)  
**Next ADR**: [004-shadow-testing-strategy.md](004-shadow-testing-strategy.md)
