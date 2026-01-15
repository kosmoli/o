# Message Schema Definition

This document defines the message schema for communication between Elixir and Gerbil.

---

## Message Format

All messages use MessagePack serialization with a 4-byte length prefix:

```
┌──────────────────────────────────────────────────────┐
│ Length (4 bytes, big-endian) │ MessagePack Payload  │
└──────────────────────────────────────────────────────┘
```

## Base Message Structure

Every message must include these fields:

```json
{
  "type": "string",        // Message type identifier
  "timestamp": 1234567890, // Unix timestamp (seconds)
  "data": {},              // Message-specific data
  "metadata": {}           // Optional metadata
}
```

---

## Messages: Gerbil → Elixir

### 1. Heartbeat

**Purpose**: Indicate liveness to Elixir supervisor

**Frequency**: Every 1 second

**Schema**:
```json
{
  "type": "heartbeat",
  "timestamp": 1234567890
}
```

**Example**:
```elixir
%{
  "type" => "heartbeat",
  "timestamp" => 1705392000
}
```

---

### 2. Checkpoint Request

**Purpose**: Request state persistence

**Trigger**: Before risky operations, periodic (5 min), or on-demand

**Schema**:
```json
{
  "type": "checkpoint",
  "timestamp": 1234567890,
  "data": {
    "agent_state": {
      "id": "string",
      "name": "string",
      "version": "string",
      "status": "idle|thinking|acting|evolving"
    },
    "memory_blocks": [
      {
        "id": "string",
        "text": "string",
        "embedding": [0.1, 0.2, ...],
        "metadata": {},
        "importance": 0.5,
        "access_count": 10,
        "created": 1234567890
      }
    ],
    "tool_registry": {
      "tools": [
        {
          "name": "string",
          "params": ["param1", "param2"],
          "doc": "string",
          "unsafe": false
        }
      ]
    },
    "conversation_history": [
      {
        "role": "user|assistant",
        "content": "string",
        "timestamp": 1234567890
      }
    ],
    "metrics": {
      "total_requests": 1000,
      "avg_latency": 10.5,
      "error_count": 5
    }
  }
}
```

---

### 3. WAL Entry

**Purpose**: Log operation before execution

**Frequency**: Before every critical operation

**Schema**:
```json
{
  "type": "wal_entry",
  "timestamp": 1234567890,
  "data": {
    "operation": "memory_add|tool_register|state_update",
    "params": {},
    "sequence": 12345
  }
}
```

**Examples**:

```json
// Memory add
{
  "type": "wal_entry",
  "timestamp": 1705392000,
  "data": {
    "operation": "memory_add",
    "params": {
      "content": "User asked about weather",
      "metadata": {"source": "conversation"},
      "importance": 0.7
    },
    "sequence": 12345
  }
}

// Tool register
{
  "type": "wal_entry",
  "timestamp": 1705392000,
  "data": {
    "operation": "tool_register",
    "params": {
      "name": "calculator",
      "params": ["expression"],
      "doc": "Evaluate mathematical expressions"
    },
    "sequence": 12346
  }
}

// State update
{
  "type": "wal_entry",
  "timestamp": 1705392000,
  "data": {
    "operation": "state_update",
    "params": {
      "status": "thinking",
      "context": {"current_task": "analyze_data"}
    },
    "sequence": 12347
  }
}
```

---

### 4. WAL Batch

**Purpose**: Send multiple WAL entries at once

**Frequency**: When buffer reaches 100 entries or 1 second

**Schema**:
```json
{
  "type": "wal_batch",
  "timestamp": 1234567890,
  "data": [
    {
      "operation": "memory_add",
      "params": {},
      "sequence": 12345
    },
    {
      "operation": "tool_register",
      "params": {},
      "sequence": 12346
    }
  ]
}
```

---

### 5. Evolution Intent

**Purpose**: Notify Elixir of planned evolution

**Trigger**: When agent decides to evolve

**Schema**:
```json
{
  "type": "evolution_intent",
  "timestamp": 1234567890,
  "hypothesis": {
    "reason": "string",
    "target": "memory_system|tool_framework|dsl|...",
    "new_code": "string",
    "expected_improvement": {
      "latency": -0.15,
      "memory": 0.05,
      "accuracy": 0.10
    }
  }
}
```

**Example**:
```json
{
  "type": "evolution_intent",
  "timestamp": 1705392000,
  "hypothesis": {
    "reason": "Optimize memory search with SIMD",
    "target": "memory_system",
    "new_code": "(def (memory-search ...) ...)",
    "expected_improvement": {
      "latency": -0.20,
      "memory": 0.0,
      "accuracy": 0.0
    }
  }
}
```

---

### 6. Metrics Report

**Purpose**: Send performance metrics to Elixir

**Frequency**: Every 10 seconds or on-demand

**Schema**:
```json
{
  "type": "metrics",
  "timestamp": 1234567890,
  "data": {
    "avg_latency": 10.5,
    "p50_latency": 8.0,
    "p95_latency": 25.0,
    "p99_latency": 50.0,
    "error_rate": 0.01,
    "memory_usage": 80000000,
    "throughput": 1000,
    "active_tools": 5,
    "memory_blocks": 1000
  }
}
```

---

## Messages: Elixir → Gerbil

### 1. Checkpoint Acknowledgment

**Purpose**: Confirm checkpoint was saved

**Schema**:
```json
{
  "type": "checkpoint_ack",
  "timestamp": 1234567890,
  "checkpoint_id": "uuid-string",
  "size": 1024000
}
```

**Example**:
```json
{
  "type": "checkpoint_ack",
  "timestamp": 1705392000,
  "checkpoint_id": "550e8400-e29b-41d4-a716-446655440000",
  "size": 1024000
}
```

---

### 2. Restore Command

**Purpose**: Restore agent from checkpoint

**Trigger**: After crash or manual restore

**Schema**:
```json
{
  "type": "restore",
  "timestamp": 1234567890,
  "checkpoint_id": "uuid-string",
  "snapshot": {
    // Same structure as checkpoint data
  },
  "wal_entries": [
    {
      "operation": "memory_add",
      "params": {},
      "sequence": 12345
    }
  ]
}
```

---

### 3. Hot Reload

**Purpose**: Load new code into running agent

**Trigger**: After successful shadow test

**Schema**:
```json
{
  "type": "hot_reload",
  "timestamp": 1234567890,
  "code": "string",
  "module": "memory|tools|dsl|...",
  "backup_checkpoint_id": "uuid-string"
}
```

**Example**:
```json
{
  "type": "hot_reload",
  "timestamp": 1705392000,
  "code": "(def (memory-search query) ...)",
  "module": "memory",
  "backup_checkpoint_id": "550e8400-e29b-41d4-a716-446655440000"
}
```

---

### 4. Evolution Result

**Purpose**: Inform agent of evolution outcome

**Trigger**: After shadow test evaluation

**Schema**:
```json
{
  "type": "evolution_result",
  "timestamp": 1234567890,
  "success": true,
  "decision": "promoted|rejected",
  "metrics": {
    "latency_improvement": -0.15,
    "error_rate_change": -0.05,
    "memory_increase": 0.10
  },
  "reason": "string"
}
```

**Examples**:

```json
// Success
{
  "type": "evolution_result",
  "timestamp": 1705392000,
  "success": true,
  "decision": "promoted",
  "metrics": {
    "latency_improvement": -0.15,
    "error_rate_change": -0.05,
    "memory_increase": 0.10
  },
  "reason": "Shadow instance showed 15% latency improvement"
}

// Failure
{
  "type": "evolution_result",
  "timestamp": 1705392000,
  "success": false,
  "decision": "rejected",
  "metrics": {
    "latency_improvement": 0.20,
    "error_rate_change": 0.15,
    "memory_increase": 0.50
  },
  "reason": "Shadow instance had 20% higher latency and 15% more errors"
}
```

---

### 5. Process Request

**Purpose**: Forward user request to agent

**Trigger**: External API call or traffic splitter

**Schema**:
```json
{
  "type": "process_request",
  "timestamp": 1234567890,
  "request_id": "uuid-string",
  "data": {
    "message": "string",
    "context": {},
    "user_id": "string"
  }
}
```

---

### 6. Shutdown Command

**Purpose**: Graceful shutdown

**Schema**:
```json
{
  "type": "shutdown",
  "timestamp": 1234567890,
  "reason": "string",
  "create_checkpoint": true
}
```

---

## Error Messages

### From Gerbil

```json
{
  "type": "error",
  "timestamp": 1234567890,
  "error": {
    "code": "MEMORY_FULL|TOOL_FAILED|EVAL_ERROR|...",
    "message": "string",
    "stack_trace": "string",
    "context": {}
  }
}
```

### From Elixir

```json
{
  "type": "error",
  "timestamp": 1234567890,
  "error": {
    "code": "CHECKPOINT_FAILED|WAL_FULL|INVALID_MESSAGE|...",
    "message": "string",
    "details": {}
  }
}
```

---

## Message Validation

### Required Fields

All messages MUST have:
- `type` (string)
- `timestamp` (integer)

### Type Validation

```elixir
# Elixir side
defmodule OSupervisor.MessageValidator do
  @valid_types_from_gerbil [
    "heartbeat",
    "checkpoint",
    "wal_entry",
    "wal_batch",
    "evolution_intent",
    "metrics",
    "error"
  ]
  
  @valid_types_from_elixir [
    "checkpoint_ack",
    "restore",
    "hot_reload",
    "evolution_result",
    "process_request",
    "shutdown",
    "error"
  ]
  
  def validate_from_gerbil(message) do
    with {:ok, type} <- Map.fetch(message, "type"),
         true <- type in @valid_types_from_gerbil,
         {:ok, _timestamp} <- Map.fetch(message, "timestamp") do
      :ok
    else
      _ -> {:error, :invalid_message}
    end
  end
end
```

---

## Versioning

Messages include an optional `version` field for schema evolution:

```json
{
  "version": 2,
  "type": "checkpoint",
  "timestamp": 1234567890,
  "data": {}
}
```

**Version History**:
- **v1**: Initial schema
- **v2**: Added `metadata` field, compression support

---

## Performance Considerations

### Message Size Limits

| Message Type | Max Size | Typical Size |
|--------------|----------|--------------|
| Heartbeat | 100 bytes | 50 bytes |
| WAL Entry | 10 KB | 1 KB |
| Checkpoint | 100 MB | 10 MB |
| Metrics | 10 KB | 2 KB |
| Evolution Intent | 1 MB | 100 KB |

### Compression

Large messages (> 1MB) should be compressed:

```json
{
  "type": "checkpoint",
  "timestamp": 1234567890,
  "compressed": true,
  "compression": "zstd",
  "data": "base64_encoded_compressed_data"
}
```

---

## Testing

### Example Test Cases

```elixir
# test/message_schema_test.exs

defmodule MessageSchemaTest do
  use ExUnit.Case
  
  test "valid heartbeat message" do
    message = %{
      "type" => "heartbeat",
      "timestamp" => 1705392000
    }
    
    assert :ok = OSupervisor.MessageValidator.validate_from_gerbil(message)
  end
  
  test "invalid message missing timestamp" do
    message = %{"type" => "heartbeat"}
    
    assert {:error, :invalid_message} = 
      OSupervisor.MessageValidator.validate_from_gerbil(message)
  end
  
  test "checkpoint round-trip" do
    checkpoint = %{
      "type" => "checkpoint",
      "timestamp" => 1705392000,
      "data" => %{"agent_state" => %{}}
    }
    
    # Encode
    packed = Msgpax.pack!(checkpoint)
    
    # Decode
    {:ok, decoded} = Msgpax.unpack(packed)
    
    assert decoded["type"] == "checkpoint"
  end
end
```

---

**Version**: 1.0  
**Last Updated**: 2026-01-16  
**Status**: Approved
