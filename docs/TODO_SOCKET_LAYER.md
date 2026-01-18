# TODO: Socket/Msgpack Layer Implementation

## Overview
This document tracks the future implementation of the complete socket/msgpack layer for communication between Gerbil agents and the Elixir supervision layer.

## Current State
- **Status**: Placeholder/stub implementation
- **Location**: `o/gerbil/database/client.ss`
- **Reason**: Gerbil syntax fixes took priority; stub functions allow testing core logic

## Implementation Requirements

### 1. MessagePack Encoding/Decoding
**File**: `o/gerbil/std/net/msgpack.ss`

Need to implement:
- `msgpack-pack` - Encode Scheme values to MessagePack binary format
- `msgpack-unpack` - Decode MessagePack binary to Scheme values
- Support for: nil, boolean, integers, floats, strings, arrays, maps

### 2. Socket Communication
**Files**:
- `o/gerbil/database/client.ss`
- `o/gerbil/agent/elixir-bridge.ss`

Need to implement:
- `socket-connect` - Connect to Elixir service
- `socket-send` - Send binary data
- `socket-recv` - Receive binary data
- Connection pooling/reconnection logic
- Error handling and retry logic

### 3. Protocol Implementation
**Request Format**:
```scheme
{
  "type": "db_request",
  "request_id": 123,
  "operation": "create_agent",
  "params": { ... }
}
```

**Response Format**:
```scheme
{
  "type": "db_response",
  "request_id": 123,
  "success": true,
  "data": { ... }
}
```

### 4. Database Operations to Implement

| Function | Status | Notes |
|----------|--------|-------|
| `db-connect!` | Stub | Need real socket connection |
| `db-disconnect!` | Stub | Need proper cleanup |
| `db-create-agent` | Stub | |
| `db-get-agent` | Stub | |
| `db-update-agent` | Stub | |
| `db-delete-agent` | Stub | |
| `db-list-agents` | Stub | |
| `db-get-memory-blocks` | Stub | |
| `db-create-memory-block` | Stub | |
| `db-update-memory-block` | Stub | |
| `db-delete-memory-block` | Stub | |
| `db-get-messages` | Stub | |
| `db-create-message` | Stub | |
| `db-search-archival-memory` | Stub | |
| `db-get-conversation` | Stub | |
| `db-get-conversation-by-role` | Stub | |
| `db-get-agent-statistics` | Stub | |

### 5. Dependencies
- Gerbil's `:std/io/socket` module
- Custom MessagePack encoder/decoder
- Elixir backend must be running on configured port (default 9000)

### 6. Testing Strategy
1. Unit tests for MessagePack encoding/decoding
2. Mock Elixir server for integration testing
3. Real Elixir backend for end-to-end testing

## References
- MessagePack spec: https://msgpack.org/
- Gerbil socket docs: https://cons.io/gerbil/
- Elixir supervisor: `o/o_supervisor/`

## Priority
**Medium** - Core Gerbil code functionality is more important. The stub implementation allows:
- Testing Gerbil agent logic
- Verifying memory systems
- Running tool execution tests

This can be implemented once Gerbil code is fully working.
