# REST API Documentation

REST API for Project O agent management and messaging.

## Base URL

```
http://localhost:8283
```

## Authentication

Currently not implemented. Will be added in future versions.

## Content Type

All requests and responses use `application/json` content type.

## Error Responses

All errors follow this format:

```json
{
  "error": "Error message",
  "details": {
    // Optional additional error details
  }
}
```

Common HTTP status codes:
- `200 OK` - Request succeeded
- `201 Created` - Resource created successfully
- `400 Bad Request` - Invalid request data
- `401 Unauthorized` - Authentication required
- `404 Not Found` - Resource not found
- `405 Method Not Allowed` - HTTP method not allowed
- `429 Too Many Requests` - Rate limit exceeded
- `500 Internal Server Error` - Server error
- `501 Not Implemented` - Feature not yet implemented
- `503 Service Unavailable` - Service temporarily unavailable

## Endpoints

### Health & Status

#### GET /health

Basic health check endpoint.

**Response:**
```json
{
  "status": "ok",
  "timestamp": 1705401600,
  "uptime": 3600
}
```

#### GET /status

Detailed status information.

**Response:**
```json
{
  "status": "ok",
  "timestamp": 1705401600,
  "uptime": 3600,
  "version": "0.1.0",
  "environment": {
    "gerbil_version": "v0.18",
    "platform": "unix"
  },
  "system": {
    "memory_usage": "N/A",
    "cpu_usage": "N/A",
    "active_connections": 0
  }
}
```

#### GET /ready

Readiness check (for Kubernetes).

**Response:**
```json
{
  "ready": true
}
```

#### GET /alive

Liveness check (for Kubernetes).

**Response:**
```json
{
  "alive": true
}
```

#### GET /version

Version information.

**Response:**
```json
{
  "version": "0.1.0",
  "build_date": "2026-01-16",
  "git_commit": "unknown",
  "gerbil_version": "v0.18"
}
```

#### GET /metrics

Prometheus-compatible metrics.

**Response:** (text/plain)
```
# HELP server_uptime_seconds Server uptime in seconds
# TYPE server_uptime_seconds gauge
server_uptime_seconds 3600

# HELP http_requests_total Total HTTP requests
# TYPE http_requests_total counter
http_requests_total 1234
```

---

### Agent Management

#### POST /v1/agents

Create a new agent.

**Request Body:**
```json
{
  "name": "MyAgent",
  "llm_config": {
    "provider": "openai",
    "model": "gpt-4",
    "temperature": 0.7,
    "max_tokens": 4096
  },
  "system_prompt": "You are a helpful assistant.",
  "memory_config": {
    "core_memory_enabled": true,
    "archival_memory_enabled": true
  }
}
```

**Required Fields:**
- `name` (string, 1-100 chars)

**Optional Fields:**
- `llm_config` (object)
- `system_prompt` (string, max 10000 chars)
- `memory_config` (object)

**Response:** (201 Created)
```json
{
  "id": "agent-1705401600-123456",
  "name": "MyAgent",
  "llm_config": {
    "provider": "openai",
    "model": "gpt-4",
    "temperature": 0.7,
    "max_tokens": 4096
  },
  "system_prompt": "You are a helpful assistant.",
  "memory_config": {
    "core_memory_enabled": true,
    "archival_memory_enabled": true
  },
  "created_at": 1705401600,
  "updated_at": 1705401600
}
```

#### GET /v1/agents

List all agents.

**Query Parameters:**
- `limit` (number, default: 10) - Number of agents to return
- `offset` (number, default: 0) - Offset for pagination

**Response:**
```json
{
  "agents": [
    {
      "id": "agent-1",
      "name": "Agent 1",
      "created_at": 1705401600
    },
    {
      "id": "agent-2",
      "name": "Agent 2",
      "created_at": 1705401700
    }
  ],
  "total": 2,
  "limit": 10,
  "offset": 0
}
```

#### GET /v1/agents/:id

Get agent by ID.

**Path Parameters:**
- `id` (string) - Agent ID

**Response:**
```json
{
  "id": "agent-123",
  "name": "MyAgent",
  "llm_config": {
    "provider": "openai",
    "model": "gpt-4"
  },
  "system_prompt": "You are a helpful assistant.",
  "memory_config": {
    "core_memory_enabled": true,
    "archival_memory_enabled": true
  },
  "created_at": 1705401600,
  "updated_at": 1705401600
}
```

#### PATCH /v1/agents/:id

Update agent.

**Path Parameters:**
- `id` (string) - Agent ID

**Request Body:**
```json
{
  "name": "UpdatedAgent",
  "system_prompt": "You are a very helpful assistant.",
  "llm_config": {
    "temperature": 0.5
  }
}
```

**Response:**
```json
{
  "id": "agent-123",
  "name": "UpdatedAgent",
  "llm_config": {
    "temperature": 0.5
  },
  "system_prompt": "You are a very helpful assistant.",
  "memory_config": {},
  "updated_at": 1705401700
}
```

#### DELETE /v1/agents/:id

Delete agent.

**Path Parameters:**
- `id` (string) - Agent ID

**Response:**
```json
{
  "deleted": true,
  "id": "agent-123"
}
```

#### GET /v1/agents/:id/config

Get agent configuration.

**Path Parameters:**
- `id` (string) - Agent ID

**Response:**
```json
{
  "llm_config": {
    "provider": "openai",
    "model": "gpt-4",
    "temperature": 0.7,
    "max_tokens": 4096
  },
  "memory_config": {
    "core_memory_enabled": true,
    "archival_memory_enabled": true,
    "max_archival_entries": 10000
  },
  "tool_config": {
    "enabled_tools": ["send_message", "conversation_search"]
  }
}
```

#### PATCH /v1/agents/:id/config

Update agent configuration.

**Path Parameters:**
- `id` (string) - Agent ID

**Request Body:**
```json
{
  "llm_config": {
    "temperature": 0.8
  },
  "memory_config": {
    "max_archival_entries": 20000
  }
}
```

**Response:**
```json
{
  "updated": true,
  "config": {
    "llm_config": {
      "temperature": 0.8
    },
    "memory_config": {
      "max_archival_entries": 20000
    }
  }
}
```

#### GET /v1/agents/:id/memory

Get agent memory.

**Path Parameters:**
- `id` (string) - Agent ID

**Response:**
```json
{
  "core_memory": {
    "persona": "You are a helpful AI assistant.",
    "human": "User prefers concise responses."
  },
  "archival_memory_count": 0,
  "recall_memory_count": 0
}
```

#### PATCH /v1/agents/:id/memory

Update agent memory.

**Path Parameters:**
- `id` (string) - Agent ID

**Request Body:**
```json
{
  "core_memory": {
    "persona": "You are a very helpful AI assistant.",
    "human": "User prefers detailed explanations."
  }
}
```

**Response:**
```json
{
  "updated": true,
  "memory": {
    "core_memory": {
      "persona": "You are a very helpful AI assistant.",
      "human": "User prefers detailed explanations."
    }
  }
}
```

---

### Message Management

#### POST /v1/agents/:id/messages

Send message to agent.

**Path Parameters:**
- `id` (string) - Agent ID

**Request Body:**
```json
{
  "message": "Hello, how are you?",
  "stream": false
}
```

**Required Fields:**
- `message` (string, 1-100000 chars)

**Optional Fields:**
- `stream` (boolean, default: false) - Enable streaming response

**Response:** (201 Created)
```json
{
  "id": "msg-1705401600-123456",
  "agent_id": "agent-123",
  "messages": [
    {
      "role": "user",
      "content": "Hello, how are you?",
      "timestamp": 1705401600
    },
    {
      "role": "assistant",
      "content": "I'm doing well, thank you! How can I help you today?",
      "timestamp": 1705401601
    }
  ],
  "usage": {
    "prompt_tokens": 10,
    "completion_tokens": 15,
    "total_tokens": 25
  },
  "created_at": 1705401600
}
```

#### GET /v1/agents/:id/messages

Get agent messages.

**Path Parameters:**
- `id` (string) - Agent ID

**Query Parameters:**
- `limit` (number, default: 50) - Number of messages to return
- `offset` (number, default: 0) - Offset for pagination
- `role` (string, optional) - Filter by role (user/assistant/system/tool)

**Response:**
```json
{
  "messages": [
    {
      "id": "msg-1",
      "role": "user",
      "content": "Hello!",
      "timestamp": 1705401600
    },
    {
      "id": "msg-2",
      "role": "assistant",
      "content": "Hi there! How can I help you?",
      "timestamp": 1705401601
    }
  ],
  "total": 2,
  "limit": 50,
  "offset": 0
}
```

#### GET /v1/agents/:id/messages/:message_id

Get specific message.

**Path Parameters:**
- `id` (string) - Agent ID
- `message_id` (string) - Message ID

**Response:**
```json
{
  "id": "msg-123",
  "agent_id": "agent-123",
  "role": "user",
  "content": "Hello!",
  "timestamp": 1705401600
}
```

#### DELETE /v1/agents/:id/messages/:message_id

Delete message.

**Path Parameters:**
- `id` (string) - Agent ID
- `message_id` (string) - Message ID

**Response:**
```json
{
  "deleted": true,
  "id": "msg-123"
}
```

#### GET /v1/agents/:id/conversation

Get full conversation history.

**Path Parameters:**
- `id` (string) - Agent ID

**Query Parameters:**
- `limit` (number, default: 100) - Number of messages to return
- `offset` (number, default: 0) - Offset for pagination

**Response:**
```json
{
  "agent_id": "agent-123",
  "messages": [
    {
      "id": "msg-1",
      "role": "user",
      "content": "Hello!",
      "timestamp": 1705401500
    },
    {
      "id": "msg-2",
      "role": "assistant",
      "content": "Hi! How can I help?",
      "timestamp": 1705401510
    },
    {
      "id": "msg-3",
      "role": "user",
      "content": "Tell me about yourself.",
      "timestamp": 1705401520
    },
    {
      "id": "msg-4",
      "role": "assistant",
      "content": "I'm an AI assistant here to help you.",
      "timestamp": 1705401530
    }
  ],
  "total": 4,
  "limit": 100,
  "offset": 0
}
```

#### DELETE /v1/agents/:id/conversation

Clear conversation history.

**Path Parameters:**
- `id` (string) - Agent ID

**Response:**
```json
{
  "cleared": true,
  "agent_id": "agent-123"
}
```

#### POST /v1/agents/:id/messages/search

Search messages.

**Path Parameters:**
- `id` (string) - Agent ID

**Request Body:**
```json
{
  "query": "hello",
  "limit": 10,
  "search_type": "text"
}
```

**Required Fields:**
- `query` (string, min 1 char)

**Optional Fields:**
- `limit` (number, 1-100, default: 10)
- `search_type` (string, "text" or "semantic", default: "text")

**Response:**
```json
{
  "query": "hello",
  "search_type": "text",
  "results": [
    {
      "id": "msg-1",
      "role": "user",
      "content": "Hello!",
      "timestamp": 1705401600,
      "score": 0.95
    }
  ],
  "total": 1
}
```

#### GET /v1/agents/:id/messages/stats

Get message statistics.

**Path Parameters:**
- `id` (string) - Agent ID

**Response:**
```json
{
  "agent_id": "agent-123",
  "total_messages": 100,
  "user_messages": 50,
  "assistant_messages": 50,
  "total_tokens": 5000,
  "avg_message_length": 50,
  "first_message_at": 1705315200,
  "last_message_at": 1705401600
}
```

---

## Examples

### Create Agent and Send Message

```bash
# 1. Create agent
curl -X POST http://localhost:8283/v1/agents \
  -H "Content-Type: application/json" \
  -d '{
    "name": "MyAssistant",
    "llm_config": {
      "provider": "openai",
      "model": "gpt-4"
    },
    "system_prompt": "You are a helpful assistant."
  }'

# Response: {"id": "agent-123", ...}

# 2. Send message
curl -X POST http://localhost:8283/v1/agents/agent-123/messages \
  -H "Content-Type: application/json" \
  -d '{
    "message": "Hello, how are you?"
  }'

# 3. Get conversation
curl http://localhost:8283/v1/agents/agent-123/conversation
```

### Update Agent Configuration

```bash
# Update LLM config
curl -X PATCH http://localhost:8283/v1/agents/agent-123/config \
  -H "Content-Type: application/json" \
  -d '{
    "llm_config": {
      "temperature": 0.8,
      "max_tokens": 2000
    }
  }'
```

### Search Messages

```bash
# Text search
curl -X POST http://localhost:8283/v1/agents/agent-123/messages/search \
  -H "Content-Type: application/json" \
  -d '{
    "query": "hello",
    "limit": 10,
    "search_type": "text"
  }'
```

---

## Rate Limiting

Rate limiting is enabled by default:
- **100 requests per minute** per IP address
- Exceeded requests return `429 Too Many Requests`

---

## CORS

CORS is enabled by default with:
- **Allowed Origins:** `*` (all origins)
- **Allowed Methods:** `GET, POST, PUT, DELETE, PATCH, OPTIONS`
- **Allowed Headers:** `Content-Type, Authorization`

---

## Validation

All request bodies are validated. Validation errors return `400 Bad Request` with details:

```json
{
  "error": "Validation failed",
  "details": {
    "validation_errors": [
      {
        "field": "name",
        "message": "Field 'name' is required"
      },
      {
        "field": "message",
        "message": "Field 'message' must be at least 1 characters"
      }
    ]
  }
}
```

---

## Future Endpoints

Coming in future phases:

### Memory Operations (Phase 4)
- `POST /v1/agents/:id/memory/core/append`
- `POST /v1/agents/:id/memory/core/replace`
- `POST /v1/agents/:id/memory/archival/insert`
- `POST /v1/agents/:id/memory/archival/search`

### Tool Management (Phase 5)
- `GET /v1/agents/:id/tools`
- `POST /v1/agents/:id/tools/:tool_name/execute`

### Evolution Monitoring (Phase 6)
- `GET /v1/agents/:id/evolution/history`
- `GET /v1/agents/:id/evolution/stats`

---

## Testing

Use the provided example server to test the API:

```bash
# Start server
gxi gerbil/server/example.ss

# Test endpoints
curl http://localhost:8283/health
curl http://localhost:8283/api/v1/hello
```

---

## License

Part of Project O - Self-Evolving AI Agent Platform
