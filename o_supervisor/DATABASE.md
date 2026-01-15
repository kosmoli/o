# Elixir Database Module

PostgreSQL database operations for Project O's Elixir supervisor layer.

## Overview

The `OSupervisor.Database` module provides structured database operations for:
- Agent management (CRUD)
- Message storage and retrieval
- Memory blocks (core memory)
- Archival memory with search
- Agent statistics tracking

## Installation

Add Postgrex dependency to `mix.exs`:

```elixir
{:postgrex, "~> 0.17"}
```

Install dependencies:

```bash
cd o_supervisor
mix deps.get
```

## Configuration

Configure database connection in `config/config.exs`:

```elixir
config :o_supervisor, OSupervisor.Database,
  hostname: "localhost",
  port: 5432,
  database: "project_o",
  username: "postgres",
  password: "",
  pool_size: 10
```

## Usage

### Starting the Database Module

The database module is automatically started by the supervisor:

```elixir
# In OSupervisor.Application
children = [
  {OSupervisor.Database, database_config()},
  # ... other children
]
```

### Agent Operations

```elixir
# Create agent
{:ok, agent} = OSupervisor.Database.create_agent(%{
  name: "MyAgent",
  llm_provider: "openai",
  llm_model: "gpt-4",
  system_prompt: "You are a helpful assistant."
})

# Get agent
{:ok, agent} = OSupervisor.Database.get_agent(agent_id)

# Update agent
{:ok, agent} = OSupervisor.Database.update_agent(agent_id, %{
  name: "UpdatedAgent",
  llm_temperature: 0.8
})

# Delete agent (soft delete)
:ok = OSupervisor.Database.delete_agent(agent_id)

# List agents
{:ok, agents} = OSupervisor.Database.list_agents(limit: 10, offset: 0)
```

### Message Operations

```elixir
# Create message
{:ok, message} = OSupervisor.Database.create_message(agent_id, %{
  role: "user",
  content: "Hello!",
  prompt_tokens: 5,
  completion_tokens: 0,
  total_tokens: 5
})

# Get messages
{:ok, messages} = OSupervisor.Database.get_messages(agent_id,
  limit: 50,
  offset: 0,
  role: "user"  # optional filter
)

# Delete message
:ok = OSupervisor.Database.delete_message(message_id)
```

### Memory Block Operations

```elixir
# Create or update memory block
{:ok, block} = OSupervisor.Database.create_memory_block(agent_id, %{
  label: "persona",
  value: "You are a helpful AI assistant.",
  is_readonly: false
})

# Get all memory blocks
{:ok, blocks} = OSupervisor.Database.get_memory_blocks(agent_id)

# Update memory block
{:ok, block} = OSupervisor.Database.update_memory_block(
  agent_id,
  "persona",
  "You are a very helpful AI assistant."
)
```

### Archival Memory Operations

```elixir
# Insert archival memory
{:ok, memory} = OSupervisor.Database.insert_archival_memory(agent_id, %{
  content: "Important information to remember",
  embedding: nil,  # TODO: Add vector embedding
  importance: 0.8,
  tags: ["important", "user_preference"]
})

# Search archival memory (text search)
{:ok, results} = OSupervisor.Database.search_archival_memory(
  agent_id,
  "important",
  limit: 10,
  search_type: :text
)

# Search archival memory (semantic search - requires embeddings)
{:ok, results} = OSupervisor.Database.search_archival_memory(
  agent_id,
  embedding_vector,
  limit: 10,
  search_type: :semantic
)
```

### Statistics Operations

```elixir
# Get agent statistics
{:ok, stats} = OSupervisor.Database.get_agent_statistics(agent_id)

# Update statistics
:ok = OSupervisor.Database.update_statistics(agent_id, %{
  evolution_attempts: 5,
  successful_evolutions: 3
})
```

## API Reference

### Agent Operations

#### `create_agent(params)`

Creates a new agent.

**Parameters:**
- `name` (required) - Agent name
- `llm_provider` (optional, default: "openai") - LLM provider
- `llm_model` (optional, default: "gpt-4") - Model name
- `llm_temperature` (optional, default: 0.7) - Temperature
- `llm_max_tokens` (optional, default: 4096) - Max tokens
- `system_prompt` (optional) - System prompt
- `core_memory_enabled` (optional, default: true) - Enable core memory
- `archival_memory_enabled` (optional, default: true) - Enable archival memory
- `max_archival_entries` (optional, default: 10000) - Max archival entries
- `enabled_tools` (optional) - List of enabled tool names

**Returns:** `{:ok, agent}` or `{:error, reason}`

#### `get_agent(agent_id)`

Gets an agent by ID.

**Returns:** `{:ok, agent}` or `{:error, :not_found}`

#### `update_agent(agent_id, params)`

Updates an agent.

**Returns:** `{:ok, agent}` or `{:error, reason}`

#### `delete_agent(agent_id)`

Soft deletes an agent.

**Returns:** `:ok` or `{:error, reason}`

#### `list_agents(opts \\ [])`

Lists agents with pagination.

**Options:**
- `limit` (default: 10) - Number of agents to return
- `offset` (default: 0) - Offset for pagination

**Returns:** `{:ok, agents}` or `{:error, reason}`

### Message Operations

#### `create_message(agent_id, params)`

Creates a new message.

**Parameters:**
- `role` (required) - Message role ("user", "assistant", "system", "tool")
- `content` (required) - Message content
- `tool_calls` (optional) - Tool call objects (JSONB)
- `tool_call_id` (optional) - Tool call ID (for tool messages)
- `tool_name` (optional) - Tool name (for tool messages)
- `prompt_tokens` (optional) - Prompt tokens used
- `completion_tokens` (optional) - Completion tokens used
- `total_tokens` (optional) - Total tokens used

**Returns:** `{:ok, message}` or `{:error, reason}`

**Side Effects:** Updates agent statistics

#### `get_messages(agent_id, opts \\ [])`

Gets messages for an agent.

**Options:**
- `limit` (default: 50) - Number of messages to return
- `offset` (default: 0) - Offset for pagination
- `role` (optional) - Filter by role

**Returns:** `{:ok, messages}` or `{:error, reason}`

#### `delete_message(message_id)`

Deletes a message.

**Returns:** `:ok` or `{:error, reason}`

### Memory Block Operations

#### `create_memory_block(agent_id, params)`

Creates or updates a memory block.

**Parameters:**
- `label` (required) - Block label
- `value` (required) - Block content
- `is_template` (optional, default: false) - Is template block
- `is_readonly` (optional, default: false) - Is read-only

**Returns:** `{:ok, block}` or `{:error, reason}`

**Note:** Uses `ON CONFLICT` to update existing blocks with the same label.

#### `get_memory_blocks(agent_id)`

Gets all memory blocks for an agent.

**Returns:** `{:ok, blocks}` or `{:error, reason}`

#### `update_memory_block(agent_id, label, value)`

Updates a memory block value.

**Returns:** `{:ok, block}` or `{:error, :not_found_or_readonly}`

**Note:** Cannot update read-only blocks.

### Archival Memory Operations

#### `insert_archival_memory(agent_id, params)`

Inserts archival memory entry.

**Parameters:**
- `content` (required) - Memory content
- `embedding` (optional) - Vector embedding (1536 dimensions)
- `importance` (optional, default: 0.5) - Importance score (0.0-1.0)
- `tags` (optional) - List of tags

**Returns:** `{:ok, memory}` or `{:error, reason}`

#### `search_archival_memory(agent_id, query, opts \\ [])`

Searches archival memory.

**Options:**
- `limit` (default: 10) - Number of results to return
- `search_type` (default: :text) - Search type (:text or :semantic)

**Returns:** `{:ok, results}` or `{:error, reason}`

**Search Types:**
- `:text` - Text-based search using ILIKE
- `:semantic` - Vector similarity search (requires embeddings)

### Statistics Operations

#### `get_agent_statistics(agent_id)`

Gets agent statistics.

**Returns:** `{:ok, stats}` or `{:error, :not_found}`

**Statistics Include:**
- Message counts (total, user, assistant)
- Token usage (total, prompt, completion)
- Memory counts (archival, recall)
- Evolution metrics (attempts, successful)
- Timestamps (first message, last message, last evolution)

#### `update_statistics(agent_id, updates)`

Updates agent statistics.

**Returns:** `:ok` or `{:error, reason}`

## Data Structures

### Agent

```elixir
%{
  id: "uuid",
  name: "MyAgent",
  llm_provider: "openai",
  llm_model: "gpt-4",
  created_at: ~U[2026-01-16 00:00:00Z],
  updated_at: ~U[2026-01-16 00:00:00Z]
}
```

### Message

```elixir
%{
  id: "uuid",
  agent_id: "uuid",
  role: "user",
  content: "Hello!",
  tool_calls: nil,
  created_at: ~U[2026-01-16 00:00:00Z],
  total_tokens: 5
}
```

### Memory Block

```elixir
%{
  id: "uuid",
  agent_id: "uuid",
  label: "persona",
  value: "You are a helpful AI assistant.",
  is_template: false,
  is_readonly: false,
  created_at: ~U[2026-01-16 00:00:00Z],
  updated_at: ~U[2026-01-16 00:00:00Z]
}
```

### Archival Memory

```elixir
%{
  id: "uuid",
  agent_id: "uuid",
  content: "Important information",
  importance: 0.8,
  tags: ["important"],
  created_at: ~U[2026-01-16 00:00:00Z]
}
```

### Statistics

```elixir
%{
  agent_id: "uuid",
  total_messages: 100,
  user_messages: 50,
  assistant_messages: 50,
  total_tokens: 5000,
  prompt_tokens: 2500,
  completion_tokens: 2500,
  archival_memory_count: 10,
  recall_memory_count: 5,
  evolution_attempts: 3,
  successful_evolutions: 2,
  first_message_at: ~U[2026-01-15 00:00:00Z],
  last_message_at: ~U[2026-01-16 00:00:00Z],
  last_evolution_at: ~U[2026-01-16 00:00:00Z]
}
```

## Error Handling

All operations return either `{:ok, result}` or `{:error, reason}`.

Common error reasons:
- `:not_found` - Resource not found
- `:not_found_or_readonly` - Memory block not found or is read-only
- `Postgrex.Error` - Database error

## Automatic Initialization

When creating an agent, the following are automatically initialized:
1. **Memory Blocks**: Default "persona" and "human" blocks
2. **Statistics**: Empty statistics row

## Performance Considerations

### Connection Pooling

The module uses Postgrex connection pooling. Configure pool size:

```elixir
config :o_supervisor, OSupervisor.Database,
  pool_size: 10  # Adjust based on load
```

### Indexes

All critical query paths are indexed in the database schema:
- Agent lookups by ID
- Message queries by agent and time
- Memory block lookups by agent and label
- Vector similarity search on archival memory

### Batch Operations

For bulk operations, consider using transactions:

```elixir
Postgrex.transaction(conn, fn conn ->
  # Multiple operations
end)
```

## Testing

```elixir
# In test/o_supervisor/database_test.exs
defmodule OSupervisor.DatabaseTest do
  use ExUnit.Case

  setup do
    # Setup test database
    :ok
  end

  test "creates agent" do
    {:ok, agent} = OSupervisor.Database.create_agent(%{
      name: "TestAgent"
    })

    assert agent.name == "TestAgent"
    assert agent.llm_provider == "openai"
  end

  # More tests...
end
```

## Integration with Gerbil

The Elixir database module is accessed from Gerbil via MessagePack protocol:

```scheme
;; Gerbil -> Elixir
(elixir-send "db_query"
  (hash 'operation 'create_agent
        'params (hash 'name "MyAgent")))

;; Elixir -> Gerbil
(hash 'type "db_result"
      'success #t
      'data (hash 'id "uuid" 'name "MyAgent"))
```

See `gerbil/database/client.ss` for the Gerbil client implementation.

## Future Enhancements

- [ ] Vector embedding generation integration
- [ ] Batch insert operations
- [ ] Database connection pooling optimization
- [ ] Query result caching
- [ ] Prepared statement support
- [ ] Database migration management
- [ ] Backup and restore utilities

## License

Part of Project O - Self-Evolving AI Agent Platform
