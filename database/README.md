# Database Schema Documentation

PostgreSQL database schema for Project O, designed to be compatible with memos' memory architecture.

## Overview

The database schema supports:
- **Agent Management** - Agent configuration and lifecycle
- **Message Storage** - Conversation history with tool calls
- **Memory System** - Core memory blocks, archival memory, recall memory
- **Evolution Tracking** - Self-evolution events and checkpoints
- **Statistics** - Aggregated metrics per agent

## Requirements

- PostgreSQL 14+
- Extensions:
  - `uuid-ossp` - UUID generation
  - `pgvector` - Vector similarity search

## Tables

### agents

Stores AI agent configurations and settings.

**Columns:**
- `id` (UUID, PK) - Unique agent identifier
- `name` (VARCHAR(100)) - Agent name
- `llm_provider` (VARCHAR(50)) - LLM provider (openai, anthropic, groq, ollama)
- `llm_model` (VARCHAR(100)) - Model name
- `llm_temperature` (FLOAT) - Sampling temperature
- `llm_max_tokens` (INTEGER) - Max tokens to generate
- `llm_api_key_encrypted` (TEXT) - Encrypted API key
- `system_prompt` (TEXT) - System prompt
- `core_memory_enabled` (BOOLEAN) - Enable core memory
- `archival_memory_enabled` (BOOLEAN) - Enable archival memory
- `max_archival_entries` (INTEGER) - Max archival memory entries
- `enabled_tools` (TEXT[]) - Enabled tool names
- `created_at` (TIMESTAMP) - Creation timestamp
- `updated_at` (TIMESTAMP) - Last update timestamp
- `deleted_at` (TIMESTAMP) - Soft delete timestamp

**Indexes:**
- `idx_agents_created_at` - Creation time
- `idx_agents_deleted_at` - Soft delete (partial index)

**Constraints:**
- Name must be 1-100 characters

### messages

Stores conversation messages between users and agents.

**Columns:**
- `id` (UUID, PK) - Unique message identifier
- `agent_id` (UUID, FK) - Agent reference
- `role` (VARCHAR(20)) - Message role (user, assistant, system, tool)
- `content` (TEXT) - Message content
- `tool_calls` (JSONB) - Tool call objects (for assistant messages)
- `tool_call_id` (VARCHAR(100)) - Tool call ID (for tool messages)
- `tool_name` (VARCHAR(100)) - Tool name (for tool messages)
- `created_at` (TIMESTAMP) - Creation timestamp
- `prompt_tokens` (INTEGER) - Prompt tokens used
- `completion_tokens` (INTEGER) - Completion tokens used
- `total_tokens` (INTEGER) - Total tokens used

**Indexes:**
- `idx_messages_agent_id` - Agent lookup
- `idx_messages_created_at` - Time-based queries
- `idx_messages_role` - Role filtering
- `idx_messages_agent_created` - Agent + time composite

**Constraints:**
- Role must be one of: user, assistant, system, tool
- Cascades on agent deletion

### memory_blocks

Stores core memory blocks (persona, human, custom).

**Columns:**
- `id` (UUID, PK) - Unique block identifier
- `agent_id` (UUID, FK) - Agent reference
- `label` (VARCHAR(100)) - Block label (persona, human, etc.)
- `value` (TEXT) - Block content
- `is_template` (BOOLEAN) - Is template block
- `is_readonly` (BOOLEAN) - Is read-only
- `created_at` (TIMESTAMP) - Creation timestamp
- `updated_at` (TIMESTAMP) - Last update timestamp

**Indexes:**
- `idx_memory_blocks_agent_id` - Agent lookup
- `idx_memory_blocks_label` - Label lookup

**Constraints:**
- Unique (agent_id, label) - One block per label per agent
- Label must be 1-100 characters
- Cascades on agent deletion

### archival_memory

Stores long-term memory with semantic search capability.

**Columns:**
- `id` (UUID, PK) - Unique memory identifier
- `agent_id` (UUID, FK) - Agent reference
- `content` (TEXT) - Memory content
- `embedding` (VECTOR(1536)) - Vector embedding for semantic search
- `importance` (FLOAT) - Importance score (0.0-1.0)
- `tags` (TEXT[]) - Memory tags
- `created_at` (TIMESTAMP) - Creation timestamp

**Indexes:**
- `idx_archival_memory_agent_id` - Agent lookup
- `idx_archival_memory_created_at` - Time-based queries
- `idx_archival_memory_importance` - Importance filtering
- `idx_archival_memory_tags` - Tag search (GIN index)
- `idx_archival_memory_embedding` - Vector similarity search (IVFFlat)

**Constraints:**
- Importance must be between 0.0 and 1.0
- Cascades on agent deletion

**Vector Search:**
Uses pgvector with IVFFlat index for cosine similarity search. Embedding dimension is 1536 (OpenAI ada-002).

### recall_memory

Stores short-term conversation context.

**Columns:**
- `id` (UUID, PK) - Unique memory identifier
- `agent_id` (UUID, FK) - Agent reference
- `content` (TEXT) - Memory content
- `message_ids` (UUID[]) - Related message IDs
- `created_at` (TIMESTAMP) - Creation timestamp
- `expires_at` (TIMESTAMP) - Expiration timestamp (optional)

**Indexes:**
- `idx_recall_memory_agent_id` - Agent lookup
- `idx_recall_memory_created_at` - Time-based queries
- `idx_recall_memory_expires_at` - Expiration queries (partial index)

**Constraints:**
- Cascades on agent deletion

### agent_checkpoints

Stores agent state snapshots for rollback.

**Columns:**
- `id` (UUID, PK) - Unique checkpoint identifier
- `agent_id` (UUID, FK) - Agent reference
- `checkpoint_data` (JSONB) - Full agent state snapshot
- `checkpoint_type` (VARCHAR(50)) - Checkpoint type (manual, auto, pre_evolution)
- `description` (TEXT) - Checkpoint description
- `created_at` (TIMESTAMP) - Creation timestamp

**Indexes:**
- `idx_agent_checkpoints_agent_id` - Agent lookup
- `idx_agent_checkpoints_created_at` - Time-based queries
- `idx_agent_checkpoints_type` - Type filtering

**Constraints:**
- Type must be one of: manual, auto, pre_evolution
- Cascades on agent deletion

### evolution_events

Tracks self-evolution attempts and results.

**Columns:**
- `id` (UUID, PK) - Unique event identifier
- `agent_id` (UUID, FK) - Agent reference
- `hypothesis` (TEXT) - Evolution hypothesis
- `code_changes` (JSONB) - Code changes applied
- `improvement` (FLOAT) - Performance improvement percentage
- `success` (BOOLEAN) - Evolution success status
- `checkpoint_id` (UUID, FK) - Pre-evolution checkpoint reference
- `created_at` (TIMESTAMP) - Creation timestamp

**Indexes:**
- `idx_evolution_events_agent_id` - Agent lookup
- `idx_evolution_events_created_at` - Time-based queries
- `idx_evolution_events_success` - Success filtering

**Constraints:**
- Cascades on agent deletion

### agent_statistics

Aggregated statistics per agent.

**Columns:**
- `id` (UUID, PK) - Unique statistics identifier
- `agent_id` (UUID, FK) - Agent reference (unique)
- `total_messages` (INTEGER) - Total message count
- `user_messages` (INTEGER) - User message count
- `assistant_messages` (INTEGER) - Assistant message count
- `total_tokens` (BIGINT) - Total tokens used
- `prompt_tokens` (BIGINT) - Prompt tokens used
- `completion_tokens` (BIGINT) - Completion tokens used
- `archival_memory_count` (INTEGER) - Archival memory entry count
- `recall_memory_count` (INTEGER) - Recall memory entry count
- `evolution_attempts` (INTEGER) - Evolution attempt count
- `successful_evolutions` (INTEGER) - Successful evolution count
- `first_message_at` (TIMESTAMP) - First message timestamp
- `last_message_at` (TIMESTAMP) - Last message timestamp
- `last_evolution_at` (TIMESTAMP) - Last evolution timestamp
- `updated_at` (TIMESTAMP) - Last update timestamp

**Indexes:**
- `idx_agent_statistics_agent_id` - Agent lookup

**Constraints:**
- Unique agent_id - One statistics row per agent
- Cascades on agent deletion

## Views

### agent_summary

Provides a summary view of agents with key statistics.

**Columns:**
- `id` - Agent ID
- `name` - Agent name
- `llm_provider` - LLM provider
- `llm_model` - LLM model
- `created_at` - Creation timestamp
- `updated_at` - Update timestamp
- `total_messages` - Total message count
- `archival_memory_count` - Archival memory count
- `successful_evolutions` - Successful evolution count

**Filters:**
- Only non-deleted agents (deleted_at IS NULL)

### recent_messages

Provides recent messages across all agents.

**Columns:**
- `id` - Message ID
- `agent_id` - Agent ID
- `agent_name` - Agent name
- `role` - Message role
- `content` - Message content
- `created_at` - Creation timestamp
- `total_tokens` - Total tokens used

**Filters:**
- Only messages from non-deleted agents
- Ordered by creation time (descending)

## Functions

### update_updated_at_column()

Trigger function that automatically updates the `updated_at` column on row updates.

**Applied to:**
- `agents`
- `memory_blocks`
- `agent_statistics`

### cleanup_expired_recall_memory()

Deletes expired recall memory entries.

**Returns:** Number of deleted entries

**Usage:**
```sql
SELECT cleanup_expired_recall_memory();
```

### prune_archival_memory(agent_id, max_entries, min_importance)

Prunes low-importance archival memory entries for an agent.

**Parameters:**
- `p_agent_id` (UUID) - Agent ID
- `p_max_entries` (INTEGER) - Maximum entries to keep
- `p_min_importance` (FLOAT, default: 0.3) - Minimum importance threshold

**Returns:** Number of deleted entries

**Usage:**
```sql
-- Keep top 5000 entries with importance >= 0.3
SELECT prune_archival_memory('agent-uuid', 5000, 0.3);
```

## Migrations

### Running Migrations

```bash
# Install Alembic
pip install alembic psycopg2-binary

# Initialize Alembic (if not already done)
alembic init alembic

# Run migrations
alembic upgrade head

# Rollback
alembic downgrade -1
```

### Migration Files

- `001_initial_schema.py` - Initial schema creation

## Setup

### 1. Install PostgreSQL Extensions

```sql
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgvector";
```

### 2. Run Schema

```bash
# Option 1: Direct SQL
psql -U postgres -d project_o < database/schema.sql

# Option 2: Alembic migrations
alembic upgrade head
```

### 3. Verify Installation

```sql
-- Check tables
\dt

-- Check extensions
\dx

-- Check sample data
SELECT * FROM agents;
SELECT * FROM agent_summary;
```

## Performance Considerations

### Indexes

All critical query paths are indexed:
- Agent lookups by ID
- Message queries by agent and time
- Memory block lookups by agent and label
- Vector similarity search on archival memory
- Tag-based search on archival memory

### Vector Search

The IVFFlat index on `archival_memory.embedding` provides fast approximate nearest neighbor search:
- Index type: IVFFlat with cosine similarity
- Lists: 100 (adjust based on data size)
- Dimension: 1536 (OpenAI ada-002)

For optimal performance:
- Rebuild index periodically: `REINDEX INDEX idx_archival_memory_embedding`
- Adjust `lists` parameter based on data size (rule of thumb: sqrt(rows))

### Maintenance

Regular maintenance tasks:
```sql
-- Clean up expired recall memory
SELECT cleanup_expired_recall_memory();

-- Prune archival memory for all agents
SELECT prune_archival_memory(id, 10000, 0.3) FROM agents;

-- Vacuum and analyze
VACUUM ANALYZE;

-- Reindex vector index
REINDEX INDEX idx_archival_memory_embedding;
```

## Security

### API Key Encryption

Agent API keys should be encrypted before storage:
- Use application-level encryption
- Store encrypted value in `llm_api_key_encrypted`
- Never log or expose decrypted keys

### Soft Deletes

Agents use soft deletes (`deleted_at` column):
- Preserves data for audit/recovery
- Filtered out in views and queries
- Can be permanently deleted later

### Row-Level Security (Future)

Consider implementing RLS for multi-tenant scenarios:
```sql
ALTER TABLE agents ENABLE ROW LEVEL SECURITY;

CREATE POLICY agent_isolation ON agents
    USING (owner_id = current_user_id());
```

## Monitoring

### Key Metrics

Monitor these metrics:
- Table sizes: `pg_total_relation_size('table_name')`
- Index usage: `pg_stat_user_indexes`
- Query performance: `pg_stat_statements`
- Vector index performance: Query execution time

### Queries

```sql
-- Table sizes
SELECT
    schemaname,
    tablename,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS size
FROM pg_tables
WHERE schemaname = 'public'
ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC;

-- Index usage
SELECT
    schemaname,
    tablename,
    indexname,
    idx_scan,
    idx_tup_read,
    idx_tup_fetch
FROM pg_stat_user_indexes
ORDER BY idx_scan DESC;

-- Slow queries (requires pg_stat_statements)
SELECT
    query,
    calls,
    total_time,
    mean_time,
    max_time
FROM pg_stat_statements
ORDER BY mean_time DESC
LIMIT 10;
```

## Backup and Recovery

### Backup

```bash
# Full database backup
pg_dump -U postgres -d project_o -F c -f backup.dump

# Schema only
pg_dump -U postgres -d project_o -s -f schema.sql

# Data only
pg_dump -U postgres -d project_o -a -f data.sql
```

### Restore

```bash
# Restore from custom format
pg_restore -U postgres -d project_o backup.dump

# Restore from SQL
psql -U postgres -d project_o < backup.sql
```

## Future Enhancements

- [ ] Partitioning for large tables (messages, archival_memory)
- [ ] Read replicas for scaling
- [ ] Connection pooling (PgBouncer)
- [ ] Row-level security for multi-tenancy
- [ ] Audit logging
- [ ] Automated backup and retention policies
- [ ] Performance monitoring dashboard

## License

Part of Project O - Self-Evolving AI Agent Platform
