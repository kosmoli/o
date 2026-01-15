-- Project O Database Schema
-- Memos-compatible schema for agent management, messages, and memory
-- PostgreSQL 14+

-- Enable required extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgvector";

-- ============================================================================
-- Agents Table
-- ============================================================================

CREATE TABLE agents (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name VARCHAR(100) NOT NULL,

    -- LLM Configuration
    llm_provider VARCHAR(50) NOT NULL DEFAULT 'openai',
    llm_model VARCHAR(100) NOT NULL DEFAULT 'gpt-4',
    llm_temperature FLOAT DEFAULT 0.7,
    llm_max_tokens INTEGER DEFAULT 4096,
    llm_api_key_encrypted TEXT,  -- Encrypted API key

    -- System Configuration
    system_prompt TEXT NOT NULL DEFAULT 'You are a helpful assistant.',

    -- Memory Configuration
    core_memory_enabled BOOLEAN DEFAULT TRUE,
    archival_memory_enabled BOOLEAN DEFAULT TRUE,
    max_archival_entries INTEGER DEFAULT 10000,

    -- Tool Configuration
    enabled_tools TEXT[] DEFAULT ARRAY['send_message', 'conversation_search'],

    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    deleted_at TIMESTAMP WITH TIME ZONE,  -- Soft delete

    -- Indexes
    CONSTRAINT agents_name_check CHECK (char_length(name) >= 1 AND char_length(name) <= 100)
);

CREATE INDEX idx_agents_created_at ON agents(created_at);
CREATE INDEX idx_agents_deleted_at ON agents(deleted_at) WHERE deleted_at IS NULL;

-- ============================================================================
-- Messages Table
-- ============================================================================

CREATE TABLE messages (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    agent_id UUID NOT NULL REFERENCES agents(id) ON DELETE CASCADE,

    -- Message Content
    role VARCHAR(20) NOT NULL,  -- 'user', 'assistant', 'system', 'tool'
    content TEXT NOT NULL,

    -- Tool Call Information (for assistant messages with tool calls)
    tool_calls JSONB,  -- Array of tool call objects

    -- Tool Response Information (for tool messages)
    tool_call_id VARCHAR(100),  -- ID of the tool call this responds to
    tool_name VARCHAR(100),     -- Name of the tool

    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    -- Token Usage
    prompt_tokens INTEGER,
    completion_tokens INTEGER,
    total_tokens INTEGER,

    -- Indexes
    CONSTRAINT messages_role_check CHECK (role IN ('user', 'assistant', 'system', 'tool'))
);

CREATE INDEX idx_messages_agent_id ON messages(agent_id);
CREATE INDEX idx_messages_created_at ON messages(created_at);
CREATE INDEX idx_messages_role ON messages(role);
CREATE INDEX idx_messages_agent_created ON messages(agent_id, created_at DESC);

-- ============================================================================
-- Memory Blocks Table (Core Memory)
-- ============================================================================

CREATE TABLE memory_blocks (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    agent_id UUID NOT NULL REFERENCES agents(id) ON DELETE CASCADE,

    -- Block Information
    label VARCHAR(100) NOT NULL,  -- 'persona', 'human', or custom label
    value TEXT NOT NULL,

    -- Block Configuration
    is_template BOOLEAN DEFAULT FALSE,  -- Is this a template block?
    is_readonly BOOLEAN DEFAULT FALSE,  -- Can this block be modified?

    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    -- Constraints
    CONSTRAINT memory_blocks_unique_label UNIQUE (agent_id, label),
    CONSTRAINT memory_blocks_label_check CHECK (char_length(label) >= 1 AND char_length(label) <= 100)
);

CREATE INDEX idx_memory_blocks_agent_id ON memory_blocks(agent_id);
CREATE INDEX idx_memory_blocks_label ON memory_blocks(label);

-- ============================================================================
-- Archival Memory Table
-- ============================================================================

CREATE TABLE archival_memory (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    agent_id UUID NOT NULL REFERENCES agents(id) ON DELETE CASCADE,

    -- Memory Content
    content TEXT NOT NULL,

    -- Embedding for semantic search
    embedding vector(1536),  -- OpenAI ada-002 embedding dimension

    -- Metadata
    importance FLOAT DEFAULT 0.5,  -- 0.0 to 1.0
    tags TEXT[],

    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    -- Constraints
    CONSTRAINT archival_memory_importance_check CHECK (importance >= 0.0 AND importance <= 1.0)
);

CREATE INDEX idx_archival_memory_agent_id ON archival_memory(agent_id);
CREATE INDEX idx_archival_memory_created_at ON archival_memory(created_at);
CREATE INDEX idx_archival_memory_importance ON archival_memory(importance);
CREATE INDEX idx_archival_memory_tags ON archival_memory USING GIN(tags);

-- Vector similarity search index (for semantic search)
CREATE INDEX idx_archival_memory_embedding ON archival_memory
    USING ivfflat (embedding vector_cosine_ops)
    WITH (lists = 100);

-- ============================================================================
-- Recall Memory Table (Conversation Context)
-- ============================================================================

CREATE TABLE recall_memory (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    agent_id UUID NOT NULL REFERENCES agents(id) ON DELETE CASCADE,

    -- Memory Content
    content TEXT NOT NULL,

    -- Context Information
    message_ids UUID[],  -- Related message IDs

    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP WITH TIME ZONE  -- Optional expiration
);

CREATE INDEX idx_recall_memory_agent_id ON recall_memory(agent_id);
CREATE INDEX idx_recall_memory_created_at ON recall_memory(created_at);
CREATE INDEX idx_recall_memory_expires_at ON recall_memory(expires_at) WHERE expires_at IS NOT NULL;

-- ============================================================================
-- Agent Checkpoints Table (for rollback)
-- ============================================================================

CREATE TABLE agent_checkpoints (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    agent_id UUID NOT NULL REFERENCES agents(id) ON DELETE CASCADE,

    -- Checkpoint Data
    checkpoint_data JSONB NOT NULL,  -- Full agent state snapshot

    -- Metadata
    checkpoint_type VARCHAR(50) NOT NULL,  -- 'manual', 'auto', 'pre_evolution'
    description TEXT,

    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    -- Constraints
    CONSTRAINT agent_checkpoints_type_check CHECK (checkpoint_type IN ('manual', 'auto', 'pre_evolution'))
);

CREATE INDEX idx_agent_checkpoints_agent_id ON agent_checkpoints(agent_id);
CREATE INDEX idx_agent_checkpoints_created_at ON agent_checkpoints(created_at);
CREATE INDEX idx_agent_checkpoints_type ON agent_checkpoints(checkpoint_type);

-- ============================================================================
-- Evolution Events Table (for self-evolution tracking)
-- ============================================================================

CREATE TABLE evolution_events (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    agent_id UUID NOT NULL REFERENCES agents(id) ON DELETE CASCADE,

    -- Evolution Information
    hypothesis TEXT NOT NULL,
    code_changes JSONB NOT NULL,

    -- Results
    improvement FLOAT,  -- Performance improvement percentage
    success BOOLEAN NOT NULL,

    -- Checkpoint Reference
    checkpoint_id UUID REFERENCES agent_checkpoints(id),

    -- Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_evolution_events_agent_id ON evolution_events(agent_id);
CREATE INDEX idx_evolution_events_created_at ON evolution_events(created_at);
CREATE INDEX idx_evolution_events_success ON evolution_events(success);

-- ============================================================================
-- Agent Statistics Table
-- ============================================================================

CREATE TABLE agent_statistics (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    agent_id UUID NOT NULL REFERENCES agents(id) ON DELETE CASCADE,

    -- Message Statistics
    total_messages INTEGER DEFAULT 0,
    user_messages INTEGER DEFAULT 0,
    assistant_messages INTEGER DEFAULT 0,

    -- Token Statistics
    total_tokens BIGINT DEFAULT 0,
    prompt_tokens BIGINT DEFAULT 0,
    completion_tokens BIGINT DEFAULT 0,

    -- Memory Statistics
    archival_memory_count INTEGER DEFAULT 0,
    recall_memory_count INTEGER DEFAULT 0,

    -- Evolution Statistics
    evolution_attempts INTEGER DEFAULT 0,
    successful_evolutions INTEGER DEFAULT 0,

    -- Timestamps
    first_message_at TIMESTAMP WITH TIME ZONE,
    last_message_at TIMESTAMP WITH TIME ZONE,
    last_evolution_at TIMESTAMP WITH TIME ZONE,

    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    -- Constraints
    CONSTRAINT agent_statistics_unique_agent UNIQUE (agent_id)
);

CREATE INDEX idx_agent_statistics_agent_id ON agent_statistics(agent_id);

-- ============================================================================
-- Triggers for updated_at
-- ============================================================================

CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER update_agents_updated_at BEFORE UPDATE ON agents
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_memory_blocks_updated_at BEFORE UPDATE ON memory_blocks
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_agent_statistics_updated_at BEFORE UPDATE ON agent_statistics
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- ============================================================================
-- Views
-- ============================================================================

-- Agent Summary View
CREATE VIEW agent_summary AS
SELECT
    a.id,
    a.name,
    a.llm_provider,
    a.llm_model,
    a.created_at,
    a.updated_at,
    COALESCE(s.total_messages, 0) as total_messages,
    COALESCE(s.archival_memory_count, 0) as archival_memory_count,
    COALESCE(s.successful_evolutions, 0) as successful_evolutions
FROM agents a
LEFT JOIN agent_statistics s ON a.id = s.agent_id
WHERE a.deleted_at IS NULL;

-- Recent Messages View
CREATE VIEW recent_messages AS
SELECT
    m.id,
    m.agent_id,
    a.name as agent_name,
    m.role,
    m.content,
    m.created_at,
    m.total_tokens
FROM messages m
JOIN agents a ON m.agent_id = a.id
WHERE a.deleted_at IS NULL
ORDER BY m.created_at DESC;

-- ============================================================================
-- Sample Data (for testing)
-- ============================================================================

-- Insert sample agent
INSERT INTO agents (name, llm_provider, llm_model, system_prompt)
VALUES ('TestAgent', 'openai', 'gpt-4', 'You are a helpful AI assistant.');

-- Insert sample memory blocks
INSERT INTO memory_blocks (agent_id, label, value)
SELECT id, 'persona', 'You are a helpful AI assistant.'
FROM agents WHERE name = 'TestAgent';

INSERT INTO memory_blocks (agent_id, label, value)
SELECT id, 'human', 'User prefers concise responses.'
FROM agents WHERE name = 'TestAgent';

-- Initialize statistics
INSERT INTO agent_statistics (agent_id)
SELECT id FROM agents WHERE name = 'TestAgent';

-- ============================================================================
-- Cleanup Functions
-- ============================================================================

-- Function to clean up old recall memory
CREATE OR REPLACE FUNCTION cleanup_expired_recall_memory()
RETURNS INTEGER AS $$
DECLARE
    deleted_count INTEGER;
BEGIN
    DELETE FROM recall_memory
    WHERE expires_at IS NOT NULL AND expires_at < CURRENT_TIMESTAMP;

    GET DIAGNOSTICS deleted_count = ROW_COUNT;
    RETURN deleted_count;
END;
$$ LANGUAGE plpgsql;

-- Function to prune low-importance archival memory
CREATE OR REPLACE FUNCTION prune_archival_memory(
    p_agent_id UUID,
    p_max_entries INTEGER,
    p_min_importance FLOAT DEFAULT 0.3
)
RETURNS INTEGER AS $$
DECLARE
    deleted_count INTEGER;
BEGIN
    WITH ranked_memories AS (
        SELECT id,
               ROW_NUMBER() OVER (ORDER BY importance DESC, created_at DESC) as rank
        FROM archival_memory
        WHERE agent_id = p_agent_id
          AND importance >= p_min_importance
    )
    DELETE FROM archival_memory
    WHERE id IN (
        SELECT id FROM ranked_memories WHERE rank > p_max_entries
    );

    GET DIAGNOSTICS deleted_count = ROW_COUNT;
    RETURN deleted_count;
END;
$$ LANGUAGE plpgsql;

-- ============================================================================
-- Comments
-- ============================================================================

COMMENT ON TABLE agents IS 'AI agents with LLM configuration and memory settings';
COMMENT ON TABLE messages IS 'Conversation messages between users and agents';
COMMENT ON TABLE memory_blocks IS 'Core memory blocks (persona, human, custom)';
COMMENT ON TABLE archival_memory IS 'Long-term memory with semantic search';
COMMENT ON TABLE recall_memory IS 'Short-term conversation context';
COMMENT ON TABLE agent_checkpoints IS 'Agent state snapshots for rollback';
COMMENT ON TABLE evolution_events IS 'Self-evolution history and results';
COMMENT ON TABLE agent_statistics IS 'Aggregated statistics per agent';

COMMENT ON COLUMN archival_memory.embedding IS 'Vector embedding for semantic search (1536 dimensions for OpenAI ada-002)';
COMMENT ON COLUMN evolution_events.improvement IS 'Performance improvement as percentage (e.g., 0.15 = 15% improvement)';
