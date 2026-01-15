"""Initial schema

Revision ID: 001
Revises:
Create Date: 2026-01-16 06:30:00.000000

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import postgresql

# revision identifiers, used by Alembic.
revision = '001'
down_revision = None
branch_labels = None
depends_on = None


def upgrade():
    # Enable extensions
    op.execute('CREATE EXTENSION IF NOT EXISTS "uuid-ossp"')
    op.execute('CREATE EXTENSION IF NOT EXISTS "pgvector"')

    # Create agents table
    op.create_table(
        'agents',
        sa.Column('id', postgresql.UUID(as_uuid=True), primary_key=True, server_default=sa.text('uuid_generate_v4()')),
        sa.Column('name', sa.String(100), nullable=False),
        sa.Column('llm_provider', sa.String(50), nullable=False, server_default='openai'),
        sa.Column('llm_model', sa.String(100), nullable=False, server_default='gpt-4'),
        sa.Column('llm_temperature', sa.Float, server_default='0.7'),
        sa.Column('llm_max_tokens', sa.Integer, server_default='4096'),
        sa.Column('llm_api_key_encrypted', sa.Text),
        sa.Column('system_prompt', sa.Text, nullable=False, server_default='You are a helpful assistant.'),
        sa.Column('core_memory_enabled', sa.Boolean, server_default='true'),
        sa.Column('archival_memory_enabled', sa.Boolean, server_default='true'),
        sa.Column('max_archival_entries', sa.Integer, server_default='10000'),
        sa.Column('enabled_tools', postgresql.ARRAY(sa.Text), server_default="ARRAY['send_message', 'conversation_search']"),
        sa.Column('created_at', sa.TIMESTAMP(timezone=True), server_default=sa.text('CURRENT_TIMESTAMP')),
        sa.Column('updated_at', sa.TIMESTAMP(timezone=True), server_default=sa.text('CURRENT_TIMESTAMP')),
        sa.Column('deleted_at', sa.TIMESTAMP(timezone=True)),
        sa.CheckConstraint("char_length(name) >= 1 AND char_length(name) <= 100", name='agents_name_check')
    )
    op.create_index('idx_agents_created_at', 'agents', ['created_at'])
    op.create_index('idx_agents_deleted_at', 'agents', ['deleted_at'], postgresql_where=sa.text('deleted_at IS NULL'))

    # Create messages table
    op.create_table(
        'messages',
        sa.Column('id', postgresql.UUID(as_uuid=True), primary_key=True, server_default=sa.text('uuid_generate_v4()')),
        sa.Column('agent_id', postgresql.UUID(as_uuid=True), nullable=False),
        sa.Column('role', sa.String(20), nullable=False),
        sa.Column('content', sa.Text, nullable=False),
        sa.Column('tool_calls', postgresql.JSONB),
        sa.Column('tool_call_id', sa.String(100)),
        sa.Column('tool_name', sa.String(100)),
        sa.Column('created_at', sa.TIMESTAMP(timezone=True), server_default=sa.text('CURRENT_TIMESTAMP')),
        sa.Column('prompt_tokens', sa.Integer),
        sa.Column('completion_tokens', sa.Integer),
        sa.Column('total_tokens', sa.Integer),
        sa.ForeignKeyConstraint(['agent_id'], ['agents.id'], ondelete='CASCADE'),
        sa.CheckConstraint("role IN ('user', 'assistant', 'system', 'tool')", name='messages_role_check')
    )
    op.create_index('idx_messages_agent_id', 'messages', ['agent_id'])
    op.create_index('idx_messages_created_at', 'messages', ['created_at'])
    op.create_index('idx_messages_role', 'messages', ['role'])
    op.create_index('idx_messages_agent_created', 'messages', ['agent_id', sa.text('created_at DESC')])

    # Create memory_blocks table
    op.create_table(
        'memory_blocks',
        sa.Column('id', postgresql.UUID(as_uuid=True), primary_key=True, server_default=sa.text('uuid_generate_v4()')),
        sa.Column('agent_id', postgresql.UUID(as_uuid=True), nullable=False),
        sa.Column('label', sa.String(100), nullable=False),
        sa.Column('value', sa.Text, nullable=False),
        sa.Column('is_template', sa.Boolean, server_default='false'),
        sa.Column('is_readonly', sa.Boolean, server_default='false'),
        sa.Column('created_at', sa.TIMESTAMP(timezone=True), server_default=sa.text('CURRENT_TIMESTAMP')),
        sa.Column('updated_at', sa.TIMESTAMP(timezone=True), server_default=sa.text('CURRENT_TIMESTAMP')),
        sa.ForeignKeyConstraint(['agent_id'], ['agents.id'], ondelete='CASCADE'),
        sa.UniqueConstraint('agent_id', 'label', name='memory_blocks_unique_label'),
        sa.CheckConstraint("char_length(label) >= 1 AND char_length(label) <= 100", name='memory_blocks_label_check')
    )
    op.create_index('idx_memory_blocks_agent_id', 'memory_blocks', ['agent_id'])
    op.create_index('idx_memory_blocks_label', 'memory_blocks', ['label'])

    # Create archival_memory table
    op.create_table(
        'archival_memory',
        sa.Column('id', postgresql.UUID(as_uuid=True), primary_key=True, server_default=sa.text('uuid_generate_v4()')),
        sa.Column('agent_id', postgresql.UUID(as_uuid=True), nullable=False),
        sa.Column('content', sa.Text, nullable=False),
        sa.Column('embedding', sa.Text),  # Will be vector(1536) via raw SQL
        sa.Column('importance', sa.Float, server_default='0.5'),
        sa.Column('tags', postgresql.ARRAY(sa.Text)),
        sa.Column('created_at', sa.TIMESTAMP(timezone=True), server_default=sa.text('CURRENT_TIMESTAMP')),
        sa.ForeignKeyConstraint(['agent_id'], ['agents.id'], ondelete='CASCADE'),
        sa.CheckConstraint("importance >= 0.0 AND importance <= 1.0", name='archival_memory_importance_check')
    )

    # Convert embedding column to vector type
    op.execute('ALTER TABLE archival_memory ALTER COLUMN embedding TYPE vector(1536) USING embedding::vector(1536)')

    op.create_index('idx_archival_memory_agent_id', 'archival_memory', ['agent_id'])
    op.create_index('idx_archival_memory_created_at', 'archival_memory', ['created_at'])
    op.create_index('idx_archival_memory_importance', 'archival_memory', ['importance'])
    op.create_index('idx_archival_memory_tags', 'archival_memory', ['tags'], postgresql_using='gin')

    # Create vector similarity search index
    op.execute('''
        CREATE INDEX idx_archival_memory_embedding ON archival_memory
        USING ivfflat (embedding vector_cosine_ops)
        WITH (lists = 100)
    ''')

    # Create recall_memory table
    op.create_table(
        'recall_memory',
        sa.Column('id', postgresql.UUID(as_uuid=True), primary_key=True, server_default=sa.text('uuid_generate_v4()')),
        sa.Column('agent_id', postgresql.UUID(as_uuid=True), nullable=False),
        sa.Column('content', sa.Text, nullable=False),
        sa.Column('message_ids', postgresql.ARRAY(postgresql.UUID(as_uuid=True))),
        sa.Column('created_at', sa.TIMESTAMP(timezone=True), server_default=sa.text('CURRENT_TIMESTAMP')),
        sa.Column('expires_at', sa.TIMESTAMP(timezone=True)),
        sa.ForeignKeyConstraint(['agent_id'], ['agents.id'], ondelete='CASCADE')
    )
    op.create_index('idx_recall_memory_agent_id', 'recall_memory', ['agent_id'])
    op.create_index('idx_recall_memory_created_at', 'recall_memory', ['created_at'])
    op.create_index('idx_recall_memory_expires_at', 'recall_memory', ['expires_at'], postgresql_where=sa.text('expires_at IS NOT NULL'))

    # Create agent_checkpoints table
    op.create_table(
        'agent_checkpoints',
        sa.Column('id', postgresql.UUID(as_uuid=True), primary_key=True, server_default=sa.text('uuid_generate_v4()')),
        sa.Column('agent_id', postgresql.UUID(as_uuid=True), nullable=False),
        sa.Column('checkpoint_data', postgresql.JSONB, nullable=False),
        sa.Column('checkpoint_type', sa.String(50), nullable=False),
        sa.Column('description', sa.Text),
        sa.Column('created_at', sa.TIMESTAMP(timezone=True), server_default=sa.text('CURRENT_TIMESTAMP')),
        sa.ForeignKeyConstraint(['agent_id'], ['agents.id'], ondelete='CASCADE'),
        sa.CheckConstraint("checkpoint_type IN ('manual', 'auto', 'pre_evolution')", name='agent_checkpoints_type_check')
    )
    op.create_index('idx_agent_checkpoints_agent_id', 'agent_checkpoints', ['agent_id'])
    op.create_index('idx_agent_checkpoints_created_at', 'agent_checkpoints', ['created_at'])
    op.create_index('idx_agent_checkpoints_type', 'agent_checkpoints', ['checkpoint_type'])

    # Create evolution_events table
    op.create_table(
        'evolution_events',
        sa.Column('id', postgresql.UUID(as_uuid=True), primary_key=True, server_default=sa.text('uuid_generate_v4()')),
        sa.Column('agent_id', postgresql.UUID(as_uuid=True), nullable=False),
        sa.Column('hypothesis', sa.Text, nullable=False),
        sa.Column('code_changes', postgresql.JSONB, nullable=False),
        sa.Column('improvement', sa.Float),
        sa.Column('success', sa.Boolean, nullable=False),
        sa.Column('checkpoint_id', postgresql.UUID(as_uuid=True)),
        sa.Column('created_at', sa.TIMESTAMP(timezone=True), server_default=sa.text('CURRENT_TIMESTAMP')),
        sa.ForeignKeyConstraint(['agent_id'], ['agents.id'], ondelete='CASCADE'),
        sa.ForeignKeyConstraint(['checkpoint_id'], ['agent_checkpoints.id'])
    )
    op.create_index('idx_evolution_events_agent_id', 'evolution_events', ['agent_id'])
    op.create_index('idx_evolution_events_created_at', 'evolution_events', ['created_at'])
    op.create_index('idx_evolution_events_success', 'evolution_events', ['success'])

    # Create agent_statistics table
    op.create_table(
        'agent_statistics',
        sa.Column('id', postgresql.UUID(as_uuid=True), primary_key=True, server_default=sa.text('uuid_generate_v4()')),
        sa.Column('agent_id', postgresql.UUID(as_uuid=True), nullable=False),
        sa.Column('total_messages', sa.Integer, server_default='0'),
        sa.Column('user_messages', sa.Integer, server_default='0'),
        sa.Column('assistant_messages', sa.Integer, server_default='0'),
        sa.Column('total_tokens', sa.BigInteger, server_default='0'),
        sa.Column('prompt_tokens', sa.BigInteger, server_default='0'),
        sa.Column('completion_tokens', sa.BigInteger, server_default='0'),
        sa.Column('archival_memory_count', sa.Integer, server_default='0'),
        sa.Column('recall_memory_count', sa.Integer, server_default='0'),
        sa.Column('evolution_attempts', sa.Integer, server_default='0'),
        sa.Column('successful_evolutions', sa.Integer, server_default='0'),
        sa.Column('first_message_at', sa.TIMESTAMP(timezone=True)),
        sa.Column('last_message_at', sa.TIMESTAMP(timezone=True)),
        sa.Column('last_evolution_at', sa.TIMESTAMP(timezone=True)),
        sa.Column('updated_at', sa.TIMESTAMP(timezone=True), server_default=sa.text('CURRENT_TIMESTAMP')),
        sa.ForeignKeyConstraint(['agent_id'], ['agents.id'], ondelete='CASCADE'),
        sa.UniqueConstraint('agent_id', name='agent_statistics_unique_agent')
    )
    op.create_index('idx_agent_statistics_agent_id', 'agent_statistics', ['agent_id'])

    # Create triggers
    op.execute('''
        CREATE OR REPLACE FUNCTION update_updated_at_column()
        RETURNS TRIGGER AS $$
        BEGIN
            NEW.updated_at = CURRENT_TIMESTAMP;
            RETURN NEW;
        END;
        $$ language 'plpgsql'
    ''')

    op.execute('CREATE TRIGGER update_agents_updated_at BEFORE UPDATE ON agents FOR EACH ROW EXECUTE FUNCTION update_updated_at_column()')
    op.execute('CREATE TRIGGER update_memory_blocks_updated_at BEFORE UPDATE ON memory_blocks FOR EACH ROW EXECUTE FUNCTION update_updated_at_column()')
    op.execute('CREATE TRIGGER update_agent_statistics_updated_at BEFORE UPDATE ON agent_statistics FOR EACH ROW EXECUTE FUNCTION update_updated_at_column()')

    # Create views
    op.execute('''
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
        WHERE a.deleted_at IS NULL
    ''')

    op.execute('''
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
        ORDER BY m.created_at DESC
    ''')

    # Create utility functions
    op.execute('''
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
        $$ LANGUAGE plpgsql
    ''')

    op.execute('''
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
        $$ LANGUAGE plpgsql
    ''')


def downgrade():
    # Drop views
    op.execute('DROP VIEW IF EXISTS recent_messages')
    op.execute('DROP VIEW IF EXISTS agent_summary')

    # Drop functions
    op.execute('DROP FUNCTION IF EXISTS prune_archival_memory')
    op.execute('DROP FUNCTION IF EXISTS cleanup_expired_recall_memory')
    op.execute('DROP FUNCTION IF EXISTS update_updated_at_column CASCADE')

    # Drop tables
    op.drop_table('agent_statistics')
    op.drop_table('evolution_events')
    op.drop_table('agent_checkpoints')
    op.drop_table('recall_memory')
    op.drop_table('archival_memory')
    op.drop_table('memory_blocks')
    op.drop_table('messages')
    op.drop_table('agents')

    # Drop extensions
    op.execute('DROP EXTENSION IF EXISTS "pgvector"')
    op.execute('DROP EXTENSION IF EXISTS "uuid-ossp"')
