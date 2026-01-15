# Gerbil Database Client

Database client library for Project O, providing high-level database operations for agents, messages, and memory.

## Overview

The database client provides:
- **Agent Management** - Create, read, update, delete agents
- **Message Operations** - Store and retrieve conversation messages
- **Memory Management** - Core memory blocks and archival memory
- **Statistics Tracking** - Agent usage statistics

## Architecture

```
Gerbil Application
       ↓
database/agents.ss    (High-level agent operations)
database/messages.ss  (High-level message operations)
       ↓
database/client.ss    (Low-level database client)
       ↓
MessagePack Protocol
       ↓
Elixir Database Module (o_supervisor/lib/o_supervisor/database.ex)
       ↓
PostgreSQL Database
```

## Installation

No installation required - part of Project O's Gerbil codebase.

## Usage

### Connecting to Database

```scheme
(import :gerbil/database/client)

;; Connect to Elixir database service
(db-connect! host: "localhost" port: 9000)

;; Check connection
(db-connected?)  ;; => #t

;; Disconnect
(db-disconnect!)
```

### Agent Operations

```scheme
(import :gerbil/database/agents)

;; Create agent
(def agent (agent-create! "MyAgent"
                          llm-provider: "openai"
                          llm-model: "gpt-4"
                          system-prompt: "You are a helpful assistant."
                          persona: "You are a helpful AI assistant."
                          human: "User prefers concise responses."))

(def agent-id (hash-ref agent 'id))

;; Get agent
(def agent (agent-get agent-id))

;; Update agent
(agent-update! agent-id (hash 'name "UpdatedAgent"))

;; Update agent config
(agent-update-config! agent-id
                      (hash 'llm_config (hash 'temperature 0.8)
                            'system_prompt "You are very helpful."))

;; Get agent memory
(def memory (agent-get-memory agent-id))

;; Update agent memory
(agent-update-memory! agent-id
                      (hash 'core_memory
                            (hash 'persona "You are very helpful."
                                  'human "User likes details.")))

;; Get agent statistics
(def stats (agent-get-stats agent-id))

;; List agents
(def agents (agent-list limit: 10 offset: 0))

;; Delete agent
(agent-delete! agent-id)
```

### Message Operations

```scheme
(import :gerbil/database/messages)

;; Create user message
(message-user! agent-id "Hello!" tokens: 5)

;; Create assistant message
(message-assistant! agent-id "Hi there!"
                    prompt-tokens: 10
                    completion-tokens: 15)

;; Create system message
(message-system! agent-id "System notification")

;; Create tool response message
(message-tool! agent-id "Result: 42" "call-123" "calculate")

;; Get conversation
(def messages (conversation-get agent-id limit: 100))

;; Get recent messages
(def recent (conversation-get-recent agent-id 10))

;; Get messages by role
(def user-messages (conversation-get-by-role agent-id "user"))

;; Search messages
(def results (message-search agent-id "hello" limit: 10))

;; Get conversation stats
(def stats (conversation-get-stats agent-id))

;; Export conversation
(def json-export (conversation-export agent-id format: 'json))
(def text-export (conversation-export agent-id format: 'text))

;; Clear conversation
(conversation-clear! agent-id)
```

### Memory Operations

```scheme
(import :gerbil/database/client)

;; Get core memory
(def core-memory (db-get-core-memory agent-id))
;; => (hash 'persona "..." 'human "...")

;; Update core memory
(db-update-core-memory agent-id "persona" "New persona")

;; Insert archival memory
(db-insert-archival-memory agent-id
                           (hash 'content "Important info"
                                 'importance 0.8
                                 'tags '("important" "user_preference")))

;; Search archival memory
(def results (db-search-archival-memory agent-id "important"
                                        limit: 10
                                        search-type: 'text))
```

## API Reference

### database/client.ss (Low-level)

#### Connection

- `(db-connect! #!key (host "localhost") (port 9000))` - Connect to database
- `(db-disconnect!)` - Disconnect from database
- `(db-connected?)` - Check connection status

#### Agent Operations

- `(db-create-agent params)` - Create agent
- `(db-get-agent agent-id)` - Get agent by ID
- `(db-update-agent agent-id params)` - Update agent
- `(db-delete-agent agent-id)` - Delete agent (soft delete)
- `(db-list-agents #!key (limit 10) (offset 0))` - List agents

#### Message Operations

- `(db-create-message agent-id params)` - Create message
- `(db-get-messages agent-id #!key (limit 50) (offset 0) (role #f))` - Get messages
- `(db-delete-message message-id)` - Delete message

#### Memory Operations

- `(db-create-memory-block agent-id params)` - Create/update memory block
- `(db-get-memory-blocks agent-id)` - Get all memory blocks
- `(db-update-memory-block agent-id label value)` - Update memory block
- `(db-insert-archival-memory agent-id params)` - Insert archival memory
- `(db-search-archival-memory agent-id query #!key (limit 10) (search-type 'text))` - Search archival memory

#### Statistics Operations

- `(db-get-agent-statistics agent-id)` - Get agent statistics
- `(db-update-statistics agent-id updates)` - Update statistics

#### Convenience Functions

- `(db-agent-exists? agent-id)` - Check if agent exists
- `(db-get-conversation agent-id #!key (limit 100))` - Get conversation history
- `(db-get-core-memory agent-id)` - Get core memory as hash
- `(db-update-core-memory agent-id label value)` - Update core memory

### database/agents.ss (High-level)

#### Agent Management

- `(agent-create! name #!key ...)` - Create agent with defaults
- `(agent-get agent-id)` - Get agent
- `(agent-update! agent-id updates)` - Update agent
- `(agent-delete! agent-id)` - Delete agent
- `(agent-list #!key (limit 10) (offset 0))` - List agents

#### Configuration

- `(agent-get-config agent-id)` - Get agent configuration
- `(agent-update-config! agent-id config)` - Update configuration

#### Memory

- `(agent-get-memory agent-id)` - Get agent memory
- `(agent-update-memory! agent-id memory)` - Update memory

#### Statistics

- `(agent-get-stats agent-id)` - Get statistics
- `(agent-increment-stat! agent-id stat-name #!optional (amount 1))` - Increment stat

#### Validation

- `(agent-exists? agent-id)` - Check existence
- `(agent-validate-config config)` - Validate configuration

### database/messages.ss (High-level)

#### Message Management

- `(message-create! agent-id role content #!key ...)` - Create message
- `(message-get-list agent-id #!key ...)` - Get message list
- `(message-delete! message-id)` - Delete message

#### Conversation Management

- `(conversation-get agent-id #!key (limit 100))` - Get conversation
- `(conversation-clear! agent-id)` - Clear conversation
- `(conversation-get-recent agent-id n)` - Get N recent messages
- `(conversation-get-by-role agent-id role #!key (limit 50))` - Get by role

#### Message Helpers

- `(message-user! agent-id content #!key (tokens 0))` - Create user message
- `(message-assistant! agent-id content #!key ...)` - Create assistant message
- `(message-system! agent-id content)` - Create system message
- `(message-tool! agent-id content tool-call-id tool-name)` - Create tool message

#### Analysis

- `(conversation-count-messages agent-id)` - Count messages
- `(conversation-count-tokens agent-id)` - Count tokens
- `(conversation-get-stats agent-id)` - Get statistics

#### Search

- `(message-search agent-id query #!key (limit 10))` - Search messages
- `(message-search-by-date agent-id start-date end-date)` - Search by date

#### Export/Import

- `(conversation-export agent-id #!key (format 'json))` - Export conversation
- `(conversation-import! agent-id conversation-data)` - Import conversation

#### Validation

- `(message-validate params)` - Validate message parameters

## Data Structures

### Agent

```scheme
(hash
 'id "uuid"
 'name "MyAgent"
 'llm_provider "openai"
 'llm_model "gpt-4"
 'llm_temperature 0.7
 'llm_max_tokens 4096
 'system_prompt "You are helpful."
 'created_at 1705401600
 'updated_at 1705401600)
```

### Message

```scheme
(hash
 'id "uuid"
 'agent_id "uuid"
 'role "user"
 'content "Hello!"
 'tool_calls #f
 'created_at 1705401600
 'total_tokens 5)
```

### Memory Block

```scheme
(hash
 'id "uuid"
 'agent_id "uuid"
 'label "persona"
 'value "You are helpful."
 'is_template #f
 'is_readonly #f
 'created_at 1705401600
 'updated_at 1705401600)
```

### Statistics

```scheme
(hash
 'agent_id "uuid"
 'total_messages 100
 'user_messages 50
 'assistant_messages 50
 'total_tokens 5000
 'prompt_tokens 2500
 'completion_tokens 2500
 'archival_memory_count 10
 'recall_memory_count 5
 'first_message_at 1705315200
 'last_message_at 1705401600)
```

## Error Handling

All operations may throw errors. Use `try/catch` for error handling:

```scheme
(try
 (agent-get "invalid-id")
 (catch (e)
   (displayln (format "Error: ~a" (error-message e)))))
```

## Protocol

The client communicates with Elixir via MessagePack protocol:

**Request:**
```scheme
(hash
 'type "db_request"
 'request_id 1
 'operation 'create_agent
 'params (hash 'name "MyAgent"))
```

**Response:**
```scheme
(hash
 'success #t
 'data (hash 'id "uuid" 'name "MyAgent"))
```

**Error Response:**
```scheme
(hash
 'success #f
 'error "Agent not found")
```

## Testing

```scheme
;; Test agent operations
(db-connect!)

(def agent (agent-create! "TestAgent"))
(def agent-id (hash-ref agent 'id))

(message-user! agent-id "Test message")
(def messages (conversation-get agent-id))

(assert (= (length messages) 1))

(agent-delete! agent-id)
(db-disconnect!)
```

## Performance

### Connection Pooling

The Elixir database module uses connection pooling. Configure pool size in Elixir config.

### Batch Operations

For bulk operations, use batch functions:

```scheme
;; Create multiple messages
(conversation-create-from-list! agent-id
  (list
   (hash 'role "user" 'content "Hello")
   (hash 'role "assistant" 'content "Hi")))
```

### Caching

Consider caching frequently accessed data:

```scheme
(def *agent-cache* (hash))

(def (agent-get-cached agent-id)
  (or (hash-ref *agent-cache* agent-id #f)
      (let ((agent (agent-get agent-id)))
        (hash-put! *agent-cache* agent-id agent)
        agent)))
```

## Integration

### With Agent Core

```scheme
(import :gerbil/agent/core
        :gerbil/database/agents
        :gerbil/database/messages)

;; Create agent with database persistence
(def agent (agent-create! "MyAgent"))
(def agent-id (hash-ref agent 'id))

;; Store messages
(message-user! agent-id "Hello")
(message-assistant! agent-id "Hi there!")

;; Restore agent from database
(def restored-agent (agent-get agent-id))
(def conversation (conversation-get agent-id))
```

### With LLM Clients

```scheme
(import :gerbil/llm/client
        :gerbil/database/messages)

;; Send message to LLM and store
(def response (llm-simple :openai "Hello"))
(message-user! agent-id "Hello" tokens: 5)
(message-assistant! agent-id response
                    prompt-tokens: 5
                    completion-tokens: 10)
```

## Future Enhancements

- [ ] Connection retry logic
- [ ] Request timeout handling
- [ ] Response caching
- [ ] Batch operation optimization
- [ ] Streaming support
- [ ] Transaction support
- [ ] Query builder
- [ ] ORM-like interface

## License

Part of Project O - Self-Evolving AI Agent Platform
