# Gerbil Message Manager

Advanced message management system for Project O, providing high-level message operations with caching, pagination, search, and validation.

## Overview

The message manager provides:
- **Message Creation** - Create user, assistant, system, and tool messages
- **Message Retrieval** - Get messages with filtering and pagination
- **Message Search** - Text-based search with context
- **Message Caching** - In-memory cache for performance
- **Message Validation** - Validate message parameters and conversation consistency
- **Statistics** - Track message counts, tokens, and distribution
- **Export/Import** - Export conversations to JSON/text formats

## Architecture

```
Application
    ↓
message/manager.ss    (High-level message manager)
    ↓
message/types.ss      (Type definitions and validation)
    ↓
database/messages.ss  (Database operations)
    ↓
database/client.ss    (Low-level database client)
    ↓
PostgreSQL Database
```

## Installation

No installation required - part of Project O's Gerbil codebase.

## Usage

### Creating a Message Manager

```scheme
(import :gerbil/message/manager
        :gerbil/database/client)

;; Connect to database
(db-connect!)

;; Create message manager for an agent
(def manager (make-manager agent-id cache-size: 100))
```

### Creating Messages

```scheme
;; Create user message
(manager-create-user-message! manager "Hello!" tokens: 5)

;; Create assistant message
(manager-create-assistant-message! manager "Hi there!"
                                   prompt-tokens: 10
                                   completion-tokens: 15)

;; Create assistant message with tool calls
(manager-create-assistant-message! manager "Let me search for that."
                                   tool-calls: (list
                                                (hash 'id "call-123"
                                                      'type "function"
                                                      'function (hash 'name "search"
                                                                     'arguments "{\"query\":\"test\"}")))
                                   prompt-tokens: 20
                                   completion-tokens: 10)

;; Create system message
(manager-create-system-message! manager "System notification")

;; Create tool response message
(manager-create-tool-message! manager "Result: 42" "call-123" "calculate")
```

### Retrieving Messages

```scheme
;; Get messages with limit
(def messages (manager-get-messages manager limit: 50))

;; Get full conversation
(def conversation (manager-get-conversation manager))

;; Get recent messages
(def recent (manager-get-recent manager 10))

;; Get messages by role
(def user-messages (manager-get-by-role manager "user" limit: 50))
(def assistant-messages (manager-get-by-role manager "assistant"))
```

### Searching Messages

```scheme
;; Text search
(def results (manager-search manager "hello" limit: 10))

;; Search by date range
(def date-results (manager-search-by-date manager 1705315200 1705401600))

;; Search with context (includes surrounding messages)
(def context-results (manager-search-with-context manager "error" context-size: 2))

;; Find tool calls
(def tool-calls (manager-find-tool-calls manager tool-name: "search" limit: 50))

;; Find tool responses
(def tool-responses (manager-find-tool-responses manager "call-123"))
```

### Filtering Messages

```scheme
(import :gerbil/message/types)

;; Create filter
(def filter (make-message-filter
             role: "user"
             start-date: 1705315200
             end-date: 1705401600
             search-text: "hello"
             limit: 20
             offset: 0))

;; Find messages matching filter
(def filtered (manager-find-messages manager filter))
```

### Pagination

```scheme
;; Get first page (20 messages per page)
(def page (manager-get-page manager 0 20))

(displayln (format "Page ~a of ~a total messages"
                   (page-result-page page)
                   (page-result-total page)))

(displayln (format "Has next page: ~a" (page-result-has-next? page)))
(displayln (format "Has previous page: ~a" (page-result-has-prev? page)))

;; Get messages from page
(def page-messages (page-result-messages page))

;; Navigate pages
(def next-page-num (manager-get-next-page page))
(def prev-page-num (manager-get-prev-page page))

;; Get next page
(when next-page-num
  (def next-page (manager-get-page manager next-page-num 20)))
```

### Statistics

```scheme
;; Get conversation statistics
(def stats (manager-get-stats manager))
(displayln (format "Total messages: ~a" (hash-ref stats 'total_messages)))
(displayln (format "User messages: ~a" (hash-ref stats 'user_messages)))
(displayln (format "Assistant messages: ~a" (hash-ref stats 'assistant_messages)))
(displayln (format "Total tokens: ~a" (hash-ref stats 'total_tokens)))
(displayln (format "Prompt tokens: ~a" (hash-ref stats 'prompt_tokens)))
(displayln (format "Completion tokens: ~a" (hash-ref stats 'completion_tokens)))

;; Count messages
(def message-count (manager-count-messages manager))

;; Count tokens
(def token-count (manager-count-tokens manager))

;; Get message distribution
(def distribution (manager-get-message-distribution manager))

;; Get conversation summary
(def summary (manager-get-conversation-summary manager))
```

### Context Window Management

```scheme
;; Get messages that fit within token limit
(def context (manager-get-context-window manager 4000))
(displayln (format "Messages in context: ~a" (length context)))

;; Truncate to message limit
(def truncated (manager-truncate-to-limit manager 50))

;; Get messages since timestamp
(def since (manager-get-messages-since manager 1705315200))

;; Get messages before timestamp
(def before (manager-get-messages-before manager 1705401600))

;; Get messages between timestamps
(def between (manager-get-messages-between manager 1705315200 1705401600))
```

### Export and Import

```scheme
;; Export conversation to JSON
(def json-export (manager-export-conversation manager format: 'json))

;; Export conversation to text
(def text-export (manager-export-conversation manager format: 'text))

;; Import conversation
(def conversation-data (hash 'messages (list ...)))
(manager-import-conversation! manager conversation-data)
```

### Message Formatting

```scheme
;; Format single message
(def formatted-msg (manager-format-message msg format: 'text))
(def json-msg (manager-format-message msg format: 'json))
(def markdown-msg (manager-format-message msg format: 'markdown))

;; Format entire conversation
(def text-conv (manager-format-conversation manager format: 'text))
(def json-conv (manager-format-conversation manager format: 'json))
(def markdown-conv (manager-format-conversation manager format: 'markdown))

(displayln markdown-conv)
```

### Message Validation

```scheme
;; Validate message parameters
(def params (hash 'role "user" 'content "Hello!"))
(def result (manager-validate-message manager params))

(if (car result)
    (displayln "Valid message")
    (displayln (format "Invalid: ~a" (cdr result))))

;; Validate entire conversation
(def validation (manager-validate-conversation manager))

(if (car validation)
    (displayln "Conversation is valid")
    (displayln (format "Validation errors: ~a" (cdr validation))))
```

### Batch Operations

```scheme
;; Create multiple messages
(def message-list (list
                   (hash 'role "user" 'content "Hello")
                   (hash 'role "assistant" 'content "Hi"
                         'prompt_tokens 5 'completion_tokens 3)))

(manager-create-messages-batch! manager message-list)

;; Delete multiple messages
(def message-ids (list "uuid-1" "uuid-2" "uuid-3"))
(manager-delete-messages-batch! manager message-ids)
```

### Caching

```scheme
;; Messages are automatically cached when created or retrieved

;; Get cached message
(def cached-msg (manager-get-cached manager msg-id))

;; Clear cache
(manager-clear-cache! manager)

;; Cache is automatically evicted when full (LRU policy)
```

### Conversation Management

```scheme
;; Get last user message
(def last-user (manager-get-last-user-message manager))

;; Get last assistant message
(def last-assistant (manager-get-last-assistant-message manager))

;; Clear entire conversation
(manager-clear-conversation! manager)
```

## API Reference

### Message Manager

#### Constructor

- `(make-manager agent-id #!key (cache-size 100))` - Create message manager

#### Message Creation

- `(manager-create-message! manager role content #!key ...)` - Create message with validation
- `(manager-create-user-message! manager content #!key (tokens 0))` - Create user message
- `(manager-create-assistant-message! manager content #!key ...)` - Create assistant message
- `(manager-create-system-message! manager content)` - Create system message
- `(manager-create-tool-message! manager content tool-call-id tool-name)` - Create tool message

#### Message Retrieval

- `(manager-get-messages manager #!key (limit 50) (offset 0) (role #f))` - Get messages
- `(manager-get-conversation manager #!key (limit 100))` - Get full conversation
- `(manager-get-recent manager n)` - Get N recent messages
- `(manager-get-by-role manager role #!key (limit 50))` - Get messages by role

#### Message Search

- `(manager-search manager query #!key (limit 10))` - Search by content
- `(manager-search-by-date manager start-date end-date)` - Search by date range
- `(manager-search-with-context manager query #!key (context-size 2))` - Search with context
- `(manager-filter-messages manager messages filter)` - Filter messages
- `(manager-find-messages manager filter)` - Find messages matching filter

#### Pagination

- `(manager-get-page manager page-number page-size #!key ...)` - Get paginated messages
- `(manager-get-next-page page-result)` - Get next page number
- `(manager-get-prev-page page-result)` - Get previous page number

#### Statistics

- `(manager-get-stats manager)` - Get conversation statistics
- `(manager-count-messages manager)` - Count total messages
- `(manager-count-tokens manager)` - Count total tokens
- `(manager-calculate-stats manager messages)` - Calculate statistics from list
- `(manager-get-message-distribution manager)` - Get message distribution by role

#### Context Window

- `(manager-get-context-window manager max-tokens)` - Get messages within token limit
- `(manager-truncate-to-limit manager max-messages)` - Get recent N messages
- `(manager-get-messages-since manager timestamp)` - Get messages since timestamp
- `(manager-get-messages-before manager timestamp)` - Get messages before timestamp
- `(manager-get-messages-between manager start end)` - Get messages between timestamps

#### Export/Import

- `(manager-export-conversation manager #!key (format 'json))` - Export conversation
- `(manager-import-conversation! manager conversation-data)` - Import conversation

#### Formatting

- `(manager-format-message msg #!key (format 'text))` - Format single message
- `(manager-format-conversation manager #!key (format 'text) (limit 100))` - Format conversation

#### Validation

- `(manager-validate-message manager msg-params)` - Validate message parameters
- `(manager-validate-conversation manager)` - Validate conversation consistency

#### Batch Operations

- `(manager-create-messages-batch! manager message-list)` - Create multiple messages
- `(manager-delete-messages-batch! manager message-ids)` - Delete multiple messages

#### Caching

- `(manager-cache-message! manager msg)` - Add message to cache
- `(manager-get-cached manager msg-id)` - Get cached message
- `(manager-clear-cache! manager)` - Clear cache

#### Conversation Management

- `(manager-clear-conversation! manager)` - Clear all messages
- `(manager-get-conversation-summary manager)` - Get conversation summary
- `(manager-get-last-user-message manager)` - Get last user message
- `(manager-get-last-assistant-message manager)` - Get last assistant message

#### Tool Operations

- `(manager-find-tool-calls manager #!key (tool-name #f) (limit 50))` - Find tool calls
- `(manager-find-tool-responses manager tool-call-id)` - Find tool responses

### Message Types

#### Structures

```scheme
(defstruct message
  (id agent-id role content tool-calls tool-call-id tool-name
   prompt-tokens completion-tokens total-tokens created-at)
  transparent: #t)

(defstruct message-filter
  (role start-date end-date search-text has-tool-calls limit offset)
  transparent: #t)

(defstruct message-stats
  (total-messages user-messages assistant-messages system-messages tool-messages
   total-tokens prompt-tokens completion-tokens first-message-at last-message-at)
  transparent: #t)

(defstruct page-result
  (messages total page page-size has-next? has-prev?)
  transparent: #t)
```

#### Validation

- `(valid-role? role)` - Check if role is valid
- `(validate-message-params params)` - Validate message parameters
- `(filter-matches? message filter)` - Check if message matches filter

#### Statistics

- `(make-empty-stats)` - Create empty statistics
- `(calculate-stats messages)` - Calculate statistics from messages

#### Conversion

- `(hash->message h)` - Convert hash to message struct
- `(message->hash msg)` - Convert message struct to hash

#### Utilities

- `(message-is-user? msg)` - Check if message is from user
- `(message-is-assistant? msg)` - Check if message is from assistant
- `(message-is-system? msg)` - Check if message is system message
- `(message-is-tool? msg)` - Check if message is tool response
- `(message-has-tool-calls? msg)` - Check if message has tool calls
- `(message-token-count msg)` - Get total token count

## Data Structures

### Message

```scheme
(hash
 'id "uuid"
 'agent_id "uuid"
 'role "user"
 'content "Hello!"
 'tool_calls #f
 'tool_call_id #f
 'tool_name #f
 'prompt_tokens 5
 'completion_tokens 0
 'total_tokens 5
 'created_at 1705401600)
```

### Message Filter

```scheme
(make-message-filter
 role: "user"
 start-date: 1705315200
 end-date: 1705401600
 search-text: "hello"
 has-tool-calls: #f
 limit: 50
 offset: 0)
```

### Page Result

```scheme
(make-page-result
 messages: (list ...)
 total: 100
 page: 0
 page-size: 20
 has-next?: #t
 has-prev?: #f)
```

### Message Statistics

```scheme
(hash
 'total_messages 100
 'user_messages 50
 'assistant_messages 50
 'system_messages 0
 'tool_messages 0
 'total_tokens 5000
 'prompt_tokens 2500
 'completion_tokens 2500
 'first_message_at 1705315200
 'last_message_at 1705401600)
```

## Features

### Message Caching

The message manager includes an in-memory cache for performance:

- **Automatic Caching**: Messages are cached when created or retrieved
- **LRU Eviction**: Oldest messages are evicted when cache is full
- **Configurable Size**: Set cache size when creating manager
- **Manual Control**: Clear cache manually when needed

### Message Validation

Comprehensive validation ensures data integrity:

- **Parameter Validation**: Validates role, content, and tool fields
- **Conversation Validation**: Checks for orphaned tool responses
- **Type Checking**: Ensures correct data types
- **Error Reporting**: Returns detailed error messages

### Context Window Management

Intelligent context window management for LLM inference:

- **Token-Based**: Get messages that fit within token limit
- **Recent Messages**: Get N most recent messages
- **Time-Based**: Get messages within time range
- **Automatic Truncation**: Handles context overflow

### Search Capabilities

Powerful search features:

- **Text Search**: Search message content
- **Date Range**: Search by time period
- **Context Search**: Include surrounding messages
- **Tool Search**: Find tool calls and responses
- **Filtering**: Complex filtering with multiple criteria

### Export Formats

Multiple export formats supported:

- **JSON**: Machine-readable format
- **Text**: Human-readable plain text
- **Markdown**: Formatted markdown

## Performance

### Caching Strategy

- Default cache size: 100 messages
- LRU eviction policy
- Automatic cache management
- Manual cache control available

### Database Queries

- Efficient pagination with limit/offset
- Indexed queries for fast retrieval
- Batch operations for bulk updates
- Connection pooling via Elixir

### Optimization Tips

1. **Use Pagination**: Don't load all messages at once
2. **Set Appropriate Cache Size**: Balance memory vs performance
3. **Use Filters**: Reduce data transfer with filtering
4. **Batch Operations**: Use batch functions for multiple operations
5. **Context Window**: Use token-based retrieval for LLM inference

## Integration

### With Database Client

```scheme
(import :gerbil/database/client
        :gerbil/message/manager)

;; Connect to database
(db-connect!)

;; Create manager
(def manager (make-manager agent-id))

;; Manager uses database client internally
```

### With LLM Clients

```scheme
(import :gerbil/llm/client
        :gerbil/message/manager)

;; Get context for LLM
(def context (manager-get-context-window manager 4000))

;; Send to LLM
(def response (llm-chat-completion :openai "gpt-4" context))

;; Store response
(manager-create-assistant-message! manager
                                   (llm-response-content response)
                                   prompt-tokens: (llm-response-prompt-tokens response)
                                   completion-tokens: (llm-response-completion-tokens response))
```

### With Agent Core

```scheme
(import :gerbil/agent/core
        :gerbil/message/manager)

;; Create agent with message manager
(def agent (make-agent ...))
(def manager (make-manager (agent-id agent)))

;; Use manager for conversation
(manager-create-user-message! manager user-input)
;; ... agent processing ...
(manager-create-assistant-message! manager agent-response)
```

## Error Handling

All operations may throw errors. Use `try/catch` for error handling:

```scheme
(try
 (manager-create-user-message! manager "")
 (catch (e)
   (displayln (format "Error: ~a" (error-message e)))))
```

## Testing

```scheme
;; Test message creation
(def manager (make-manager agent-id))
(def msg (manager-create-user-message! manager "Test" tokens: 5))
(assert (equal? (hash-ref msg 'role) "user"))

;; Test pagination
(def page (manager-get-page manager 0 20))
(assert (page-result? page))

;; Test search
(def results (manager-search manager "test"))
(assert (list? results))

;; Test validation
(def params (hash 'role "user" 'content "Test"))
(def result (manager-validate-message manager params))
(assert (car result))
```

## Future Enhancements

- [ ] Async message operations
- [ ] Message threading support
- [ ] Message reactions/annotations
- [ ] Advanced search (regex, fuzzy)
- [ ] Message compression
- [ ] Streaming message creation
- [ ] Message templates
- [ ] Conversation branching

## License

Part of Project O - Self-Evolving AI Agent Platform
