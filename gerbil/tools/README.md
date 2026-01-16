# Gerbil Tools System

Tool system for Project O, implementing tool definitions, registry, dispatcher, and core tools for agent operations.

## Overview

The tools system provides:
- **Tool Definitions** - Structured tool definitions with parameters and handlers
- **Tool Registry** - Central registry for managing available tools
- **Tool Dispatcher** - Execution dispatcher with approval workflow
- **Core Tools** - Built-in tools (send_message, conversation_search)
- **Tool Validation** - Parameter validation and type checking
- **Tool Execution** - Safe tool execution with error handling
- **Approval Workflow** - Optional approval for sensitive tools

## Architecture

```
Application
    ↓
tools/core.ss (Tool dispatcher and core tools)
    ↓
tools/types.ss (Type definitions and validation)
    ↓
message/manager.ss (Message operations)
    ↓
database/client.ss (Database operations)
```

## Installation

No installation required - part of Project O's Gerbil codebase.

## Usage

### Creating a Tool Dispatcher

```scheme
(import :gerbil/tools/core
        :gerbil/tools/types)

;; Create dispatcher
(def dispatcher (make-tool-dispatcher))

;; Register core tools
(register-core-tools! dispatcher)
```

### Calling Tools

```scheme
;; Call send_message tool
(def call (dispatcher-call-tool dispatcher
                               "send_message"
                               (hash 'message "Hello, user!")
                               agent-id))

;; Check result
(displayln (format "Status: ~a" (tool-call-status call)))
(displayln (format "Result: ~a" (tool-call-result call)))

;; Call conversation_search tool
(def search-call (dispatcher-call-tool dispatcher
                                      "conversation_search"
                                      (hash 'query "hello" 'limit 5)
                                      agent-id))
```

### Defining Custom Tools

```scheme
;; Define custom tool
(def my-tool
  (make-tool-definition
   name: "my_custom_tool"
   description: "My custom tool description"
   parameters: (hash
                'input (hash 'type :string
                            'description "Input parameter"
                            'required #t)
                'count (hash 'type :integer
                            'description "Count parameter"
                            'required #f
                            'default 1))
   handler: (lambda (arguments context)
             ;; Tool implementation
             (let ((input (hash-ref arguments 'input))
                   (count (hash-ref arguments 'count 1)))
               (make-success-result
                (hash 'processed input
                      'count count))))
   category: :custom
   requires-approval: #f
   metadata: (hash 'version "1.0")))

;; Register custom tool
(dispatcher-register-tool! dispatcher my-tool)

;; Call custom tool
(def result (dispatcher-call-tool dispatcher
                                 "my_custom_tool"
                                 (hash 'input "test" 'count 3)
                                 agent-id))
```

### Tool Approval Workflow

```scheme
;; Define tool requiring approval
(def sensitive-tool
  (make-tool-definition
   name: "sensitive_operation"
   description: "Sensitive operation requiring approval"
   parameters: (hash 'action (hash 'type :string 'required #t))
   handler: (lambda (args ctx)
             (make-success-result "Operation completed"))
   category: :custom
   requires-approval: #t  ; Requires approval
   metadata: (hash)))

;; Register tool
(dispatcher-register-tool! dispatcher sensitive-tool)

;; Call tool (goes to approval queue)
(def call (dispatcher-call-tool dispatcher
                               "sensitive_operation"
                               (hash 'action "delete")
                               agent-id))

;; Check status
(displayln (format "Status: ~a" (tool-call-status call)))  ; :pending

;; Get pending calls
(def pending (dispatcher-get-pending dispatcher))
(displayln (format "Pending calls: ~a" (length pending)))

;; Approve call
(def approved (dispatcher-approve-call! dispatcher (tool-call-id call)))
(displayln (format "Approved status: ~a" (tool-call-status approved)))  ; :completed

;; Or reject call
(def rejected (dispatcher-reject-call! dispatcher (tool-call-id call) "User rejected"))
(displayln (format "Rejected status: ~a" (tool-call-status rejected)))  ; :rejected
```

### Tool Registry Operations

```scheme
;; Get tool definition
(def tool-def (registry-get-tool (tool-dispatcher-registry dispatcher)
                                "send_message"))

;; List all tools
(def all-tools (registry-list-tools (tool-dispatcher-registry dispatcher)))
(displayln (format "Available tools: ~a" all-tools))

;; List tools by category
(def core-tools (registry-list-tools (tool-dispatcher-registry dispatcher)
                                    category: :core))
(displayln (format "Core tools: ~a" core-tools))

;; Check if tool exists
(if (registry-has-tool? (tool-dispatcher-registry dispatcher) "send_message")
    (displayln "Tool exists")
    (displayln "Tool not found"))
```

### Tool Call History

```scheme
;; Get call history
(def history (dispatcher-get-history dispatcher limit: 10))
(displayln (format "Call history: ~a calls" (length history)))

;; Iterate through history
(for-each
 (lambda (call)
   (displayln (format "~a: ~a (~a)"
                     (tool-call-tool-name call)
                     (tool-call-status call)
                     (tool-call-timestamp call))))
 history)

;; Get specific call
(def call (dispatcher-get-call dispatcher call-id))
(when call
  (displayln (format "Call found: ~a" (tool-call-tool-name call))))
```

## Core Tools

### send_message

Send a message to the user.

**Parameters:**
- `message` (string, required) - The message content to send

**Example:**
```scheme
(def call (dispatcher-call-tool dispatcher
                               "send_message"
                               (hash 'message "Hello, user!")
                               agent-id))
```

**Returns:**
```scheme
(hash 'message_id "uuid"
      'content "Hello, user!"
      'timestamp 1705401600)
```

### conversation_search

Search through conversation history.

**Parameters:**
- `query` (string, required) - Search query
- `limit` (integer, optional, default: 10) - Maximum results
- `page` (integer, optional, default: 0) - Page number for pagination

**Example:**
```scheme
(def call (dispatcher-call-tool dispatcher
                               "conversation_search"
                               (hash 'query "hello" 'limit 5 'page 0)
                               agent-id))
```

**Returns:**
```scheme
(hash 'results (list (hash 'id "uuid"
                          'role "user"
                          'content "hello"
                          'timestamp 1705401600)
                    ...)
      'count 5
      'query "hello"
      'page 0)
```

## API Reference

### Tool Structures

#### tool-definition

```scheme
(defstruct tool-definition
  (name              ; Tool name (string)
   description       ; Tool description (string)
   parameters        ; Parameter schema (hash)
   handler           ; Tool handler function
   category          ; Tool category (symbol)
   requires-approval ; Requires approval? (boolean)
   metadata)         ; Additional metadata (hash)
  transparent: #t)
```

#### tool-call

```scheme
(defstruct tool-call
  (id                ; Unique call ID (string)
   tool-name         ; Tool name (string)
   arguments         ; Tool arguments (hash)
   timestamp         ; Call timestamp (seconds)
   agent-id          ; Agent ID (string)
   status            ; Call status (symbol)
   result            ; Tool result (optional)
   error)            ; Error message (optional)
  transparent: #t)
```

#### tool-result

```scheme
(defstruct tool-result
  (success           ; Was successful? (boolean)
   value             ; Result value (any)
   error             ; Error message (optional)
   metadata)         ; Additional metadata (hash)
  transparent: #t)
```

### Tool Registry

#### Constructor

- `(make-tool-registry)` - Create empty tool registry

#### Operations

- `(registry-register-tool! registry tool-def)` - Register tool
- `(registry-get-tool registry name)` - Get tool by name
- `(registry-list-tools registry #!key (category #f))` - List tools
- `(registry-has-tool? registry name)` - Check if tool exists

### Tool Dispatcher

#### Constructor

- `(make-tool-dispatcher)` - Create tool dispatcher

#### Operations

- `(dispatcher-register-tool! dispatcher tool-def)` - Register tool
- `(dispatcher-call-tool dispatcher tool-name arguments agent-id)` - Call tool
- `(dispatcher-execute-call! dispatcher call)` - Execute tool call
- `(dispatcher-approve-call! dispatcher call-id)` - Approve pending call
- `(dispatcher-reject-call! dispatcher call-id reason)` - Reject pending call
- `(dispatcher-get-call dispatcher call-id)` - Get call by ID
- `(dispatcher-get-history dispatcher #!key (limit 10))` - Get call history
- `(dispatcher-get-pending dispatcher)` - Get pending calls

### Tool Execution

- `(execute-tool registry tool-name arguments context)` - Execute tool
- `(register-core-tools! dispatcher)` - Register core tools

### Tool Validation

- `(validate-tool-name name)` - Validate tool name
- `(validate-tool-parameters params)` - Validate parameter schema
- `(validate-tool-definition tool-def)` - Validate tool definition
- `(validate-tool-arguments tool-def arguments)` - Validate arguments

### Tool Results

- `(make-success-result value #!key (metadata (hash)))` - Create success result
- `(make-error-result error-message #!key (metadata (hash)))` - Create error result

### Tool Call Status

- `(tool-call-pending? call)` - Check if pending
- `(tool-call-approved? call)` - Check if approved
- `(tool-call-executing? call)` - Check if executing
- `(tool-call-completed? call)` - Check if completed
- `(tool-call-failed? call)` - Check if failed

### Conversion Functions

- `(tool-definition->hash tool-def)` - Convert to hash
- `(tool-call->hash call)` - Convert to hash
- `(tool-result->hash result)` - Convert to hash
- `(hash->tool-call h)` - Convert from hash

## Tool Categories

- `:core` - Core tools (send_message, conversation_search)
- `:memory` - Memory tools (core_memory_append, archival_memory_insert, etc.)
- `:system` - System tools (internal operations)
- `:custom` - Custom user-defined tools

## Tool Parameter Types

- `:string` - String value
- `:number` - Numeric value (integer or float)
- `:integer` - Integer value
- `:boolean` - Boolean value (true/false)
- `:object` - Hash/object value
- `:array` - List/array value
- `:null` - Null value

## Tool Call Status

- `:pending` - Waiting for approval
- `:approved` - Approved, ready to execute
- `:rejected` - Rejected by user
- `:executing` - Currently executing
- `:completed` - Successfully completed
- `:failed` - Failed with error

## Features

### Tool Definition

- Structured tool definitions with metadata
- Parameter schemas with type validation
- Custom validation functions
- Enum parameter support
- Required/optional parameters with defaults

### Tool Registry

- Central registry for all tools
- Category-based organization
- Tool lookup by name
- List tools by category

### Tool Dispatcher

- Centralized tool execution
- Approval workflow for sensitive tools
- Call history tracking
- Pending call queue management
- Error handling and reporting

### Tool Validation

- Tool name validation
- Parameter schema validation
- Argument type checking
- Required parameter checking
- Enum value validation
- Custom validators

### Tool Execution

- Safe execution with error handling
- Execution context with metadata
- Success/error result handling
- Tool handler isolation

## Error Handling

All tool operations may throw errors. Use `try/catch` for error handling:

```scheme
(try
 (def call (dispatcher-call-tool dispatcher
                                "my_tool"
                                (hash 'param "value")
                                agent-id))
 (if (tool-call-completed? call)
     (displayln "Success!")
     (displayln (format "Failed: ~a" (tool-call-error call))))
 (catch (e)
   (displayln (format "Error: ~a" (error-message e)))))
```

## Testing

```scheme
;; Test tool registration
(def dispatcher (make-tool-dispatcher))
(register-core-tools! dispatcher)
(assert (registry-has-tool? (tool-dispatcher-registry dispatcher) "send_message"))

;; Test tool execution
(def call (dispatcher-call-tool dispatcher
                               "send_message"
                               (hash 'message "Test")
                               agent-id))
(assert (tool-call-completed? call))

;; Test approval workflow
(def approval-tool (make-tool-definition
                    name: "test_approval"
                    description: "Test"
                    parameters: (hash)
                    handler: (lambda (args ctx) (make-success-result "ok"))
                    category: :custom
                    requires-approval: #t
                    metadata: (hash)))
(dispatcher-register-tool! dispatcher approval-tool)
(def call (dispatcher-call-tool dispatcher "test_approval" (hash) agent-id))
(assert (tool-call-pending? call))
```

## Future Enhancements

- [ ] Tool sandboxing for security
- [ ] Tool rate limiting
- [ ] Tool usage statistics
- [ ] Tool versioning
- [ ] Tool dependencies
- [ ] Tool composition
- [ ] Async tool execution
- [ ] Tool result caching

## License

Part of Project O - Self-Evolving AI Agent Platform
