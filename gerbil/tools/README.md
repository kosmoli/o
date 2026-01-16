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

### Tool Execution Sandbox

```scheme
;; Create sandboxed dispatcher with default config
(def sandboxed (make-sandboxed-dispatcher dispatcher))

;; Execute tool in sandbox
(def execution (sandboxed-dispatcher-call-tool sandboxed
                                              "send_message"
                                              (hash 'message "Hello!")
                                              agent-id))

;; Check result
(if (sandbox-execution-succeeded? execution)
    (displayln "Tool executed successfully")
    (displayln (format "Tool execution failed: ~a" (sandbox-execution-error execution))))

;; Create strict sandbox (only allows core tools)
(def strict-config (make-strict-sandbox-config))
(def strict-sandboxed (make-sandboxed-dispatcher dispatcher config: strict-config))

;; Try to execute blocked tool
(def blocked-execution (sandboxed-dispatcher-call-tool strict-sandboxed
                                                      "archival_memory_insert"
                                                      (hash 'content "Test")
                                                      agent-id))
(check (sandbox-execution-failed? blocked-execution))

;; Create custom sandbox config
(def custom-config (make-sandbox-config
                    max-execution-time: 60
                    max-memory-mb: 200
                    allow-network: #f
                    allow-file-read: #f
                    allow-file-write: #f
                    allowed-tools: '("send_message" "conversation_search")
                    blocked-tools: '()))

;; Get execution statistics
(def stats (sandboxed-dispatcher-get-stats sandboxed))
(displayln (format "Success rate: ~a%" (hash-ref stats 'success_rate)))
(displayln (format "Total executions: ~a" (hash-ref stats 'total)))
(displayln (format "Completed: ~a" (hash-ref stats 'completed)))
(displayln (format "Timeouts: ~a" (hash-ref stats 'timeout)))
(displayln (format "Errors: ~a" (hash-ref stats 'error)))

;; Get sandbox execution history
(def history (sandboxed-dispatcher-get-history sandboxed limit: 10))
(for-each
 (lambda (execution)
   (displayln (format "~a: ~a (~as)"
                     (sandbox-execution-tool-name execution)
                     (sandbox-execution-status execution)
                     (sandbox-execution-duration execution))))
 history)
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

## Memory Tools

### core_memory_append

Append content to a core memory block.

**Parameters:**
- `name` (string, required) - Name of the memory block (e.g., 'persona', 'human')
- `content` (string, required) - Content to append to the memory block

**Example:**
```scheme
(def call (dispatcher-call-tool dispatcher
                               "core_memory_append"
                               (hash 'name "persona"
                                     'content "I am helpful and friendly.")
                               agent-id))
```

**Returns:**
```scheme
(hash 'block_name "persona"
      'old_value "..."
      'new_value "... I am helpful and friendly."
      'updated_at 1705401600)
```

### core_memory_replace

Replace content in a core memory block.

**Parameters:**
- `name` (string, required) - Name of the memory block (e.g., 'persona', 'human')
- `old_content` (string, required) - Content to replace (must exist in the block)
- `new_content` (string, required) - New content to replace with

**Example:**
```scheme
(def call (dispatcher-call-tool dispatcher
                               "core_memory_replace"
                               (hash 'name "persona"
                                     'old_content "helpful"
                                     'new_content "very helpful")
                               agent-id))
```

**Returns:**
```scheme
(hash 'block_name "persona"
      'old_value "I am helpful."
      'new_value "I am very helpful."
      'updated_at 1705401600)
```

### archival_memory_insert

Insert content into archival memory.

**Parameters:**
- `content` (string, required) - Content to store in archival memory
- `importance` (integer, optional, default: 5) - Importance score (1-10)
- `tags` (array, optional, default: []) - Tags for categorization

**Example:**
```scheme
(def call (dispatcher-call-tool dispatcher
                               "archival_memory_insert"
                               (hash 'content "User prefers Python over JavaScript"
                                     'importance 8
                                     'tags '("preferences" "programming"))
                               agent-id))
```

**Returns:**
```scheme
(hash 'id "uuid"
      'content "User prefers Python over JavaScript"
      'importance 8
      'tags '("preferences" "programming")
      'created_at 1705401600)
```

### archival_memory_search

Search archival memory for relevant information.

**Parameters:**
- `query` (string, required) - Search query to find matching entries
- `limit` (integer, optional, default: 10) - Maximum number of results
- `page` (integer, optional, default: 0) - Page number for pagination

**Example:**
```scheme
(def call (dispatcher-call-tool dispatcher
                               "archival_memory_search"
                               (hash 'query "Python"
                                     'limit 5
                                     'page 0)
                               agent-id))
```

**Returns:**
```scheme
(hash 'results (list (hash 'id "uuid"
                          'content "User prefers Python..."
                          'importance 8
                          'tags '("preferences")
                          'created_at 1705401600)
                    ...)
      'count 5
      'query "Python"
      'page 0)
```

### archival_memory_semantic_search

Search archival memory using semantic similarity.

**Parameters:**
- `query` (string, required) - Search query for semantic matching
- `limit` (integer, optional, default: 10) - Maximum number of results
- `min_similarity` (number, optional, default: 0.7) - Minimum similarity score (0.0-1.0)

**Example:**
```scheme
(def call (dispatcher-call-tool dispatcher
                               "archival_memory_semantic_search"
                               (hash 'query "programming languages"
                                     'limit 5
                                     'min_similarity 0.7)
                               agent-id))
```

**Returns:**
```scheme
(hash 'results (list (hash 'id "uuid"
                          'content "User prefers Python..."
                          'importance 8
                          'tags '("preferences")
                          'similarity 0.85
                          'created_at 1705401600)
                    ...)
      'count 5
      'query "programming languages"
      'min_similarity 0.7)
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

## Tool Rules and Approval Workflow

### Overview

The tool rules system provides fine-grained control over tool execution through:
- **Rule Engine** - Evaluate conditions and determine actions
- **Approval Manager** - Manage approval requests and workflow
- **Rule-Based Dispatcher** - Integrate rules with tool execution

### Tool Rules

Tool rules define conditions and actions for tool execution:

```scheme
(defstruct tool-rule
  (id                    ; Unique rule ID
   name                  ; Rule name
   description           ; Rule description
   condition             ; Condition function (tool-name, arguments, context) -> boolean
   action                ; Action (:allow, :deny, :require-approval)
   priority              ; Rule priority (higher = evaluated first)
   enabled               ; Is rule enabled?
   metadata)             ; Additional metadata
  transparent: #t)
```

### Creating Rules

```scheme
;; Custom rule
(def my-rule
  (make-tool-rule
   id: "my-rule"
   name: "My Custom Rule"
   description: "Require approval for important operations"
   condition: (lambda (tool-name arguments context)
               (and (equal? tool-name "archival_memory_insert")
                    (> (hash-ref arguments 'importance 0) 8)))
   action: :require-approval
   priority: 50
   enabled: #t
   metadata: (hash)))

;; Built-in rules
(def allow-rule (make-always-allow-rule))
(def deny-rule (make-deny-tool-rule "dangerous_tool"))
(def approval-rule (make-require-approval-for-tool-rule "send_message"))
```

### Rule Engine

The rule engine evaluates rules in priority order:

```scheme
;; Create rule engine
(def engine (make-rule-engine))

;; Add rules
(rule-engine-add-rule! engine my-rule)
(rule-engine-add-rule! engine approval-rule)

;; Remove rule
(rule-engine-remove-rule! engine "my-rule")

;; Get rule
(def rule (rule-engine-get-rule engine "my-rule"))

;; Evaluate rules
(def context (make-tool-execution-context
              agent-id: agent-id
              call-id: "call-123"
              timestamp: (current-seconds)
              metadata: (hash)))

(def result (rule-engine-evaluate engine "send_message" (hash 'message "Test") context))
;; Returns (cons :action "reason")
;; Actions: :allow, :deny, :require-approval

;; Get evaluation history
(def history (rule-engine-get-history engine limit: 10))
```

### Approval Manager

The approval manager handles approval requests:

```scheme
;; Create approval manager
(def manager (make-approval-manager))

;; Create approval request
(def request (approval-manager-create-request! manager tool-call "Requires review"))

;; Approve request
(def approved (approval-manager-approve-request! manager
                                                 (approval-request-id request)
                                                 "reviewer-123"
                                                 "Looks good"))

;; Reject request
(def rejected (approval-manager-reject-request! manager
                                                (approval-request-id request)
                                                "reviewer-123"
                                                "Not allowed"))

;; Get pending requests
(def pending (approval-manager-get-pending manager))

;; Get specific request
(def req (approval-manager-get-request manager request-id))
```

### Rule-Based Dispatcher

The rule-based dispatcher integrates rules with tool execution:

```scheme
;; Create rule-based dispatcher
(def dispatcher (make-tool-dispatcher))
(register-core-tools! dispatcher)
(register-memory-tools! dispatcher)

(def rbd (make-rule-based-dispatcher dispatcher))

;; Add rules
(rule-engine-add-rule! (rule-based-dispatcher-rule-engine rbd)
                       (make-require-approval-for-tool-rule "archival_memory_insert"))

;; Call tool - may require approval
(def result (rule-based-dispatcher-call-tool rbd
                                             "archival_memory_insert"
                                             (hash 'content "Important data")
                                             agent-id))

;; Check result type
(cond
 ((tool-call? result)
  ;; Tool executed directly (allowed or denied)
  (if (tool-call-completed? result)
      (displayln "Tool executed successfully")
      (displayln (format "Tool denied: ~a" (tool-call-error result)))))

 ((approval-request? result)
  ;; Approval required
  (displayln "Approval required")

  ;; Approve and execute
  (def executed (rule-based-dispatcher-approve-call! rbd
                                                     (approval-request-id result)
                                                     "reviewer-123"
                                                     "Approved"))
  (displayln (format "Executed: ~a" (tool-call-status executed))))

 (else
  (displayln "Unexpected result")))
```

### Rule Actions

- **:allow** - Allow tool execution immediately
- **:deny** - Deny tool execution with error
- **:require-approval** - Queue for approval before execution

### Rule Priority

Rules are evaluated in priority order (highest first). First matching rule determines the action:

```scheme
;; High priority deny rule (evaluated first)
(def deny-rule (make-tool-rule
                id: "deny-dangerous"
                name: "Deny Dangerous"
                description: "Block dangerous operations"
                condition: (lambda (tn args ctx) (equal? tn "dangerous_tool"))
                action: :deny
                priority: 100
                enabled: #t
                metadata: (hash)))

;; Low priority allow rule (evaluated later)
(def allow-rule (make-tool-rule
                 id: "allow-all"
                 name: "Allow All"
                 description: "Allow everything else"
                 condition: (lambda (tn args ctx) #t)
                 action: :allow
                 priority: 10
                 enabled: #t
                 metadata: (hash)))
```

### Approval Workflow Example

```scheme
;; Setup
(def dispatcher (make-tool-dispatcher))
(register-core-tools! dispatcher)
(def rbd (make-rule-based-dispatcher dispatcher))

;; Add approval rule for sensitive operations
(rule-engine-add-rule! (rule-based-dispatcher-rule-engine rbd)
                       (make-require-approval-for-tool-rule "send_message"))

;; Agent calls tool
(def result (rule-based-dispatcher-call-tool rbd
                                             "send_message"
                                             (hash 'message "Important message")
                                             agent-id))

;; Check if approval needed
(when (approval-request? result)
  (displayln "Approval request created")
  (displayln (format "Request ID: ~a" (approval-request-id result)))
  (displayln (format "Reason: ~a" (approval-request-reason result)))

  ;; Get all pending requests
  (def pending (approval-manager-get-pending (rule-based-dispatcher-approval-manager rbd)))
  (displayln (format "Pending requests: ~a" (length pending)))

  ;; Reviewer approves
  (def executed (rule-based-dispatcher-approve-call! rbd
                                                     (approval-request-id result)
                                                     "reviewer-123"
                                                     "Approved for execution"))

  ;; Check execution result
  (if (tool-call-completed? executed)
      (displayln "Tool executed successfully after approval")
      (displayln "Tool execution failed")))
```

### Built-in Rules

```scheme
;; Always allow (priority 0, enabled)
(make-always-allow-rule)

;; Always deny (priority 100, disabled by default)
(make-always-deny-rule)

;; Require approval for specific tool
(make-require-approval-for-tool-rule "tool-name")

;; Deny specific tool
(make-deny-tool-rule "tool-name")
```

## Future Enhancements

- [ ] Tool rate limiting
- [ ] Tool usage statistics
- [ ] Tool versioning
- [ ] Tool dependencies
- [ ] Tool composition
- [ ] Async tool execution
- [ ] Tool result caching
- [ ] Rule templates and presets
- [ ] Conditional approval chains
- [ ] Time-based rules

## License

Part of Project O - Self-Evolving AI Agent Platform
