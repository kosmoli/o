# Gerbil Agent Execution System

Agent execution system for Project O, implementing step-based execution loop with LLM inference, tool calls, and memory management.

## Overview

The agent execution system provides:
- **Agent Configuration** - Configure agent behavior, LLM settings, and capabilities
- **Agent State Management** - Track execution state, step count, and status
- **Step-Based Execution** - Execute agent operations as discrete steps
- **Execution Context** - Maintain conversation history, memory, and execution state
- **Step Executor** - Execute individual steps (user messages, LLM inference, tool calls, memory updates)
- **Execution Loop** - Orchestrate multi-step agent execution with automatic step determination

## Architecture

```
Application
    ↓
agent/executor.ss (Step executor and execution loop)
    ↓
agent/types.ss (Type definitions)
    ↓
├── llm/client.ss (LLM inference)
├── tools/core.ss (Tool execution)
├── message/manager.ss (Message management)
└── memory/blocks.ss (Memory management)
```

## Installation

No installation required - part of Project O's Gerbil codebase.

## Usage

### Creating an Agent Configuration

```scheme
(import :gerbil/agent/types
        :gerbil/agent/executor)

;; Create default configuration
(def config (make-default-agent-config "agent-123" "My Agent"))

;; Or create custom configuration
(def custom-config
  (make-agent-config
   id: "agent-456"
   name: "Custom Agent"
   persona: "I am a helpful AI assistant specialized in coding."
   human: "The user I am assisting with programming tasks."
   llm-provider: :anthropic
   llm-model: "claude-3-5-sonnet-20241022"
   llm-config: (hash 'temperature 0.7 'max_tokens 4096)
   max-steps: 50
   context-window: 100000
   tools-enabled: '("send_message" "conversation_search"
                    "core_memory_append" "core_memory_replace")
   memory-enabled: #t
   streaming-enabled: #f
   metadata: (hash)))
```

### Creating an Execution Context

```scheme
;; Create initial agent state
(def state (make-initial-agent-state "agent-123"))

;; Create execution context
(def context
  (make-execution-context
   agent-id: "agent-123"
   agent-config: config
   agent-state: state
   conversation-history: '()
   memory-blocks: '()
   step-history: '()
   current-step: 0
   start-time: (current-seconds)
   metadata: (hash)))
```

### Creating a Step Executor

```scheme
(import :gerbil/llm/client
        :gerbil/tools/core
        :gerbil/message/manager
        :gerbil/memory/blocks)

;; Create LLM client
(def llm-client (make-llm-client :anthropic "your-api-key"))

;; Create tool dispatcher
(def tool-dispatcher (make-tool-dispatcher))
(register-core-tools! tool-dispatcher)

;; Create message manager
(def message-manager (make-message-manager db-client))

;; Create memory manager
(def memory-manager (make-memory-block-manager db-client))

;; Create step executor
(def executor (make-step-executor
               llm-client
               tool-dispatcher
               message-manager
               memory-manager
               max-retries: 3))
```

### Executing Individual Steps

```scheme
;; Execute user message step
(def user-step (make-execution-step-record
                "agent-123"
                0
                step-type-user-message
                (hash 'content "Hello, how can you help me?")))

(def result (execute-step executor context user-step))
(displayln (format "Step status: ~a" (execution-step-status result)))
(displayln (format "Step output: ~a" (execution-step-output result)))

;; Execute LLM inference step
(def llm-step (make-execution-step-record
               "agent-123"
               1
               step-type-llm-inference
               (hash)))

(def llm-result (execute-step executor context llm-step))
(displayln (format "LLM response: ~a"
                  (hash-ref (execution-step-output llm-result) 'content)))

;; Execute tool call step
(def tool-step (make-execution-step-record
                "agent-123"
                2
                step-type-tool-call
                (hash 'tool_name "send_message"
                      'arguments (hash 'message "I can help you with that!"))))

(def tool-result (execute-step executor context tool-step))
(displayln (format "Tool result: ~a" (execution-step-output tool-result)))
```

### Running the Execution Loop

```scheme
;; Execute agent with automatic step determination
(def result (execute-agent executor context))

;; Check execution result
(if (execution-result-success result)
    (begin
      (displayln "Execution successful!")
      (displayln (format "Steps executed: ~a" (execution-result-steps-executed result)))
      (displayln (format "Duration: ~as" (execution-result-duration result)))
      (displayln (format "Final status: ~a"
                        (agent-state-status (execution-result-final-state result)))))
    (begin
      (displayln "Execution failed!")
      (displayln (format "Error: ~a" (execution-result-error result)))))

;; Access step history
(def history (execution-context-step-history context))
(displayln (format "Total steps in history: ~a" (length history)))

;; Iterate through steps
(for-each
 (lambda (step)
   (displayln (format "Step ~a: ~a (~a)"
                     (execution-step-step-number step)
                     (execution-step-type step)
                     (execution-step-status step))))
 (reverse history))
```

### Working with Agent State

```scheme
;; Check agent state
(def state (execution-result-final-state result))

(displayln (format "Agent status: ~a" (agent-state-status state)))
(displayln (format "Step count: ~a" (agent-state-step-count state)))
(displayln (format "Message count: ~a" (agent-state-message-count state)))
(displayln (format "Token count: ~a" (agent-state-token-count state)))

;; Check if agent is running
(if (agent-running? state)
    (displayln "Agent is currently running")
    (displayln "Agent is not running"))

;; Check if agent has error
(when (agent-error? state)
  (displayln (format "Agent error: ~a" (agent-state-error state))))
```

## Step Types

### User Message Step

Adds a user message to the conversation history.

**Input:**
```scheme
(hash 'content "User message content")
```

**Output:**
```scheme
(hash 'message_id "uuid"
      'content "User message content"
      'timestamp 1705401600)
```

### LLM Inference Step

Calls the LLM with conversation history and available tools.

**Input:**
```scheme
(hash)  ; No input required
```

**Output:**
```scheme
(hash 'content "LLM response content"
      'tool_calls (list (hash 'tool_name "tool_name"
                             'arguments (hash ...)))
      'usage (hash 'prompt_tokens 100
                   'completion_tokens 50
                   'total_tokens 150)
      'finish_reason "stop")
```

### Tool Call Step

Executes a tool through the tool dispatcher.

**Input:**
```scheme
(hash 'tool_name "send_message"
      'arguments (hash 'message "Hello!"))
```

**Output:**
```scheme
(hash 'tool_name "send_message"
      'arguments (hash 'message "Hello!")
      'status :completed
      'result (hash ...)
      'error #f)
```

### Memory Update Step

Updates core memory blocks.

**Input (Append):**
```scheme
(hash 'operation :append
      'block_name "persona"
      'content "Additional information")
```

**Input (Replace):**
```scheme
(hash 'operation :replace
      'block_name "persona"
      'old_content "old text"
      'new_content "new text")
```

**Output:**
```scheme
(hash 'operation "append"
      'block_name "persona"
      'content "Additional information")
```

### System Step

Internal system operations.

**Input:**
```scheme
(hash 'action "system_action"
      ...)
```

**Output:**
```scheme
(hash ...)  ; Returns input as output
```

## Execution Flow

The execution loop follows this pattern:

1. **Initial State**: Agent starts in `:idle` status
2. **Start Execution**: Status changes to `:running`
3. **Step Determination**: Determine next step based on history
4. **Step Execution**: Execute the determined step
5. **State Update**: Update agent state (step count, token count, etc.)
6. **Check Completion**:
   - If max steps reached → Complete
   - If no more steps → Complete
   - If step failed → Error
   - Otherwise → Continue to step 3
7. **Final State**: Status changes to `:completed` or `:error`

### Step Determination Logic

The execution loop automatically determines the next step:

- **No history**: Start with LLM inference
- **After user message**: Do LLM inference
- **After LLM inference**:
  - If tool calls present → Execute first tool call
  - If no tool calls → Complete
- **After tool call**: Do LLM inference
- **Unknown step type**: Stop

## API Reference

### Agent Configuration

#### agent-config

```scheme
(defstruct agent-config
  (id                    ; Agent ID (string)
   name                  ; Agent name (string)
   persona               ; Agent persona (string)
   human                 ; Human description (string)
   llm-provider          ; LLM provider (:openai, :anthropic, :groq, :ollama)
   llm-model             ; LLM model name (string)
   llm-config            ; LLM configuration (hash)
   max-steps             ; Maximum steps per execution (integer)
   context-window        ; Context window size in tokens (integer)
   tools-enabled         ; List of enabled tool names
   memory-enabled        ; Is memory system enabled? (boolean)
   streaming-enabled     ; Is streaming enabled? (boolean)
   metadata)             ; Additional metadata (hash)
  transparent: #t)
```

#### Constructors

- `(make-default-agent-config agent-id name)` - Create default configuration
- `(make-agent-config ...)` - Create custom configuration

### Agent State

#### agent-state

```scheme
(defstruct agent-state
  (agent-id              ; Agent ID (string)
   step-count            ; Current step count (integer)
   message-count         ; Total message count (integer)
   token-count           ; Total token count (integer)
   last-activity         ; Last activity timestamp (seconds)
   status                ; Agent status (:idle, :running, :paused, :error)
   error                 ; Error message if status is :error (optional)
   metadata)             ; Additional state metadata (hash)
  transparent: #t)
```

#### Constructors

- `(make-initial-agent-state agent-id)` - Create initial state

#### Predicates

- `(agent-running? state)` - Check if agent is running
- `(agent-idle? state)` - Check if agent is idle
- `(agent-error? state)` - Check if agent has error

### Execution Step

#### execution-step

```scheme
(defstruct execution-step
  (id                    ; Step ID (string)
   agent-id              ; Agent ID (string)
   step-number           ; Step number (integer)
   timestamp             ; Step timestamp (seconds)
   type                  ; Step type (:user-message, :llm-inference, :tool-call, :memory-update)
   input                 ; Step input (any)
   output                ; Step output (any)
   status                ; Step status (:pending, :running, :completed, :failed)
   duration              ; Step duration in seconds (optional)
   error                 ; Error message if failed (optional)
   metadata)             ; Additional step metadata (hash)
  transparent: #t)
```

#### Constructors

- `(make-execution-step-record agent-id step-number type input)` - Create step record

#### Predicates

- `(step-completed? step)` - Check if step is completed
- `(step-failed? step)` - Check if step failed
- `(step-running? step)` - Check if step is running

### Execution Context

#### execution-context

```scheme
(defstruct execution-context
  (agent-id              ; Agent ID (string)
   agent-config          ; Agent configuration
   agent-state           ; Agent state
   conversation-history  ; List of messages
   memory-blocks         ; Core memory blocks
   step-history          ; List of execution steps
   current-step          ; Current step number (integer)
   start-time            ; Execution start time (seconds)
   metadata)             ; Additional context metadata (hash)
  transparent: #t)
```

### Execution Result

#### execution-result

```scheme
(defstruct execution-result
  (success               ; Was execution successful? (boolean)
   agent-id              ; Agent ID (string)
   steps-executed        ; Number of steps executed (integer)
   final-state           ; Final agent state
   output                ; Execution output (any)
   error                 ; Error message if failed (optional)
   duration              ; Total execution duration in seconds
   metadata)             ; Additional result metadata (hash)
  transparent: #t)
```

### Step Executor

#### step-executor

```scheme
(defstruct step-executor
  (llm-client            ; LLM client for inference
   tool-dispatcher       ; Tool dispatcher for tool calls
   message-manager       ; Message manager for conversation
   memory-manager        ; Memory block manager
   max-retries           ; Maximum retries for failed steps
   metadata)             ; Additional metadata
  transparent: #t)
```

#### Constructor

- `(make-step-executor llm-client tool-dispatcher message-manager memory-manager #!key (max-retries 3))` - Create step executor

#### Operations

- `(execute-step executor context step)` - Execute a single step
- `(execute-agent executor context)` - Execute agent with automatic step determination

### Helper Functions

- `(build-llm-messages context)` - Build messages for LLM inference
- `(build-system-message config memory-blocks)` - Build system message with persona and memory
- `(get-available-tools executor config)` - Get available tools for agent
- `(determine-next-step executor context)` - Determine next step to execute

## Status Types

### Agent Status

- `:idle` - Agent is idle, not executing
- `:running` - Agent is currently executing
- `:paused` - Agent execution is paused
- `:error` - Agent encountered an error
- `:completed` - Agent execution completed successfully

### Step Status

- `:pending` - Step is pending execution
- `:running` - Step is currently executing
- `:completed` - Step completed successfully
- `:failed` - Step failed with error

### Step Types

- `:user-message` - User message step
- `:llm-inference` - LLM inference step
- `:tool-call` - Tool call step
- `:memory-update` - Memory update step
- `:system` - System step

## Error Handling

All execution operations may throw errors. Use `try/catch` for error handling:

```scheme
(try
 (def result (execute-agent executor context))
 (if (execution-result-success result)
     (displayln "Success!")
     (displayln (format "Failed: ~a" (execution-result-error result))))
 (catch (e)
   (displayln (format "Error: ~a" (error-message e)))))
```

Step execution automatically catches errors and marks steps as failed:

```scheme
(def step-result (execute-step executor context step))
(when (step-failed? step-result)
  (displayln (format "Step failed: ~a" (execution-step-error step-result))))
```

## Conversion Functions

Convert structures to hash representations for serialization:

```scheme
;; Convert agent config to hash
(def config-hash (agent-config->hash config))

;; Convert agent state to hash
(def state-hash (agent-state->hash state))

;; Convert execution step to hash
(def step-hash (execution-step->hash step))

;; Convert execution result to hash
(def result-hash (execution-result->hash result))
```

## Testing

```scheme
;; Test step execution
(def executor (make-step-executor llm-client tool-dispatcher
                                 message-manager memory-manager))
(def context (make-execution-context ...))
(def step (make-execution-step-record agent-id 0 step-type-llm-inference (hash)))
(def result (execute-step executor context step))
(assert (step-completed? result))

;; Test execution loop
(def exec-result (execute-agent executor context))
(assert (execution-result-success exec-result))
(assert (> (execution-result-steps-executed exec-result) 0))

;; Test state management
(def state (execution-result-final-state exec-result))
(assert (or (agent-idle? state) (eq? (agent-state-status state) agent-status-completed)))
```

## Performance Considerations

### Context Window Management

The execution system automatically manages context windows:
- System message includes persona and memory blocks
- Conversation history is included in LLM calls
- Token usage is tracked in agent state

### Step Limits

Configure `max-steps` to prevent infinite loops:
```scheme
(agent-config-max-steps-set! config 50)  ; Limit to 50 steps
```

### Memory Management

- Step history grows with each execution
- Consider clearing old steps for long-running agents
- Memory blocks are loaded on demand

## Integration

### With LLM Clients

```scheme
(import :gerbil/llm/client)
(def llm-client (make-llm-client :anthropic "api-key"))
```

### With Tool System

```scheme
(import :gerbil/tools/core)
(def dispatcher (make-tool-dispatcher))
(register-core-tools! dispatcher)
(register-memory-tools! dispatcher)
```

### With Message System

```scheme
(import :gerbil/message/manager)
(def message-manager (make-message-manager db-client))
```

### With Memory System

```scheme
(import :gerbil/memory/blocks)
(def memory-manager (make-memory-block-manager db-client))
```

## Context Window Manager

The context window manager handles token limits and optimizes conversation history to fit within model constraints.

### Creating a Context Window Manager

```scheme
(import :gerbil/agent/context)

;; Create with default settings
(def manager (make-context-window-manager))

;; Create with custom settings
(def custom-manager
  (make-context-window-manager
   max-tokens: 50000
   system-tokens: 1000
   response-tokens: 2048
   strategy: :sliding))
```

### Token Counting

```scheme
;; Estimate tokens in text
(def tokens (estimate-tokens "Hello, world!"))

;; Count tokens in a message
(def msg-tokens (count-message-tokens manager message))

;; Count tokens in multiple messages
(def total-tokens (count-messages-tokens manager messages))

;; Count tokens in memory blocks
(def block-tokens (count-memory-block-tokens manager block))

;; Count tokens in system message
(def sys-tokens (count-system-message-tokens manager persona blocks))
```

### Context Analysis

```scheme
;; Analyze token usage
(def usage (analyze-context-usage manager context))

;; Check usage statistics
(displayln (format "Total tokens: ~a" (hash-ref usage 'total_tokens)))
(displayln (format "Available: ~a" (hash-ref usage 'available_tokens)))
(displayln (format "Usage: ~a%" (hash-ref usage 'usage_percent)))

;; Check if context fits
(if (check-context-fits manager context)
    (displayln "Context fits within limits")
    (displayln "Context needs optimization"))
```

### Optimization Strategies

#### Truncate Strategy

Removes oldest messages until context fits:

```scheme
(def manager (make-context-window-manager strategy: :truncate))
(def optimized (optimize-context manager context))
```

#### Sliding Window Strategy

Keeps first few messages (for context) and most recent messages:

```scheme
(def manager (make-context-window-manager strategy: :sliding))
(def optimized (optimize-context manager context))
```

#### Summarize Strategy

Summarizes older messages (placeholder - requires LLM):

```scheme
(def manager (make-context-window-manager strategy: :summarize))
(def optimized (optimize-context manager context))
```

### Integration with Executor

```scheme
;; Create context window manager
(def ctx-manager (make-context-window-manager
                  max-tokens: 100000
                  strategy: :sliding))

;; Before execution, optimize context
(def optimized-context (optimize-context ctx-manager context))

;; Execute with optimized context
(def result (execute-agent executor optimized-context))
```

### Context Window Configuration

Configure context window settings in agent config:

```scheme
(def config
  (make-agent-config
   id: "agent-1"
   name: "Assistant"
   persona: "I am helpful"
   human: "User"
   llm-provider: :anthropic
   llm-model: "claude-3-5-sonnet-20241022"
   llm-config: (hash 'temperature 0.7 'max_tokens 4096)
   max-steps: 50
   context-window: 100000  ; Maximum context window size
   tools-enabled: '("send_message")
   memory-enabled: #t
   streaming-enabled: #f
   metadata: (hash)))
```

## Examples

### Simple Agent Execution

```scheme
;; Setup
(def config (make-default-agent-config "agent-1" "Assistant"))
(def state (make-initial-agent-state "agent-1"))
(def context (make-execution-context
              agent-id: "agent-1"
              agent-config: config
              agent-state: state
              conversation-history: '()
              memory-blocks: '()
              step-history: '()
              current-step: 0
              start-time: (current-seconds)
              metadata: (hash)))

;; Execute
(def result (execute-agent executor context))

;; Check result
(displayln (format "Success: ~a" (execution-result-success result)))
(displayln (format "Steps: ~a" (execution-result-steps-executed result)))
```

### Agent with Conversation History

```scheme
;; Add conversation history
(execution-context-conversation-history-set!
 context
 (list (make-message
        id: "msg-1"
        agent-id: "agent-1"
        role: :user
        content: "Hello!"
        timestamp: (current-seconds)
        metadata: (hash))
       (make-message
        id: "msg-2"
        agent-id: "agent-1"
        role: :assistant
        content: "Hi! How can I help?"
        timestamp: (current-seconds)
        metadata: (hash))))

;; Execute with history
(def result (execute-agent executor context))
```

### Agent with Memory Blocks

```scheme
;; Add memory blocks
(execution-context-memory-blocks-set!
 context
 (list (make-memory-block
        name: "persona"
        label: "Persona"
        value: "I am a helpful AI assistant."
        template: #f
        limit: 1000
        metadata: (hash))
       (make-memory-block
        name: "human"
        label: "Human"
        value: "The user I am assisting."
        template: #f
        limit: 1000
        metadata: (hash))))

;; Execute with memory
(def result (execute-agent executor context))
```

## Future Enhancements

- [ ] Parallel step execution
- [ ] Step checkpointing and recovery
- [ ] Streaming execution with callbacks
- [ ] Advanced step determination strategies
- [ ] Step execution metrics and profiling
- [ ] Step execution visualization
- [ ] Multi-agent coordination
- [ ] Step execution policies

## License

Part of Project O - Self-Evolving AI Agent Platform
