;;; agent/types.ss - Agent Execution Types
;;;
;;; Type definitions for agent execution, steps, and state management.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format)

;;; ============================================================================
;;; Agent Configuration
;;; ============================================================================

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

;;; ============================================================================
;;; Agent State
;;; ============================================================================

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

;;; ============================================================================
;;; Execution Step
;;; ============================================================================

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

;;; ============================================================================
;;; Execution Context
;;; ============================================================================

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

;;; ============================================================================
;;; Execution Result
;;; ============================================================================

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

;;; ============================================================================
;;; Step Types
;;; ============================================================================

(def step-type-user-message :user-message)
(def step-type-llm-inference :llm-inference)
(def step-type-tool-call :tool-call)
(def step-type-memory-update :memory-update)
(def step-type-system :system)

;;; ============================================================================
;;; Agent Status
;;; ============================================================================

(def agent-status-idle :idle)
(def agent-status-running :running)
(def agent-status-paused :paused)
(def agent-status-error :error)
(def agent-status-completed :completed)

;;; ============================================================================
;;; Step Status
;;; ============================================================================

(def step-status-pending :pending)
(def step-status-running :running)
(def step-status-completed :completed)
(def step-status-failed :failed)

;;; ============================================================================
;;; Constructors
;;; ============================================================================

(def (make-default-agent-config agent-id name)
  "Create default agent configuration

   Args:
     agent-id: Agent ID
     name: Agent name

   Returns:
     Agent configuration"

  (make-agent-config
   id: agent-id
   name: name
   persona: "I am a helpful AI assistant."
   human: "The user I am assisting."
   llm-provider: :anthropic
   llm-model: "claude-3-5-sonnet-20241022"
   llm-config: (hash 'temperature 0.7 'max_tokens 4096)
   max-steps: 50
   context-window: 100000
   tools-enabled: '("send_message" "conversation_search"
                    "core_memory_append" "core_memory_replace"
                    "archival_memory_insert" "archival_memory_search")
   memory-enabled: #t
   streaming-enabled: #f
   metadata: (hash)))

(def (make-initial-agent-state agent-id)
  "Create initial agent state

   Args:
     agent-id: Agent ID

   Returns:
     Agent state"

  (make-agent-state
   agent-id: agent-id
   step-count: 0
   message-count: 0
   token-count: 0
   last-activity: (current-seconds)
   status: agent-status-idle
   error: #f
   metadata: (hash)))

(def (make-execution-step-record agent-id step-number type input)
  "Create execution step record

   Args:
     agent-id: Agent ID
     step-number: Step number
     type: Step type
     input: Step input

   Returns:
     Execution step"

  (make-execution-step
   id: (uuid-generate)
   agent-id: agent-id
   step-number: step-number
   timestamp: (current-seconds)
   type: type
   input: input
   output: #f
   status: step-status-pending
   duration: #f
   error: #f
   metadata: (hash)))

;;; ============================================================================
;;; Predicates
;;; ============================================================================

(def (agent-running? state)
  "Check if agent is running

   Args:
     state: Agent state

   Returns:
     #t if running, #f otherwise"

  (eq? (agent-state-status state) agent-status-running))

(def (agent-idle? state)
  "Check if agent is idle

   Args:
     state: Agent state

   Returns:
     #t if idle, #f otherwise"

  (eq? (agent-state-status state) agent-status-idle))

(def (agent-error? state)
  "Check if agent has error

   Args:
     state: Agent state

   Returns:
     #t if error, #f otherwise"

  (eq? (agent-state-status state) agent-status-error))

(def (step-completed? step)
  "Check if step is completed

   Args:
     step: Execution step

   Returns:
     #t if completed, #f otherwise"

  (eq? (execution-step-status step) step-status-completed))

(def (step-failed? step)
  "Check if step failed

   Args:
     step: Execution step

   Returns:
     #t if failed, #f otherwise"

  (eq? (execution-step-status step) step-status-failed))

(def (step-running? step)
  "Check if step is running

   Args:
     step: Execution step

   Returns:
     #t if running, #f otherwise"

  (eq? (execution-step-status step) step-status-running))

;;; ============================================================================
;;; Conversion Functions
;;; ============================================================================

(def (agent-config->hash config)
  "Convert agent configuration to hash

   Args:
     config: Agent configuration

   Returns:
     Hash representation"

  (hash 'id (agent-config-id config)
        'name (agent-config-name config)
        'persona (agent-config-persona config)
        'human (agent-config-human config)
        'llm_provider (symbol->string (agent-config-llm-provider config))
        'llm_model (agent-config-llm-model config)
        'llm_config (agent-config-llm-config config)
        'max_steps (agent-config-max-steps config)
        'context_window (agent-config-context-window config)
        'tools_enabled (agent-config-tools-enabled config)
        'memory_enabled (agent-config-memory-enabled config)
        'streaming_enabled (agent-config-streaming-enabled config)
        'metadata (agent-config-metadata config)))

(def (agent-state->hash state)
  "Convert agent state to hash

   Args:
     state: Agent state

   Returns:
     Hash representation"

  (let ((h (make-hash-table)))
    (hash-put! h 'agent_id (agent-state-agent-id state))
    (hash-put! h 'step_count (agent-state-step-count state))
    (hash-put! h 'message_count (agent-state-message-count state))
    (hash-put! h 'token_count (agent-state-token-count state))
    (hash-put! h 'last_activity (agent-state-last-activity state))
    (hash-put! h 'status (symbol->string (agent-state-status state)))
    (hash-put! h 'error (agent-state-error state))
    (hash-put! h 'metadata (agent-state-metadata state))
    h))

(def (execution-step->hash step)
  "Convert execution step to hash

   Args:
     step: Execution step

   Returns:
     Hash representation"

  (let ((h (make-hash-table)))
    (hash-put! h 'id (execution-step-id step))
    (hash-put! h 'agent_id (execution-step-agent-id step))
    (hash-put! h 'step_number (execution-step-step-number step))
    (hash-put! h 'timestamp (execution-step-timestamp step))
    (hash-put! h 'type (symbol->string (execution-step-type step)))
    (hash-put! h 'input (execution-step-input step))
    (hash-put! h 'output (execution-step-output step))
    (hash-put! h 'status (symbol->string (execution-step-status step)))
    (hash-put! h 'duration (execution-step-duration step))
    (hash-put! h 'error (execution-step-error step))
    (hash-put! h 'metadata (execution-step-metadata step))
    h))

(def (execution-result->hash result)
  "Convert execution result to hash

   Args:
     result: Execution result

   Returns:
     Hash representation"

  (let ((h (make-hash-table)))
    (hash-put! h 'success (execution-result-success result))
    (hash-put! h 'agent_id (execution-result-agent-id result))
    (hash-put! h 'steps_executed (execution-result-steps-executed result))
    (hash-put! h 'final_state (agent-state->hash (execution-result-final-state result)))
    (hash-put! h 'output (execution-result-output result))
    (hash-put! h 'error (execution-result-error result))
    (hash-put! h 'duration (execution-result-duration result))
    (hash-put! h 'metadata (execution-result-metadata result))
    h))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (uuid-generate)
  "Generate UUID (placeholder implementation)

   Returns:
     UUID string"

  (let ((timestamp (time->seconds (current-time)))
        (random (random-integer 1000000)))
    (format "~a-~a" timestamp random)))
