#lang racket

;;; agent/types.rkt - Agent Execution Types
;;;
;;; Type definitions for agent execution, steps, and state management.

(provide (struct-out agent-config)
         (struct-out agent-state)
         (struct-out execution-step)
         (struct-out execution-context)
         (struct-out execution-result)
         step-type-user-message
         step-type-llm-inference
         step-type-tool-call
         step-type-memory-update
         step-type-system
         agent-status-idle
         agent-status-running
         agent-status-paused
         agent-status-error
         agent-status-completed
         step-status-pending
         step-status-running
         step-status-completed
         step-status-failed
         make-default-agent-config
         make-initial-agent-state
         make-execution-step-record
         agent-running?
         agent-idle?
         agent-error?
         step-completed?
         step-failed?
         step-running?
         agent-config->hash
         agent-state->hash
         execution-step->hash
         execution-result->hash
         uuid-generate
         current-seconds)

(require racket/hash
         racket/format
         racket/date)

;;; ============================================================================
;;; Agent Configuration
;;; ============================================================================

(struct agent-config
  (id                    ; Agent ID (string)
   name                  ; Agent name (string)
   persona               ; Agent persona (string)
   human                 ; Human description (string)
   llm-provider          ; LLM provider (symbol: 'openai, 'anthropic, 'groq, 'ollama)
   llm-model             ; LLM model name (string)
   llm-config            ; LLM configuration (hash)
   max-steps             ; Maximum steps per execution (integer)
   context-window        ; Context window size in tokens (integer)
   tools-enabled         ; List of enabled tool names
   memory-enabled        ; Is memory system enabled? (boolean)
   streaming-enabled     ; Is streaming enabled? (boolean)
   metadata)             ; Additional metadata (hash)
  #:transparent)

;;; ============================================================================
;;; Agent State
;;; ============================================================================

(struct agent-state
  (agent-id              ; Agent ID (string)
   step-count            ; Current step count (integer)
   message-count         ; Total message count (integer)
   token-count           ; Total token count (integer)
   last-activity         ; Last activity timestamp (seconds)
   status                ; Agent status (symbol: 'idle, 'running, 'paused, 'error)
   error                 ; Error message if status is 'error (optional)
   metadata)             ; Additional state metadata (hash)
  #:transparent
  #:mutable)

;;; ============================================================================
;;; Execution Step
;;; ============================================================================

(struct execution-step
  (id                    ; Step ID (string)
   agent-id              ; Agent ID (string)
   step-number           ; Step number (integer)
   timestamp             ; Step timestamp (seconds)
   type                  ; Step type (symbol)
   input                 ; Step input (any)
   output                ; Step output (any)
   status                ; Step status (symbol)
   duration              ; Step duration in seconds (optional)
   error                 ; Error message if failed (optional)
   metadata)             ; Additional step metadata (hash)
  #:transparent
  #:mutable)

;;; ============================================================================
;;; Execution Context
;;; ============================================================================

(struct execution-context
  (agent-id              ; Agent ID (string)
   agent-config          ; Agent configuration
   agent-state           ; Agent state
   conversation-history  ; List of messages
   memory-blocks         ; Core memory blocks
   step-history          ; List of execution steps
   current-step          ; Current step number (integer)
   start-time            ; Execution start time (seconds)
   metadata)             ; Additional context metadata (hash)
  #:transparent
  #:mutable)

;;; ============================================================================
;;; Execution Result
;;; ============================================================================

(struct execution-result
  (success               ; Was execution successful? (boolean)
   agent-id              ; Agent ID (string)
   steps-executed        ; Number of steps executed (integer)
   final-state           ; Final agent state
   output                ; Execution output (any)
   error                 ; Error message if failed (optional)
   duration              ; Total execution duration in seconds
   metadata)             ; Additional result metadata (hash)
  #:transparent)

;;; ============================================================================
;;; Step Types
;;; ============================================================================

(define step-type-user-message 'user-message)
(define step-type-llm-inference 'llm-inference)
(define step-type-tool-call 'tool-call)
(define step-type-memory-update 'memory-update)
(define step-type-system 'system)

;;; ============================================================================
;;; Agent Status
;;; ============================================================================

(define agent-status-idle 'idle)
(define agent-status-running 'running)
(define agent-status-paused 'paused)
(define agent-status-error 'error)
(define agent-status-completed 'completed)

;;; ============================================================================
;;; Step Status
;;; ============================================================================

(define step-status-pending 'pending)
(define step-status-running 'running)
(define step-status-completed 'completed)
(define step-status-failed 'failed)

;;; ============================================================================
;;; Constructors
;;; ============================================================================

(define (make-default-agent-config agent-id name)
  "Create default agent configuration

   Args:
     agent-id: Agent ID
     name: Agent name

   Returns:
     Agent configuration"

  (agent-config
   agent-id
   name
   "I am a helpful AI assistant."
   "The user I am assisting."
   'anthropic
   "claude-3-5-sonnet-20241022"
   (hash 'temperature 0.7 'max_tokens 4096)
   50
   100000
   '("send_message" "conversation_search"
     "core_memory_append" "core_memory_replace"
     "archival_memory_insert" "archival_memory_search")
   #t
   #f
   (hash)))

(define (make-initial-agent-state agent-id)
  "Create initial agent state

   Args:
     agent-id: Agent ID

   Returns:
     Agent state"

  (agent-state
   agent-id
   0
   0
   0
   (current-seconds)
   agent-status-idle
   #f
   (hash)))

(define (make-execution-step-record agent-id step-number type input)
  "Create execution step record

   Args:
     agent-id: Agent ID
     step-number: Step number
     type: Step type
     input: Step input

   Returns:
     Execution step"

  (execution-step
   (uuid-generate)
   agent-id
   step-number
   (current-seconds)
   type
   input
   #f
   step-status-pending
   #f
   #f
   (hash)))

;;; ============================================================================
;;; Predicates
;;; ============================================================================

(define (agent-running? state)
  "Check if agent is running

   Args:
     state: Agent state

   Returns:
     #t if running, #f otherwise"

  (eq? (agent-state-status state) agent-status-running))

(define (agent-idle? state)
  "Check if agent is idle

   Args:
     state: Agent state

   Returns:
     #t if idle, #f otherwise"

  (eq? (agent-state-status state) agent-status-idle))

(define (agent-error? state)
  "Check if agent has error

   Args:
     state: Agent state

   Returns:
     #t if error, #f otherwise"

  (eq? (agent-state-status state) agent-status-error))

(define (step-completed? step)
  "Check if step is completed

   Args:
     step: Execution step

   Returns:
     #t if completed, #f otherwise"

  (eq? (execution-step-status step) step-status-completed))

(define (step-failed? step)
  "Check if step failed

   Args:
     step: Execution step

   Returns:
     #t if failed, #f otherwise"

  (eq? (execution-step-status step) step-status-failed))

(define (step-running? step)
  "Check if step is running

   Args:
     step: Execution step

   Returns:
     #t if running, #f otherwise"

  (eq? (execution-step-status step) step-status-running))

;;; ============================================================================
;;; Conversion Functions
;;; ============================================================================

(define (agent-config->hash config)
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

(define (agent-state->hash state)
  "Convert agent state to hash

   Args:
     state: Agent state

   Returns:
     Hash representation"

  (hash 'agent_id (agent-state-agent-id state)
        'step_count (agent-state-step-count state)
        'message_count (agent-state-message-count state)
        'token_count (agent-state-token-count state)
        'last_activity (agent-state-last-activity state)
        'status (symbol->string (agent-state-status state))
        'error (agent-state-error state)
        'metadata (agent-state-metadata state)))

(define (execution-step->hash step)
  "Convert execution step to hash

   Args:
     step: Execution step

   Returns:
     Hash representation"

  (hash 'id (execution-step-id step)
        'agent_id (execution-step-agent-id step)
        'step_number (execution-step-step-number step)
        'timestamp (execution-step-timestamp step)
        'type (symbol->string (execution-step-type step))
        'input (execution-step-input step)
        'output (execution-step-output step)
        'status (symbol->string (execution-step-status step))
        'duration (execution-step-duration step)
        'error (execution-step-error step)
        'metadata (execution-step-metadata step)))

(define (execution-result->hash result)
  "Convert execution result to hash

   Args:
     result: Execution result

   Returns:
     Hash representation"

  (hash 'success (execution-result-success result)
        'agent_id (execution-result-agent-id result)
        'steps_executed (execution-result-steps-executed result)
        'final_state (agent-state->hash (execution-result-final-state result))
        'output (execution-result-output result)
        'error (execution-result-error result)
        'duration (execution-result-duration result)
        'metadata (execution-result-metadata result)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (uuid-generate)
  "Generate UUID (simple implementation)

   Returns:
     UUID string"

  (let ([timestamp (current-seconds)]
        [random (random 1000000)])
    (format "~a-~a" timestamp random)))

;;; ============================================================================
;;; Mutable Setter Compatibility Aliases
;;; ============================================================================
;;; Racket uses set-struct-field! naming, Gerbil uses struct-field-set!
;;; These aliases provide Gerbil-style naming for compatibility

(define (agent-state-status-set! state v) (set-agent-state-status! state v))
(define (agent-state-last-activity-set! state v) (set-agent-state-last-activity! state v))
(define (agent-state-step-count-set! state v) (set-agent-state-step-count! state v))
(define (agent-state-error-set! state v) (set-agent-state-error! state v))
(define (agent-state-message-count-set! state v) (set-agent-state-message-count! state v))
(define (agent-state-token-count-set! state v) (set-agent-state-token-count! state v))

(provide agent-state-status-set!
         agent-state-last-activity-set!
         agent-state-step-count-set!
         agent-state-error-set!
         agent-state-message-count-set!
         agent-state-token-count-set!
         make-execution-result)

(define (make-execution-result success agent-id steps-executed final-state
                               output error duration metadata)
  "Create execution result

   Args:
     success: Was execution successful? (boolean)
     agent-id: Agent ID (string)
     steps-executed: Number of steps executed (integer)
     final-state: Final agent state
     output: Execution output (any)
     error: Error message if failed (optional)
     duration: Total execution duration in seconds
     metadata: Additional result metadata (hash)

   Returns:
     Execution result"

  (execution-result
   success
   agent-id
   steps-executed
   final-state
   output
   error
   duration
   metadata))
