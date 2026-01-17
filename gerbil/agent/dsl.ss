;;; agent/dsl.ss - Domain Specific Language for Agent Definition
;;;
;;; This module provides high-level macros for defining agents, tools,
;;; and conditional behaviors in a declarative style.

(export #t)

(import
  :std/misc/threads
  :std/sugar
  :std/format
  :std/srfi/1
  :std/misc/hash
  :o/agent/core
  :o/agent/elixir-bridge)

;;; ============================================================================
;;; defagent - Define an Agent
;;; ============================================================================

(defrules defagent ()
  ;; Basic form: (defagent name body ...)
  ((_ name body ...)
   (defagent name version: "0.1.0" body ...))

  ;; With version: (defagent name version: "1.0" body ...)
  ((_ name version: ver body ...)
   (defagent name version: ver config: (hash) body ...))

  ;; Full form with config
  ((_ name version: ver config: cfg
      (init init-body ...)
      (perceive perceive-body ...)
      (think think-body ...)
      (act act-body ...)
      clauses ...)
   (begin
     (def name
       (let* ((agent (make-agent-instance
                      name: (symbol->string 'name)
                      version: ver
                      config: cfg))
              (state-init (lambda (agent) init-body ...))
              (memory-init (lambda (agent) #f)))  ; Will be initialized properly

         ;; Define agent methods
         (hash-put! (agent-metadata agent) 'perceive-fn
                    (lambda (agent input) perceive-body ...))
         (hash-put! (agent-metadata agent) 'think-fn
                    (lambda (agent context) think-body ...))
         (hash-put! (agent-metadata agent) 'act-fn
                    (lambda (agent action) act-body ...))

         ;; Initialize agent
         (agent-initialize! agent (state-init agent) (memory-init agent))

         agent))

     ;; Define helper functions
     (def (name-perceive input)
       (let ((perceive-fn (agent-get-metadata name 'perceive-fn)))
         (perceive-fn name input)))

     (def (name-think context)
       (let ((think-fn (agent-get-metadata name 'think-fn)))
         (think-fn name context)))

     (def (name-act action)
       (let ((act-fn (agent-get-metadata name 'act-fn)))
         (act-fn name action))))))

;;; ============================================================================
;;; deftool - Define a Tool
;;; ============================================================================

(defrules deftool ()
  ;; Basic form: (deftool name (params ...) body ...)
  ((_ name (params ...) body ...)
   (deftool name
            description: (symbol->string 'name)
            parameters: '(params ...)
            (params ...) body ...))

  ;; With description
  ((_ name
      description: desc
      parameters: param-spec
      (params ...) body ...)
   (begin
     (def (name params ...)
       body ...)

     (def name-tool-spec
       (hash
        'name (symbol->string 'name)
        'description desc
        'parameters param-spec
        'function name)))))

;;; ============================================================================
;;; when-> - Conditional Pipeline Macro
;;; ============================================================================

(defrules when-> ()
  ;; Base case: no conditions
  ((_ value)
   value)

  ;; Single condition with action
  ((_ value (condition action))
   (if condition
       action
       value))

  ;; Multiple conditions
  ((_ value (condition action) rest ...)
   (let ((result (if condition action value)))
     (when-> result rest ...))))

;;; ============================================================================
;;; defstate - Define Agent State Structure
;;; ============================================================================

(defrules defstate ()
  ((_ name (field default) ...)
   (begin
     (defstruct name (field ...) transparent: #t)

     (def (make-name-default)
       (make-name field: default ...)))))

;;; ============================================================================
;;; defmemory - Define Memory Block Structure
;;; ============================================================================

(defrules defmemory ()
  ((_ name (field default) ...)
   (begin
     (defstruct name (field ...) transparent: #t)

     (def (make-name-default)
       (make-name field: default ...)))))

;;; ============================================================================
;;; with-checkpoint - Execute with Automatic Checkpointing
;;; ============================================================================

(defrule (with-checkpoint agent body ...)
  (begin
    (let ((checkpoint-id (agent-checkpoint! agent)))
      (try
       (begin body ...)
       (catch (e)
         (displayln (format "Error during execution: ~a" e))
         (displayln (format "Rolling back to checkpoint: ~a" checkpoint-id))
         (agent-restore! checkpoint-id)
         (raise e))))))

;;; ============================================================================
;;; with-evolution - Execute Evolution Block
;;; ============================================================================

(defrule (with-evolution agent body ...)
  (begin
    (agent-begin-evolution! agent)
    (let ((result (try
                   (begin body ...)
                   (catch (e)
                     (displayln (format "Evolution failed: ~a" e))
                     #f))))
      (agent-end-evolution! agent (if result #t #f))
      result)))

;;; ============================================================================
;;; defperception - Define Perception Handler
;;; ============================================================================

(defrules defperception ()
  ((_ name (input-type) body ...)
   (def (name input)
     (when (perception-matches? input 'input-type)
       body ...))))

;;; ============================================================================
;;; defaction - Define Action Handler
;;; ============================================================================

(defrules defaction ()
  ((_ name (params ...) body ...)
   (def (name params ...)
     body ...)))

;;; ============================================================================
;;; agent-loop - Main Agent Processing Loop
;;; ============================================================================

(def (agent-loop agent perceive-fn think-fn act-fn)
  "Main agent processing loop: perceive -> think -> act"
  (let loop ()
    (when (agent-is-running? agent)
      (try
       (begin
         ;; Perceive
         (let* ((input (perceive-fn agent))
                ;; Think
                (action (think-fn agent input)))
           ;; Act
           (when action
             (act-fn agent action))

           ;; Checkpoint periodically
           (when (should-checkpoint? agent)
             (agent-checkpoint! agent))))

       (catch (e)
         (displayln (format "Error in agent loop: ~a" e))
         (let ((error-data (make-hash-table)))
           (hash-put! error-data 'agent_id (agent-id agent))
           (hash-put! error-data 'error (error-object->string e))
           (hash-put! error-data 'timestamp (time->seconds (current-time)))
           (elixir-send "agent_error" error-data))))

      ;; Continue loop
      (thread-sleep! 0.1)
      (loop))))

(def (should-checkpoint? agent)
  "Determine if agent should create a checkpoint"
  (let* ((last-checkpoint (agent-updated-at agent))
         (now (time->seconds (current-time)))
         (checkpoint-interval (agent-get-config agent 'checkpoint-interval 300)))
    (> (- now last-checkpoint) checkpoint-interval)))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(def (perception-matches? input type)
  "Check if input matches perception type"
  (and (hash-table? input)
       (eq? (hash-ref input 'type #f) type)))

(def (error-object->string e)
  "Convert error object to string"
  (with-output-to-string
    (lambda ()
      (display-exception e (current-output-port)))))

;;; ============================================================================
;;; Tool Registration Helpers
;;; ============================================================================

(def (register-tool! agent tool-spec)
  "Register a tool with the agent"
  (let ((tool-name (hash-ref tool-spec 'name)))
    (hash-put! (agent-tools agent) tool-name tool-spec)
    (set! (agent-updated-at agent) (time->seconds (current-time)))

    ;; Log to WAL
    (let ((log-data (make-hash-table)))
      (hash-put! log-data 'agent_id (agent-id agent))
      (hash-put! log-data 'tool_name tool-name)
      (elixir-wal-log! 'register-tool log-data))))

(def (unregister-tool! agent tool-name)
  "Unregister a tool from the agent"
  (hash-remove! (agent-tools agent) tool-name)
  (set! (agent-updated-at agent) (time->seconds (current-time)))

  ;; Log to WAL
  (let ((log-data (make-hash-table)))
    (hash-put! log-data 'agent_id (agent-id agent))
    (hash-put! log-data 'tool_name tool-name)
    (elixir-wal-log! 'unregister-tool log-data)))

(def (get-tool agent tool-name)
  "Get a tool by name"
  (hash-ref (agent-tools agent) tool-name #f))

(def (list-tools agent)
  "List all registered tools"
  (hash-keys (agent-tools agent)))

;;; ============================================================================
;;; Macro Utilities
;;; ============================================================================

(defrule (with-agent-context agent body ...)
  "Execute body with agent context bound"
  (parameterize ((current-agent agent))
    body ...))

(def current-agent (make-parameter #f))

(def (get-current-agent)
  "Get the current agent from parameter"
  (or (current-agent)
      (error "No agent in current context")))

;;; ============================================================================
;;; Evolution DSL
;;; ============================================================================

(defrules defevolution ()
  ((_ name
      (trigger trigger-body ...)
      (generate generate-body ...)
      (evaluate evaluate-body ...))
   (begin
     (def (name agent)
       (let* ((should-evolve? (begin trigger-body ...))
              (new-code (if should-evolve?
                            (begin generate-body ...)
                            #f))
              (evaluation (if new-code
                              (begin evaluate-body ...)
                              #f)))
         (hash
          'should_evolve should-evolve?
          'new_code new-code
          'evaluation evaluation))))))

;;; ============================================================================
;;; Pattern Matching for Perceptions
;;; ============================================================================

(defrules match-perception ()
  ;; Base case
  ((_ input)
   #f)

  ;; Pattern with guard
  ((_ input
      ((pattern guard) body ...)
      rest ...)
   (if (and (matches-pattern? input 'pattern) guard)
       (begin body ...)
       (match-perception input rest ...)))

  ;; Pattern without guard
  ((_ input
      (pattern body ...)
      rest ...)
   (if (matches-pattern? input 'pattern)
       (begin body ...)
       (match-perception input rest ...))))

(def (matches-pattern? input pattern)
  "Check if input matches pattern"
  (cond
   ((symbol? pattern)
    (eq? (hash-ref input 'type #f) pattern))
   ((list? pattern)
    (and (hash-table? input)
         (every (lambda (key)
                  (hash-key? input key))
                (map car pattern))))
   (else #f)))

;;; ============================================================================
;;; Async Tool Execution
;;; ============================================================================

(defrule (async-tool agent tool-name args ...)
  "Execute tool asynchronously"
  (spawn
   (lambda ()
     (let* ((tool (get-tool agent tool-name))
            (tool-fn (hash-ref tool 'function))
            (result (try
                     (tool-fn args ...)
                     (catch (e)
                       (hash 'error (error-object->string e))))))
       ;; Send result back
       (elixir-send "tool_result"
                    (hash 'agent_id (agent-id agent)
                          'tool_name tool-name
                          'result result))))))

;;; ============================================================================
;;; Conditional Evolution Triggers
;;; ============================================================================

(defrules when-performance ()
  ((_ metric operator threshold body ...)
   (let ((current-value (get-metric metric)))
     (when (operator current-value threshold)
       body ...))))

(def (get-metric metric)
  "Get current performance metric"
  ;; Placeholder - will be implemented with actual metrics
  0)

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Example 1: Define a simple agent
(defagent my-agent
  version: "1.0.0"
  config: (hash 'checkpoint-interval 300)

  (init
   (displayln "Initializing agent..."))

  (perceive input
   (displayln (format "Perceived: ~a" input))
   input)

  (think context
   (displayln (format "Thinking about: ~a" context))
   (hash 'action 'respond 'data context))

  (act action
   (displayln (format "Acting: ~a" action))))

;; Example 2: Define a tool
(deftool calculate-sum
  description: "Calculate sum of two numbers"
  parameters: '((a number) (b number))
  (a b)
  (+ a b))

;; Example 3: Conditional pipeline
(when-> initial-value
  ((> value 10) (process-large value))
  ((< value 0) (process-negative value))
  (else (process-normal value)))

;; Example 4: Evolution definition
(defevolution improve-performance
  (trigger
   (> (get-metric 'latency) 100))

  (generate
   (generate-optimized-code agent))

  (evaluate
   (test-in-shadow-instance new-code)))
|#
