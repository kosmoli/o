#lang racket

;;; agent/dsl.rkt - Domain Specific Language for Agent Definition
;;;
;;; This module provides high-level macros for defining agents, tools,
;;; and conditional behaviors in a declarative style.

(provide defagent
         deftool
         when->
         defstate
         defmemory
         with-checkpoint
         with-evolution
         defperception
         defaction
         agent-loop
         register-tool!
         unregister-tool!
         get-tool
         list-tools
         with-agent-context
         current-agent
         get-current-agent
         defevolution
         match-perception
         async-tool
         when-performance
         perception-matches?
         matches-pattern?
         should-checkpoint?
         get-metric
         error-object->string)

(require racket/hash
        racket/format
        racket/string
        racket/match
        racket/stxparam
        "./core.rkt"
        "../elixir-bridge.rkt")

;;; ============================================================================
;;; defagent - Define an Agent
;;; ============================================================================

;;; Define an agent with optional version and config
;;; Multiple forms supported for flexibility

(define-syntax defagent
  (syntax-rules ()
    ;; Basic form
    [(_ name)
     (defagent-simple name)]

    ;; With version only
    [(_ name version: ver)
     (defagent-simple name #:version ver)]

    ;; With config only
    [(_ name config: cfg)
     (defagent-simple name #:config cfg)]

    ;; With version and config
    [(_ name version: ver config: cfg)
     (defagent-simple name #:version ver #:config cfg)]

    ;; Full form with body clauses
    [(_ name version: ver config: cfg
        (init init-body ...)
        (perceive perceive-body ...)
        (think think-body ...)
        (act act-body ...))
     (defagent-full name ver cfg
                     init-body ...
                     perceive-body ...
                     think-body ...
                     act-body ...)]))

(define-syntax-rule (defagent-simple name #:version [ver "0.1.0"] #:config [cfg (hash)])
  ;; Simple agent definition without custom handlers
  (define name
    (let ([the-agent (make-agent-instance
                      #:name (symbol->string 'name)
                      #:version ver
                      #:config cfg)])
      ;; Default handlers
      (hash-set! (agent-metadata the-agent) 'perceive-fn
                 (lambda (agent input) input))
      (hash-set! (agent-metadata the-agent) 'think-fn
                 (lambda (agent context) #f))
      (hash-set! (agent-metadata the-agent) 'act-fn
                 (lambda (agent action) (void)))
      (agent-initialize! the-agent #f #f)
      the-agent)))

(define-syntax (defagent-full stx)
  (syntax-case stx ()
    [(_ name ver cfg init-clause perceive-clause think-clause act-clause)
     (syntax-case #'(init-clause perceive-clause think-clause act-clause) ()
       [((init init-body ...) (perceive perceive-body ...) (think think-body ...) (act act-body ...))
        #'(begin
            ;; Create agent with custom handlers
            (define name
              (let ([the-agent (make-agent-instance
                                #:name (symbol->string 'name)
                                #:version ver
                                #:config cfg)])
                ;; Store custom methods in metadata
                (hash-set! (agent-metadata the-agent) 'perceive-fn
                           (lambda (agent input) perceive-body ...))
                (hash-set! (agent-metadata the-agent) 'think-fn
                           (lambda (agent context) think-body ...))
                (hash-set! (agent-metadata the-agent) 'act-fn
                           (lambda (agent action) act-body ...))

                ;; Initialize agent
                (agent-initialize! the-agent #f #f)
                the-agent))

            ;; Define helper functions for agent interaction
            (define (name-perceive input)
              (let ([perceive-fn (hash-ref (agent-metadata name) 'perceive-fn)])
                (perceive-fn name input)))

            (define (name-think context)
              (let ([think-fn (hash-ref (agent-metadata name) 'think-fn)])
                (think-fn name context)))

            (define (name-act action)
              (let ([act-fn (hash-ref (agent-metadata name) 'act-fn)])
                (act-fn name action))))])]))

;;; ============================================================================
;;; deftool - Define a Tool
;;; ============================================================================

(define-syntax deftool
  (syntax-rules ()
    ;; Basic form
    [(_ name (params ...) body ...)
     (begin
       (define (name params ...)
         body ...)
       (define name-tool-spec
         (hash 'name (symbol->string 'name)
               'description (symbol->string 'name)
               'parameters '(params ...)
               'function name)))]

    ;; With description and parameters
    [(_ name
        description: desc
        parameters: param-spec
        (params ...) body ...)
     (begin
       (define (name params ...)
         body ...)
       (define name-tool-spec
         (hash 'name (symbol->string 'name)
               'description desc
               'parameters param-spec
               'function name)))]))

;;; ============================================================================
;;; when-> - Conditional Pipeline Macro
;;; ============================================================================

(define-syntax when->
  (syntax-rules ()
    [(_ value) value]
    [(_ value (condition action))
     (if condition action value)]
    [(_ value (condition action) rest ...)
     (let ([result (if condition action value)])
       (when-> result rest ...))]))

;;; ============================================================================
;;; defstate - Define Agent State Structure
;;; ============================================================================

(define-syntax-rule (defstate name (field default) ...)
  (begin
    (struct name (field ...) #:transparent)
    (define (make-name-default)
      (name default ...))))

;;; ============================================================================
;;; defmemory - Define Memory Block Structure
;;; ============================================================================

(define-syntax-rule (defmemory name (field default) ...)
  (begin
    (struct name (field ...) #:transparent)
    (define (make-name-default)
      (name default ...))))

;;; ============================================================================
;;; with-checkpoint - Execute with Automatic Checkpointing
;;; ============================================================================

(define-syntax-rule (with-checkpoint agent body ...)
  (let ([checkpoint-id (agent-checkpoint! agent)])
    (with-handlers* ([exn:fail?
                      (lambda (e)
                        (displayln (format "Error during execution: ~a" (exn-message e)))
                        (displayln (format "Rolling back to checkpoint: ~a" checkpoint-id))
                        (agent-restore! checkpoint-id)
                        (raise e))])
      body ...)))

;;; ============================================================================
;;; with-evolution - Execute Evolution Block
;;; ============================================================================

(define-syntax-rule (with-evolution agent body ...)
  (begin
    (agent-begin-evolution! agent)
    (let ([result (with-handlers* ([exn:fail?
                                     (lambda (e)
                                       (displayln (format "Evolution failed: ~a" (exn-message e)))
                                       #f)])
                     body ...)])
      (agent-end-evolution! agent (if result #t #f))
      result)))

;;; ============================================================================
;;; defperception - Define Perception Handler
;;; ============================================================================

(define-syntax-rule (defperception name (input-type) body ...)
  (define (name input)
    (when (perception-matches? input 'input-type)
      body ...)))

;;; ============================================================================
;;; defaction - Define Action Handler
;;; ============================================================================

(define (log-action-helper! action-name params-vec result)
  (elixir-wal-log! action-name
                   (hash 'params (vector->list params-vec)
                         'result result)))

(define-syntax defaction
  (syntax-rules ()
    [(_ name (params ...) body ...)
     (define (name params ...)
       (let ([result (begin body ...)])
         (log-action-helper! 'name (vector 'params ...) result)
         result))]))

;;; ============================================================================
;;; agent-loop - Main Agent Processing Loop
;;; ============================================================================

(define (agent-loop agent perceive-fn think-fn act-fn)
  "Main agent processing loop: perceive -> think -> act

   Args:
     agent: Agent instance
     perceive-fn: Function to perceive input
     think-fn: Function to think about input
     act-fn: Function to perform action

   The loop runs continuously while agent is running"
  (define (loop)
    (when (agent-running? agent)
      (with-handlers* ([exn:fail?
                        (lambda (e)
                          (displayln (format "Error in agent loop: ~a" (exn-message e)))
                          (elixir-send "agent_error"
                                       (hash 'agent_id (agent-id agent)
                                             'error (error-object->string e)
                                             'timestamp (current-seconds))))])
        (let* ([input (perceive-fn agent)]
               [action (think-fn agent input)])
          (when action
            (act-fn agent action))
          (when (should-checkpoint? agent)
            (agent-checkpoint! agent))))
      (sleep 0.1)
      (loop)))
  (loop))

(define (should-checkpoint? agent)
  "Determine if agent should create a checkpoint"
  (let* ([last-checkpoint (agent-updated-at agent)]
         [now (current-seconds)]
         [checkpoint-interval (agent-get-config agent 'checkpoint-interval 300)])
    (> (- now last-checkpoint) checkpoint-interval)))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (perception-matches? input type)
  "Check if input matches perception type"
  (and (hash? input)
       (eq? (hash-ref input 'type #f) type)))

(define (error-object->string e)
  "Convert error object to string"
  (if (exn? e)
      (exn-message e)
      (format "~a" e)))

;;; ============================================================================
;;; Tool Registration Helpers
;;; ============================================================================

(define (register-tool! agent tool-spec)
  "Register a tool with the agent"
  (define tool-name (hash-ref tool-spec 'name))
  (hash-set! (agent-tools agent) tool-name tool-spec)
  (set-agent-updated-at! agent (current-seconds))
  (elixir-wal-log! 'register-tool
                   (hash 'agent_id (agent-id agent)
                         'tool_name tool-name)))

(define (unregister-tool! agent tool-name)
  "Unregister a tool from the agent"
  (hash-remove! (agent-tools agent) tool-name)
  (set-agent-updated-at! agent (current-seconds))
  (elixir-wal-log! 'unregister-tool
                   (hash 'agent_id (agent-id agent)
                         'tool_name tool-name)))

(define (get-tool agent tool-name)
  "Get a tool by name"
  (hash-ref (agent-tools agent) tool-name #f))

(define (list-tools agent)
  "List all registered tools"
  (hash-keys (agent-tools agent)))

;;; ============================================================================
;;; Macro Utilities
;;; ============================================================================

(define-syntax-parameter current-agent #f)

(define-syntax-rule (with-agent-context agent body ...)
  (syntax-parameterize ([current-agent
                         (make-rename-transformer #'agent)])
    body ...))

(define-syntax-rule (get-current-agent)
  current-agent)

;;; ============================================================================
;;; Evolution DSL
;;; ============================================================================

(define-syntax defevolution
  (syntax-rules ()
    [(_ name (trigger trigger-body ...) (generate generate-body ...) (evaluate evaluate-body ...))
     (define (name agent)
       (let* ([should-evolve? (begin trigger-body ...)]
              [new-code (if should-evolve? (begin generate-body ...) #f)]
              [evaluation (if new-code (begin evaluate-body ...) #f)])
         (hash 'should_evolve should-evolve?
               'new_code new-code
               'evaluation evaluation)))]))

;;; ============================================================================
;;; Pattern Matching for Perceptions
;;; ============================================================================

(define-syntax match-perception
  (syntax-rules ()
    [(_ input) #f]
    [(_ input
        ((pattern guard) body ...)
        rest ...)
     (if (and (matches-pattern? input 'pattern) guard)
         (begin body ...)
         (match-perception input rest ...))]
    [(_ input
        (pattern body ...)
        rest ...)
     (if (matches-pattern? input 'pattern)
         (begin body ...)
         (match-perception input rest ...))]))

(define (matches-pattern? input pattern)
  "Check if input matches pattern"
  (cond
   [(symbol? pattern)
    (eq? (hash-ref input 'type #f) pattern)]
   [(list? pattern)
    (and (hash? input)
         (for/and ([key-value (in-list pattern)])
           (define key (if (pair? key-value) (car key-value) key-value))
           (hash-has-key? input key)))]
   [else #f]))

;;; ============================================================================
;;; Async Tool Execution
;;; ============================================================================

(define-syntax-rule (async-tool agent tool-name arg ...)
  (thread
   (lambda ()
     (let* ([tool (get-tool agent tool-name)]
            [tool-fn (hash-ref tool 'function)]
            [result (with-handlers* ([exn:fail?
                                       (lambda (e)
                                         (hash 'error (error-object->string e)))])
                      (tool-fn arg ...))])
       (elixir-send "tool_result"
                    (hash 'agent_id (agent-id agent)
                          'tool_name tool-name
                          'result result))))))

;;; ============================================================================
;;; Conditional Evolution Triggers
;;; ============================================================================

(define-syntax when-performance
  (syntax-rules ()
    [(_ metric operator threshold body ...)
     (let ([current-value (get-metric 'metric)])
       (when (operator current-value threshold)
         body ...))]))

(define (get-metric metric)
  "Get current performance metric (placeholder)"
  0)

;;; ============================================================================
;;; Example Usage
;;; ============================================================================
;;;
;;; ;; Example 1: Define a simple agent
;;; (defagent my-agent
;;;   version: "1.0.0"
;;;   config: (hash 'checkpoint-interval 300)
;;;
;;;   (init
;;;    (displayln "Initializing agent..."))
;;;
;;;   (perceive input
;;;    (displayln (format "Perceived: ~a" input))
;;;    input)
;;;
;;;   (think context
;;;    (displayln (format "Thinking about: ~a" context))
;;;    (hash 'action 'respond 'data context))
;;;
;;;   (act action
;;;    (displayln (format "Acting: ~a" action))))
;;;
;;; ;; Example 2: Define a tool
;;; (deftool calculate-sum
;;;   description: "Calculate sum of two numbers"
;;;   parameters: '((a number) (b number))
;;;   (a b)
;;;   (+ a b))
;;;
;;; ;; Example 3: Conditional pipeline
;;; (when-> initial-value
;;;   ((> value 10) (process-large value))
;;;   ((< value 0) (process-negative value))
;;;   (else (process-normal value)))
;;;
;;; ;; Example 4: Define state structure
;;; (defstate my-state
;;;   (counter 0)
;;;   (last-update 0))
;;;
;;; ;; Example 5: Evolution definition
;;; (defevolution improve-performance
;;;   (trigger
;;;    (> (get-metric 'latency) 100))
;;;
;;;   (generate
;;;    (generate-optimized-code agent))
;;;
;;;   (evaluate
;;;    (test-in-shadow-instance new-code)))
;;;
;;; ;; Example 6: Async tool execution
;;; (async-tool my-agent 'calculate-sum 10 20)
;;;
;;; ;;; ============================================================================
