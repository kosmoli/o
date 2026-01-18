;;; agent/streaming.ss - Streaming Execution with Callbacks
;;;
;;; Streaming execution system with real-time progress updates and callbacks.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/error
  :o/agent/types
  :o/agent/executor)

;;; ============================================================================
;;; Callback Types
;;; ============================================================================

(defstruct execution-callback
  (on-execution-start    ; Called when execution starts
   on-execution-complete ; Called when execution completes
   on-execution-error    ; Called when execution fails
   on-step-start         ; Called when step starts
   on-step-complete      ; Called when step completes
   on-step-error         ; Called when step fails
   on-progress           ; Called for progress updates
   metadata)             ; Additional metadata
  transparent: #t)

;;; ============================================================================
;;; Callback Events
;;; ============================================================================

(defstruct execution-event
  (type                  ; Event type (:execution-start, :step-start, etc.)
   timestamp             ; Event timestamp
   agent-id              ; Agent ID
   data                  ; Event data (make-hash-table)
   metadata)             ; Additional metadata
  transparent: #t)

;;; Event types
(def event-type-execution-start 'execution-start)
(def event-type-execution-complete 'execution-complete)
(def event-type-execution-error 'execution-error)
(def event-type-step-start 'step-start)
(def event-type-step-complete 'step-complete)
(def event-type-step-error 'step-error)
(def event-type-progress 'progress)

;;; ============================================================================
;;; Streaming Executor
;;; ============================================================================

(defstruct streaming-executor
  (base-executor         ; Base step executor
   callbacks             ; Execution callbacks
   metadata)             ; Additional metadata
  transparent: #t)

(def (make-streaming-executor base-executor . rest (callbacks #f))
  "Create streaming executor with callbacks

   Args:
     base-executor: Base step executor
     callbacks: Execution callbacks (optional)

   Returns:
     Streaming executor"

  (make-streaming-executor
   base-executor: base-executor
   callbacks: (or callbacks (make-default-callbacks))
   metadata: (make-hash-table)))

;;; ============================================================================
;;; Default Callbacks
;;; ============================================================================

(def (make-default-callbacks)
  "Create default callbacks (no-op)

   Returns:
     Execution callbacks"

  (make-execution-callback
   on-execution-start: (lambda (event) (void))
   on-execution-complete: (lambda (event) (void))
   on-execution-error: (lambda (event) (void))
   on-step-start: (lambda (event) (void))
   on-step-complete: (lambda (event) (void))
   on-step-error: (lambda (event) (void))
   on-progress: (lambda (event) (void))
   metadata: (make-hash-table)))

(def (make-logging-callbacks)
  "Create logging callbacks for debugging

   Returns:
     Execution callbacks"

  (make-execution-callback
   on-execution-start: (lambda (event)
                        (displayln (format "[START] Execution started for agent ~a"
                                          (execution-event-agent-id event))))
   on-execution-complete: (lambda (event)
                           (displayln (format "[COMPLETE] Execution completed for agent ~a"
                                             (execution-event-agent-id event))))
   on-execution-error: (lambda (event)
                        (displayln (format "[ERROR] Execution failed for agent ~a: ~a"
                                          (execution-event-agent-id event)
                                          (hash-ref (execution-event-data event) 'error))))
   on-step-start: (lambda (event)
                   (displayln (format "[STEP START] Step ~a: ~a"
                                     (hash-ref (execution-event-data event) 'step_number)
                                     (hash-ref (execution-event-data event) 'step_type))))
   on-step-complete: (lambda (event)
                      (displayln (format "[STEP COMPLETE] Step ~a completed in ~as"
                                        (hash-ref (execution-event-data event) 'step_number)
                                        (hash-ref (execution-event-data event) 'duration))))
   on-step-error: (lambda (event)
                   (displayln (format "[STEP ERROR] Step ~a failed: ~a"
                                     (hash-ref (execution-event-data event) 'step_number)
                                     (hash-ref (execution-event-data event) 'error))))
   on-progress: (lambda (event)
                 (displayln (format "[PROGRESS] ~a% (~a/~a steps)"
                                   (hash-ref (execution-event-data event) 'percent)
                                   (hash-ref (execution-event-data event) 'current)
                                   (hash-ref (execution-event-data event) 'total))))
   metadata: (make-hash-table)))

;;; ============================================================================
;;; Event Creation
;;; ============================================================================

(def (make-event type agent-id data)
  "Create execution event

   Args:
     type: Event type
     agent-id: Agent ID
     data: Event data

   Returns:
     Execution event"

  (make-execution-event
   type: type
   timestamp: (current-seconds)
   agent-id: agent-id
   data: data
   metadata: (make-hash-table)))

;;; ============================================================================
;;; Callback Invocation
;;; ============================================================================

(def (invoke-callback callbacks event)
  "Invoke appropriate callback for event

   Args:
     callbacks: Execution callbacks
     event: Execution event

   Returns:
     void"

  (let ((type (execution-event-type event)))
    (case type
      (('execution-start)
       ((execution-callback-on-execution-start callbacks) event))
      (('execution-complete)
       ((execution-callback-on-execution-complete callbacks) event))
      (('execution-error)
       ((execution-callback-on-execution-error callbacks) event))
      (('step-start)
       ((execution-callback-on-step-start callbacks) event))
      (('step-complete)
       ((execution-callback-on-step-complete callbacks) event))
      (('step-error)
       ((execution-callback-on-step-error callbacks) event))
      (('progress)
       ((execution-callback-on-progress callbacks) event))
      (else
       (void)))))

;;; ============================================================================
;;; Streaming Execution
;;; ============================================================================

(def (execute-agent-streaming executor context)
  "Execute agent with streaming callbacks

   Args:
     executor: Streaming executor
     context: Execution context

   Returns:
     Execution result"

  (let ((start-time (current-seconds))
        (base-executor (streaming-executor-base-executor executor))
        (callbacks (streaming-executor-callbacks executor))
        (config (execution-context-agent-config context))
        (state (execution-context-agent-state context))
        (agent-id (execution-context-agent-id context))
        (max-steps (agent-config-max-steps config)))

    ;; Fire execution start event
    (invoke-callback callbacks
                    (make-event event-type-execution-start
                               agent-id
                               (let ((ht (make-hash-table)))
  (hash-put! ht 'config (agent-config->hash config))
  ht)))

    ;; Update agent state to running
    (agent-state-status-set! state agent-status-running)
    (agent-state-last-activity-set! state (current-seconds))

    (try
     ;; Execute steps with callbacks
     (let loop ((step-num 0))
       (cond
        ;; Check if max steps reached
        ((>= step-num max-steps)
         (let ((result (make-execution-result
                        success: #t
                        agent-id: agent-id
                        steps-executed: step-num
                        final-state: state
                        output: "Max steps reached"
                        error: #f
                        duration: (- (current-seconds) start-time)
                        metadata: (make-hash-table))))
           ;; Fire execution complete event
           (invoke-callback callbacks
                           (make-event event-type-execution-complete
                                      agent-id
                                      (let ((ht (make-hash-table)))
  (hash-put! ht 'result (execution-result->hash result))
  ht)))
           result))

        ;; Continue execution
        (else
         ;; Determine next step
         (let ((next-step (determine-next-step base-executor context)))
           (if next-step
               (begin
                 ;; Fire step start event
                 (invoke-callback callbacks
                                 (make-event event-type-step-start
                                            agent-id
                                            (let ((ht (make-hash-table)))
  (hash-put! ht 'step_number step-num)
  (hash-put! ht 'step_type (execution-step-type next-step))
  ht)))

                 ;; Execute step
                 (let ((executed-step (execute-step base-executor context next-step)))
                   ;; Add to step history
                   (execution-context-step-history-set!
                    context
                    (cons executed-step (execution-context-step-history context)))

                   ;; Update state
                   (agent-state-step-count-set! state (+ (agent-state-step-count state) 1))
                   (agent-state-last-activity-set! state (current-seconds))

                   ;; Fire progress event
                   (let ((progress-percent (inexact->exact
                                           (floor (* 100 (/ (+ step-num 1) max-steps))))))
                     (invoke-callback callbacks
                                     (make-event event-type-progress
                                                agent-id
                                                (let ((ht (make-hash-table)))
  (hash-put! ht 'current (+ step-num 1))
  ht))))

                   ;; Check if step failed
                   (if (step-failed? executed-step)
                       ;; Step failed - fire error event and return
                       (begin
                         (agent-state-status-set! state agent-status-error)
                         (agent-state-error-set! state (execution-step-error executed-step))

                         ;; Fire step error event
                         (invoke-callback callbacks
                                         (make-event event-type-step-error
                                                    agent-id
                                                    (let ((ht (make-hash-table)))
  (hash-put! ht 'step_number step-num)
  (hash-put! ht 'error (execution-step-error executed-step))
  ht)))

                         ;; Fire execution error event
                         (let ((result (make-execution-result
                                        success: #f
                                        agent-id: agent-id
                                        steps-executed: (+ step-num 1)
                                        final-state: state
                                        output: #f
                                        error: (execution-step-error executed-step)
                                        duration: (- (current-seconds) start-time)
                                        metadata: (make-hash-table))))
                           (invoke-callback callbacks
                                           (make-event event-type-execution-error
                                                      agent-id
                                                      (let ((ht (make-hash-table)))
  (hash-put! ht 'result (execution-result->hash result))
  ht)))
                           result))

                       ;; Step succeeded - fire complete event and continue
                       (begin
                         ;; Fire step complete event
                         (invoke-callback callbacks
                                         (make-event event-type-step-complete
                                                    agent-id
                                                    (let ((ht (make-hash-table)))
  (hash-put! ht 'step_number step-num)
  (hash-put! ht 'step_type (execution-step-type executed-step))
  ht)))
                         (loop (+ step-num 1))))))

               ;; No more steps - execution complete
               (begin
                 (agent-state-status-set! state agent-status-completed)
                 (let ((result (make-execution-result
                                success: #t
                                agent-id: agent-id
                                steps-executed: step-num
                                final-state: state
                                output: "Execution complete"
                                error: #f
                                duration: (- (current-seconds) start-time)
                                metadata: (make-hash-table))))
                   ;; Fire execution complete event
                   (invoke-callback callbacks
                                   (make-event event-type-execution-complete
                                              agent-id
                                              (let ((ht (make-hash-table)))
  (hash-put! ht 'result (execution-result->hash result))
  ht)))
                   result)))))))

     (catch (e)
       ;; Execution error
       (agent-state-status-set! state agent-status-error)
       (agent-state-error-set! state (error-message e))
       (let ((result (make-execution-result
                      success: #f
                      agent-id: agent-id
                      steps-executed: (agent-state-step-count state)
                      final-state: state
                      output: #f
                      error: (error-message e)
                      duration: (- (current-seconds) start-time)
                      metadata: (make-hash-table))))
         ;; Fire execution error event
         (invoke-callback callbacks
                         (make-event event-type-execution-error
                                    agent-id
                                    (let ((ht (make-hash-table)))
  (hash-put! ht 'result (execution-result->hash result))
  ht)))
         result)))))

;;; ============================================================================
;;; Callback Builders
;;; ============================================================================

(def (make-custom-callbacks . rest
                            (on-execution-start #f)
                            (on-execution-complete #f)
                            (on-execution-error #f)
                            (on-step-start #f)
                            (on-step-complete #f)
                            (on-step-error #f)
                            (on-progress #f))
  "Create custom callbacks

   Args:
     on-execution-start: Execution start callback (optional)
     on-execution-complete: Execution complete callback (optional)
     on-execution-error: Execution error callback (optional)
     on-step-start: Step start callback (optional)
     on-step-complete: Step complete callback (optional)
     on-step-error: Step error callback (optional)
     on-progress: Progress callback (optional)

   Returns:
     Execution callbacks"

  (let ((default (make-default-callbacks)))
    (make-execution-callback
     on-execution-start: (or on-execution-start
                            (execution-callback-on-execution-start default))
     on-execution-complete: (or on-execution-complete
                               (execution-callback-on-execution-complete default))
     on-execution-error: (or on-execution-error
                            (execution-callback-on-execution-error default))
     on-step-start: (or on-step-start
                       (execution-callback-on-step-start default))
     on-step-complete: (or on-step-complete
                          (execution-callback-on-step-complete default))
     on-step-error: (or on-step-error
                       (execution-callback-on-step-error default))
     on-progress: (or on-progress
                     (execution-callback-on-progress default))
     metadata: (make-hash-table))))

