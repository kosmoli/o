#lang racket

;;; agent/streaming.rkt - Streaming Execution with Callbacks
;;;
;;; Streaming execution system with real-time progress updates and callbacks.

(provide (struct-out streaming-executor)
         event-type-execution-start
         event-type-execution-complete
         event-type-execution-error
         event-type-step-start
         event-type-step-complete
         event-type-step-error
         event-type-progress
         make-streaming-executor
         make-default-callbacks
         make-logging-callbacks
         make-custom-callbacks
         make-event
         invoke-callback
         execute-agent-streaming)

(require racket/hash
        racket/format
        racket/math
        "./types.rkt"
        "./executor.rkt")

;;; ============================================================================
;;; Callback Structures
;;; ============================================================================

(struct execution-callback
  (on-execution-start
   on-execution-complete
   on-execution-error
   on-step-start
   on-step-complete
   on-step-error
   on-progress
   metadata)
  #:transparent)

(struct execution-event
  (type
   timestamp
   agent-id
   data
   metadata)
  #:transparent)

;;; Event types
(define event-type-execution-start 'execution-start)
(define event-type-execution-complete 'execution-complete)
(define event-type-execution-error 'execution-error)
(define event-type-step-start 'step-start)
(define event-type-step-complete 'step-complete)
(define event-type-step-error 'step-error)
(define event-type-progress 'progress)

;;; ============================================================================
;;; Streaming Executor
;;; ============================================================================

(struct streaming-executor
  (base-executor
   callbacks
   metadata)
  #:transparent)

(define (make-streaming-executor base-executor #:callbacks [callbacks #f])
  (streaming-executor
   base-executor
   (or callbacks (make-default-callbacks))
   (hash)))

;;; ============================================================================
;;; Default Callbacks
;;; ============================================================================

(define (make-default-callbacks)
  (execution-callback
   (lambda (e) (void))
   (lambda (e) (void))
   (lambda (e) (void))
   (lambda (e) (void))
   (lambda (e) (void))
   (lambda (e) (void))
   (lambda (e) (void))
   (hash)))

(define (make-logging-callbacks)
  (execution-callback
   (lambda (e)
     (displayln (format "[START] Execution started for agent ~a"
                        (execution-event-agent-id e))))
   (lambda (e)
     (displayln (format "[COMPLETE] Execution completed for agent ~a"
                        (execution-event-agent-id e))))
   (lambda (e)
     (displayln (format "[ERROR] Execution failed for agent ~a: ~a"
                        (execution-event-agent-id e)
                        (hash-ref (execution-event-data e) 'error))))
   (lambda (e)
     (displayln (format "[STEP START] Step ~a: ~a"
                        (hash-ref (execution-event-data e) 'step_number)
                        (hash-ref (execution-event-data e) 'step_type))))
   (lambda (e)
     (displayln (format "[STEP COMPLETE] Step ~a completed in ~as"
                        (hash-ref (execution-event-data e) 'step_number)
                        (hash-ref (execution-event-data e) 'duration))))
   (lambda (e)
     (displayln (format "[STEP ERROR] Step ~a failed: ~a"
                        (hash-ref (execution-event-data e) 'step_number)
                        (hash-ref (execution-event-data e) 'error))))
   (lambda (e)
     (displayln (format "[PROGRESS] ~a% (~a/~a steps)"
                        (hash-ref (execution-event-data e) 'percent)
                        (hash-ref (execution-event-data e) 'current)
                        (hash-ref (execution-event-data e) 'total))))
   (hash)))

;;; ============================================================================
;;; Event Creation
;;; ============================================================================

(define (make-event type agent-id data #:metadata [metadata (hash)])
  (execution-event
   type
   (current-seconds)
   agent-id
   data
   metadata))

;;; ============================================================================
;;; Callback Invocation
;;; ============================================================================

(define (invoke-callback callbacks event)
  (define type (execution-event-type event))
  (case type
    [(execution-start)
     ((execution-callback-on-execution-start callbacks) event)]
    [(execution-complete)
     ((execution-callback-on-execution-complete callbacks) event)]
    [(execution-error)
     ((execution-callback-on-execution-error callbacks) event)]
    [(step-start)
     ((execution-callback-on-step-start callbacks) event)]
    [(step-complete)
     ((execution-callback-on-step-complete callbacks) event)]
    [(step-error)
     ((execution-callback-on-step-error callbacks) event)]
    [(progress)
     ((execution-callback-on-progress callbacks) event)]
    [else (void)]))

;;; ============================================================================
;;; Streaming Execution
;;; ============================================================================

(define (execute-agent-streaming executor context)
  (define start-time (current-seconds))
  (define base-executor (streaming-executor-base-executor executor))
  (define callbacks (streaming-executor-callbacks executor))
  (define config (execution-context-agent-config context))
  (define state (execution-context-agent-state context))
  (define agent-id (execution-context-agent-id context))
  (define max-steps (agent-config-max-steps config))

  (invoke-callback callbacks
                  (make-event event-type-execution-start
                             agent-id
                             (hash 'config (agent-config->hash config)
                                   'state (agent-state->hash state))))

  (agent-state-status-set! state 'running)
  (agent-state-last-activity-set! state (current-seconds))

  (with-handlers* ([exn:fail?
                     (lambda (e)
                       (agent-state-status-set! state 'error)
                       (agent-state-error-set! state (exn-message e))
                       (define result (make-execution-result
                                      #f
                                      agent-id
                                      (agent-state-step-count state)
                                      state
                                      #f
                                      (exn-message e)
                                      (- (current-seconds) start-time)
                                      (hash)))
                       (invoke-callback callbacks
                                       (make-event event-type-execution-error
                                                  agent-id
                                                  (hash 'result (execution-result->hash result)
                                                        'error (exn-message e))))
                       result)])

    (define (loop step-num)
      (cond
       [(>= step-num max-steps)
        (define result (make-execution-result #t agent-id step-num state
                                               "Max steps reached" #f
                                               (- (current-seconds) start-time) (hash)))
        (invoke-callback callbacks
                        (make-event event-type-execution-complete agent-id
                                   (hash 'result (execution-result->hash result))))
        result]

       [(not (determine-next-step base-executor context))
        (agent-state-status-set! state 'completed)
        (define result (make-execution-result #t agent-id step-num state
                                               "Execution complete" #f
                                               (- (current-seconds) start-time) (hash)))
        (invoke-callback callbacks
                        (make-event event-type-execution-complete agent-id
                                   (hash 'result (execution-result->hash result))))
        result]

       [else
        (define next-step (determine-next-step base-executor context))
        (invoke-callback callbacks
                        (make-event event-type-step-start agent-id
                                   (hash 'step_number step-num
                                         'step_type (execution-step-type next-step))))

        (define executed-step (execute-step base-executor context next-step))
        (set-execution-context-step-history! context
              (cons executed-step (execution-context-step-history context)))

        (agent-state-step-count-set! state (+ (agent-state-step-count state) 1))
        (agent-state-last-activity-set! state (current-seconds))

        (define progress-percent (inexact->exact (floor (* 100 (/ (+ step-num 1) max-steps)))))
        (invoke-callback callbacks
                        (make-event event-type-progress agent-id
                                   (hash 'current (+ step-num 1)
                                         'total max-steps
                                         'percent progress-percent)))

        (cond
         [(step-failed? executed-step)
          (agent-state-status-set! state 'error)
          (agent-state-error-set! state (execution-step-error executed-step))
          (invoke-callback callbacks
                          (make-event event-type-step-error agent-id
                                     (hash 'step_number step-num
                                           'error (execution-step-error executed-step))))
          (define result (make-execution-result #f agent-id (+ step-num 1) state #f
                                                 (execution-step-error executed-step)
                                                 (- (current-seconds) start-time) (hash)))
          (invoke-callback callbacks
                          (make-event event-type-execution-error agent-id
                                     (hash 'result (execution-result->hash result)
                                           'error (execution-step-error executed-step))))
          result]

         [else
          (invoke-callback callbacks
                          (make-event event-type-step-complete agent-id
                                     (hash 'step_number step-num
                                           'step_type (execution-step-type executed-step)
                                           'duration (execution-step-duration executed-step))))
          (loop (+ step-num 1))])]))

    (loop 0)))

;;; ============================================================================
;;; Callback Builders
;;; ============================================================================

(define (make-custom-callbacks #:on-execution-start [on-execution-start #f]
                               #:on-execution-complete [on-execution-complete #f]
                               #:on-execution-error [on-execution-error #f]
                               #:on-step-start [on-step-start #f]
                               #:on-step-complete [on-step-complete #f]
                               #:on-step-error [on-step-error #f]
                               #:on-progress [on-progress #f])
  (define default (make-default-callbacks))
  (execution-callback
   (or on-execution-start
       (execution-callback-on-execution-start default))
   (or on-execution-complete
       (execution-callback-on-execution-complete default))
   (or on-execution-error
       (execution-callback-on-execution-error default))
   (or on-step-start
       (execution-callback-on-step-start default))
   (or on-step-complete
       (execution-callback-on-step-complete default))
   (or on-step-error
       (execution-callback-on-step-error default))
   (or on-progress
       (execution-callback-on-progress default))
   (hash)))
