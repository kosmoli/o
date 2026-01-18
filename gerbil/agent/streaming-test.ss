;;; agent/streaming-test.ss - Streaming Execution Tests
;;;
;;; Test suite for streaming execution with callbacks.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/test
  :o/llm/types
  :o/tools/types
  :o/tools/core
  :o/message/types
  :o/memory/types
  :o/agent/types
  :o/agent/executor
  :o/agent/streaming)

;;; ============================================================================
;;; Test Setup
;;; ============================================================================

(def test-agent-id "test-agent-streaming-123")

(def (make-mock-llm-client)
  "Create mock LLM client for testing"
  (lambda (method . args)
    (case method
      (('chat)
       (make-llm-response
        content: "Test response"
        tool-calls: '()
        usage: (let ((ht (make-hash-table)))
  (hash-put! ht 'prompt_tokens 10)
  (hash-put! ht 'completion_tokens 20)
  (hash-put! ht 'total_tokens 30)
  ht)
        finish-reason: "stop"
        metadata: (make-hash-table)))
      (else
       (error "Unknown method" method)))))

(def (make-mock-tool-dispatcher)
  "Create mock tool dispatcher for testing"
  (let ((dispatcher (make-tool-dispatcher)))
    (dispatcher-register-tool! dispatcher
                              (make-tool-definition
                               name: "test_tool"
                               description: "Test tool"
                               parameters: (let ((ht (make-hash-table)))
  (hash-put! ht 'input (hash 'type 'string 'required #t))
  ht)
                               handler: (lambda (args ctx)
                                         (make-success-result
                                          (let ((ht (make-hash-table)))
  (hash-put! ht 'output (hash-ref args 'input))
  ht)))
                               category: 'custom
                               requires-approval: #f
                               metadata: (make-hash-table)))
    dispatcher))

(def (make-mock-message-manager)
  "Create mock message manager for testing"
  (lambda (method . args)
    (case method
      (('create-message)
       (make-message
        id: "msg-123"
        agent-id: test-agent-id
        role: 'user
        content: "Test message"
        timestamp: (current-seconds)
        metadata: (make-hash-table)))
      (else
       (error "Unknown method" method)))))

(def (make-mock-memory-manager)
  "Create mock memory manager for testing"
  (let ((blocks (make-hash-table)))
    (lambda (method . args)
      (case method
        (('get-block)
         (let ((agent-id (car args))
               (block-name (cadr args)))
           (hash-ref blocks block-name
                    (make-memory-block
                     name: block-name
                     label: block-name
                     value: "Test memory"
                     template: #f
                     limit: 1000
                     metadata: (hash)))))
        (('update-block)
         (let ((agent-id (car args))
               (block-name (cadr args))
               (value (caddr args)))
           (hash-put! blocks block-name
                     (make-memory-block
                      name: block-name
                      label: block-name
                      value: value
                      template: #f
                      limit: 1000
                      metadata: (hash)))
           #t))
        (else
         (error "Unknown method" method))))))

(def (setup-test-base-executor)
  "Create test base executor with mocks"
  (make-step-executor
   (make-mock-llm-client)
   (make-mock-tool-dispatcher)
   (make-mock-message-manager)
   (make-mock-memory-manager)
   max-retries: 3))

(def (setup-test-context)
  "Create test execution context"
  (make-execution-context
   agent-id: test-agent-id
   agent-config: (make-default-agent-config test-agent-id "Test Agent")
   agent-state: (make-initial-agent-state test-agent-id)
   conversation-history: '()
   memory-blocks: '()
   step-history: '()
   current-step: 0
   start-time: (current-seconds)
   metadata: (make-hash-table)))

;;; ============================================================================
;;; Callback Structure Tests
;;; ============================================================================

(def callback-structure-tests
  (test-suite "Callback Structure Tests"

    (test-case "Create default callbacks"
      (def callbacks (make-default-callbacks))
      (check (execution-callback? callbacks))
      (check (procedure? (execution-callback-on-execution-start callbacks)))
      (check (procedure? (execution-callback-on-execution-complete callbacks)))
      (check (procedure? (execution-callback-on-step-start callbacks))))

    (test-case "Create logging callbacks"
      (def callbacks (make-logging-callbacks))
      (check (execution-callback? callbacks))
      (check (procedure? (execution-callback-on-execution-start callbacks)))
      (check (procedure? (execution-callback-on-progress callbacks))))

    (test-case "Create custom callbacks"
      (def called (box #f))
      (def callbacks (make-custom-callbacks
                      on-step-start: (lambda (event) (set-box! called #t))))
      (check (execution-callback? callbacks))
      ;; Test custom callback
      (def event (make-event event-type-step-start test-agent-id (make-hash-table)))
      ((execution-callback-on-step-start callbacks) event)
      (check (unbox called)))))

;;; ============================================================================
;;; Event Tests
;;; ============================================================================

(def event-tests
  (test-suite "Event Tests"

    (test-case "Create execution event"
      (def event (make-event event-type-execution-start
                            test-agent-id
                            (let ((ht (make-hash-table)))
  (hash-put! ht 'test "data")
  ht)))
      (check (execution-event? event))
      (check (eq? (execution-event-type event) event-type-execution-start))
      (check (equal? (execution-event-agent-id event) test-agent-id))
      (check (hash-table? (execution-event-data event)))
      (check (equal? (hash-ref (execution-event-data event) 'test) "data")))

    (test-case "Event has timestamp"
      (def event (make-event event-type-step-start test-agent-id (make-hash-table)))
      (check (number? (execution-event-timestamp event)))
      (check (> (execution-event-timestamp event) 0)))

    (test-case "Different event types"
      (def types (list event-type-execution-start
                      event-type-execution-complete
                      event-type-step-start
                      event-type-step-complete
                      event-type-progress))
      (for-each
       (lambda (type)
         (def event (make-event type test-agent-id (make-hash-table)))
         (check (eq? (execution-event-type event) type)))
       types))))

;;; ============================================================================
;;; Callback Invocation Tests
;;; ============================================================================

(def callback-invocation-tests
  (test-suite "Callback Invocation Tests"

    (test-case "Invoke execution start callback"
      (def called (box #f))
      (def callbacks (make-custom-callbacks
                      on-execution-start: (lambda (event) (set-box! called #t))))
      (def event (make-event event-type-execution-start test-agent-id (make-hash-table)))
      (invoke-callback callbacks event)
      (check (unbox called)))

    (test-case "Invoke step complete callback"
      (def called (box #f))
      (def callbacks (make-custom-callbacks
                      on-step-complete: (lambda (event) (set-box! called #t))))
      (def event (make-event event-type-step-complete test-agent-id (make-hash-table)))
      (invoke-callback callbacks event)
      (check (unbox called)))

    (test-case "Invoke progress callback"
      (def progress-data (box #f))
      (def callbacks (make-custom-callbacks
                      on-progress: (lambda (event)
                                    (set-box! progress-data
                                             (execution-event-data event)))))
      (def event (make-event event-type-progress
                            test-agent-id
                            (let ((ht (make-hash-table)))
  (hash-put! ht 'current 5)
  (hash-put! ht 'total 10)
  (hash-put! ht 'percent 50)
  ht)))
      (invoke-callback callbacks event)
      (check (hash-table? (unbox progress-data)))
      (check (= (hash-ref (unbox progress-data) 'percent) 50)))

    (test-case "Invoke error callback"
      (def error-msg (box #f))
      (def callbacks (make-custom-callbacks
                      on-step-error: (lambda (event)
                                      (set-box! error-msg
                                               (hash-ref (execution-event-data event) 'error)))))
      (def event (make-event event-type-step-error
                            test-agent-id
                            (let ((ht (make-hash-table)))
  (hash-put! ht 'error "Test error")
  ht)))
      (invoke-callback callbacks event)
      (check (equal? (unbox error-msg) "Test error")))))

;;; ============================================================================
;;; Streaming Executor Tests
;;; ============================================================================

(def streaming-executor-tests
  (test-suite "Streaming Executor Tests"

    (test-case "Create streaming executor"
      (def base-executor (setup-test-base-executor))
      (def executor (make-streaming-executor base-executor))
      (check (streaming-executor? executor))
      (check (step-executor? (streaming-executor-base-executor executor)))
      (check (execution-callback? (streaming-executor-callbacks executor))))

    (test-case "Create streaming executor with custom callbacks"
      (def base-executor (setup-test-base-executor))
      (def callbacks (make-logging-callbacks))
      (def executor (make-streaming-executor base-executor callbacks: callbacks))
      (check (streaming-executor? executor))
      (check (eq? (streaming-executor-callbacks executor) callbacks)))))

;;; ============================================================================
;;; Streaming Execution Tests
;;; ============================================================================

(def streaming-execution-tests
  (test-suite "Streaming Execution Tests"

    (test-case "Execute agent with streaming callbacks"
      (def base-executor (setup-test-base-executor))
      (def context (setup-test-context))
      (def events (box '()))
      (def callbacks (make-custom-callbacks
                      on-execution-start: (lambda (e) (set-box! events (cons 'start (unbox events))))
                      on-execution-complete: (lambda (e) (set-box! events (cons 'complete (unbox events))))
                      on-step-start: (lambda (e) (set-box! events (cons 'step-start (unbox events))))
                      on-step-complete: (lambda (e) (set-box! events (cons 'step-complete (unbox events))))))
      (def executor (make-streaming-executor base-executor callbacks: callbacks))
      (def result (execute-agent-streaming executor context))
      (check (execution-result? result))
      (check (execution-result-success result))
      ;; Check that callbacks were invoked
      (def event-list (unbox events))
      (check (member 'start event-list))
      (check (member 'complete event-list)))

    (test-case "Streaming execution fires progress events"
      (def base-executor (setup-test-base-executor))
      (def context (setup-test-context))
      (def progress-events (box '()))
      (def callbacks (make-custom-callbacks
                      on-progress: (lambda (e)
                                    (set-box! progress-events
                                             (cons (execution-event-data e) (unbox progress-events))))))
      (def executor (make-streaming-executor base-executor callbacks: callbacks))
      (def result (execute-agent-streaming executor context))
      ;; Check that progress events were fired
      (check (> (length (unbox progress-events)) 0)))

    (test-case "Streaming execution handles errors"
      (def base-executor (setup-test-base-executor))
      (def context (setup-test-context))
      (def error-events (box '()))
      (def callbacks (make-custom-callbacks
                      on-execution-error: (lambda (e) (set-box! error-events (cons e (unbox error-events))))))
      (def executor (make-streaming-executor base-executor callbacks: callbacks))
      ;; Set max steps to 0 to trigger completion
      (agent-config-max-steps-set! (execution-context-agent-config context) 0)
      (def result (execute-agent-streaming executor context))
      (check (execution-result? result)))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(def integration-tests
  (test-suite "Integration Tests"

    (test-case "Full streaming execution cycle"
      (def base-executor (setup-test-base-executor))
      (def context (setup-test-context))
      (def event-log (box '()))
      (def callbacks (make-custom-callbacks
                      on-execution-start: (lambda (e) (set-box! event-log (cons 'exec-start (unbox event-log))))
                      on-execution-complete: (lambda (e) (set-box! event-log (cons 'exec-complete (unbox event-log))))
                      on-step-start: (lambda (e) (set-box! event-log (cons 'step-start (unbox event-log))))
                      on-step-complete: (lambda (e) (set-box! event-log (cons 'step-complete (unbox event-log))))
                      on-progress: (lambda (e) (set-box! event-log (cons 'progress (unbox event-log))))))
      (def executor (make-streaming-executor base-executor callbacks: callbacks))
      (def result (execute-agent-streaming executor context))
      ;; Verify execution succeeded
      (check (execution-result-success result))
      ;; Verify event sequence
      (def events (reverse (unbox event-log)))
      (check (eq? (car events) 'exec-start))
      (check (eq? (car (reverse events)) 'exec-complete)))

    (test-case "Streaming with logging callbacks"
      (def base-executor (setup-test-base-executor))
      (def context (setup-test-context))
      (def callbacks (make-logging-callbacks))
      (def executor (make-streaming-executor base-executor callbacks: callbacks))
      (def result (execute-agent-streaming executor context))
      (check (execution-result? result))
      (check (execution-result-success result)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(def streaming-test-suite
  (test-suite "Streaming Execution Test Suite"
    callback-structure-tests
    event-tests
    callback-invocation-tests
    streaming-executor-tests
    streaming-execution-tests
    integration-tests))

;;; Run tests
#|
(import :std/test)
(test-run! streaming-test-suite)
|#
