;;; agent/executor-test.ss - Agent Executor Tests
;;;
;;; Test suite for agent step executor and execution loop.

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
  :o/agent/executor)

;;; ============================================================================
;;; Test Setup
;;; ============================================================================

(def test-agent-id "test-agent-executor-123")

(def (make-mock-llm-client)
  "Create mock LLM client for testing"
  (lambda (method . args)
    (case method
      (('chat)
       ;; Return mock LLM response
       (make-llm-response
        content: "This is a test response"
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
    ;; Register a test tool
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
       ;; Return mock message
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

(def (setup-test-executor)
  "Create test executor with mocks"
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
;;; Step Executor Tests
;;; ============================================================================

(def step-executor-tests
  (test-suite "Step Executor Tests"

    (test-case "Create step executor"
      (def executor (setup-test-executor))
      (check (step-executor? executor))
      (check (= (step-executor-max-retries executor) 3)))

    (test-case "Execute user message step"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      (def step (make-execution-step-record
                 test-agent-id
                 0
                 step-type-user-message
                 (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Hello")
  ht)))
      (def result (execute-step executor context step))
      (check (step-completed? result))
      (check (hash-table? (execution-step-output result)))
      (check (hash-key? (execution-step-output result) 'message_id)))

    (test-case "Execute LLM inference step"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      (def step (make-execution-step-record
                 test-agent-id
                 0
                 step-type-llm-inference
                 (make-hash-table)))
      (def result (execute-step executor context step))
      (check (step-completed? result))
      (check (hash-table? (execution-step-output result)))
      (check (hash-key? (execution-step-output result) 'content))
      (check (equal? (hash-ref (execution-step-output result) 'content)
                    "This is a test response")))

    (test-case "Execute tool call step"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      (def step (make-execution-step-record
                 test-agent-id
                 0
                 step-type-tool-call
                 (let ((ht (make-hash-table)))
  (hash-put! ht 'tool_name "test_tool")
  (hash-put! ht 'arguments (hash 'input "test"))
  ht)))
      (def result (execute-step executor context step))
      (check (step-completed? result))
      (check (hash-table? (execution-step-output result)))
      (check (equal? (hash-ref (execution-step-output result) 'tool_name)
                    "test_tool")))

    (test-case "Execute memory update step - append"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      (def step (make-execution-step-record
                 test-agent-id
                 0
                 step-type-memory-update
                 (let ((ht (make-hash-table)))
  (hash-put! ht 'operation 'append)
  (hash-put! ht 'block_name "persona")
  (hash-put! ht 'content "Additional info")
  ht)))
      (def result (execute-step executor context step))
      (check (step-completed? result))
      (check (hash-table? (execution-step-output result)))
      (check (equal? (hash-ref (execution-step-output result) 'operation)
                    "append")))

    (test-case "Execute memory update step - replace"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      (def step (make-execution-step-record
                 test-agent-id
                 0
                 step-type-memory-update
                 (let ((ht (make-hash-table)))
  (hash-put! ht 'operation 'replace)
  (hash-put! ht 'block_name "persona")
  (hash-put! ht 'old_content "Test")
  (hash-put! ht 'new_content "Updated")
  ht)))
      (def result (execute-step executor context step))
      (check (step-completed? result))
      (check (hash-table? (execution-step-output result)))
      (check (equal? (hash-ref (execution-step-output result) 'operation)
                    "replace")))

    (test-case "Execute system step"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      (def step (make-execution-step-record
                 test-agent-id
                 0
                 step-type-system
                 (let ((ht (make-hash-table)))
  (hash-put! ht 'action "test")
  ht)))
      (def result (execute-step executor context step))
      (check (step-completed? result))
      (check (hash-table? (execution-step-output result))))

    (test-case "Step execution sets duration"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      (def step (make-execution-step-record
                 test-agent-id
                 0
                 step-type-system
                 (make-hash-table)))
      (def result (execute-step executor context step))
      (check (number? (execution-step-duration result)))
      (check (>= (execution-step-duration result) 0)))

    (test-case "Step execution handles errors"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      ;; Create step with invalid type
      (def step (make-execution-step
                 id: "test-step"
                 agent-id: test-agent-id
                 step-number: 0
                 timestamp: (current-seconds)
                 type: 'invalid-type
                 input: (make-hash-table)
                 output: #f
                 status: step-status-pending
                 duration: #f
                 error: #f
                 metadata: (make-hash-table)))
      (def result (execute-step executor context step))
      (check (step-failed? result))
      (check (string? (execution-step-error result))))))

;;; ============================================================================
;;; Helper Function Tests
;;; ============================================================================

(def helper-function-tests
  (test-suite "Helper Function Tests"

    (test-case "Build LLM messages"
      (def context (setup-test-context))
      (def messages (build-llm-messages context))
      (check (list? messages))
      (check (> (length messages) 0))
      ;; First message should be system message
      (check (eq? (llm-message-role (car messages)) 'system)))

    (test-case "Build system message"
      (def config (make-default-agent-config test-agent-id "Test Agent"))
      (def blocks (list
                   (make-memory-block
                    name: "persona"
                    label: "Persona"
                    value: "I am helpful"
                    template: #f
                    limit: 1000
                    metadata: (make-hash-table))))
      (def msg (build-system-message config blocks))
      (check (string? msg))
      (check (string-contains msg "helpful")))

    (test-case "Get available tools"
      (def executor (setup-test-executor))
      (def config (make-default-agent-config test-agent-id "Test Agent"))
      ;; Set enabled tools to include test_tool
      (agent-config-tools-enabled-set! config '("test_tool"))
      (def tools (get-available-tools executor config))
      (check (list? tools))
      (check (= (length tools) 1))
      (check (equal? (tool-definition-name (car tools)) "test_tool")))

    (test-case "String replace"
      (check (equal? (string-replace "hello world" "world" "there")
                    "hello there"))
      (check (equal? (string-replace "test" "xyz" "abc")
                    "test"))
      (check (equal? (string-replace "aaa" "a" "b")
                    "baa")))

    (test-case "String contains"
      (check (= (string-contains "hello world" "world") 6))
      (check (= (string-contains "test" "es") 1))
      (check (not (string-contains "test" "xyz")))
      (check (= (string-contains "aaa" "a") 0)))))

;;; ============================================================================
;;; Execution Loop Tests
;;; ============================================================================

(def execution-loop-tests
  (test-suite "Execution Loop Tests"

    (test-case "Determine next step - initial"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      (def next-step (determine-next-step executor context))
      (check (execution-step? next-step))
      (check (eq? (execution-step-type next-step) step-type-llm-inference)))

    (test-case "Determine next step - after user message"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      ;; Add user message to history
      (execution-context-step-history-set!
       context
       (list (make-execution-step-record
              test-agent-id
              0
              step-type-user-message
              (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Hello")
  ht))))
      (def next-step (determine-next-step executor context))
      (check (execution-step? next-step))
      (check (eq? (execution-step-type next-step) step-type-llm-inference)))

    (test-case "Determine next step - after LLM inference with no tool calls"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      ;; Add LLM inference to history with no tool calls
      (def llm-step (make-execution-step-record
                     test-agent-id
                     0
                     step-type-llm-inference
                     (make-hash-table)))
      (execution-step-output-set! llm-step
                                 (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Response")
  (hash-put! ht 'tool_calls '())
  ht))
      (execution-context-step-history-set! context (list llm-step))
      (def next-step (determine-next-step executor context))
      ;; Should be #f (no more steps)
      (check (not next-step)))

    (test-case "Determine next step - after LLM inference with tool calls"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      ;; Add LLM inference to history with tool calls
      (def llm-step (make-execution-step-record
                     test-agent-id
                     0
                     step-type-llm-inference
                     (make-hash-table)))
      (execution-step-output-set! llm-step
                                 (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Response")
  (hash-put! ht 'tool_calls (list (hash 'tool_name "test_tool"
                                                              'arguments (hash))))
  ht))
      (execution-context-step-history-set! context (list llm-step))
      (def next-step (determine-next-step executor context))
      (check (execution-step? next-step))
      (check (eq? (execution-step-type next-step) step-type-tool-call)))

    (test-case "Determine next step - after tool call"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      ;; Add tool call to history
      (execution-context-step-history-set!
       context
       (list (make-execution-step-record
              test-agent-id
              0
              step-type-tool-call
              (let ((ht (make-hash-table)))
  (hash-put! ht 'tool_name "test_tool")
  ht))))
      (def next-step (determine-next-step executor context))
      (check (execution-step? next-step))
      (check (eq? (execution-step-type next-step) step-type-llm-inference)))

    (test-case "Execute agent - basic execution"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      (def result (execute-agent executor context))
      (check (execution-result? result))
      (check (execution-result-success result))
      (check (> (execution-result-steps-executed result) 0))
      (check (> (execution-result-duration result) 0)))

    (test-case "Execute agent - updates agent state"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      (def state (execution-context-agent-state context))
      (def initial-status (agent-state-status state))
      (def result (execute-agent executor context))
      ;; State should be updated
      (check (not (eq? (agent-state-status state) initial-status)))
      (check (> (agent-state-step-count state) 0)))

    (test-case "Execute agent - respects max steps"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      ;; Set max steps to 2
      (agent-config-max-steps-set! (execution-context-agent-config context) 2)
      (def result (execute-agent executor context))
      (check (execution-result? result))
      ;; Should stop at max steps
      (check (<= (execution-result-steps-executed result) 2)))

    (test-case "Execute agent - adds steps to history"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      (def result (execute-agent executor context))
      (def history (execution-context-step-history context))
      (check (list? history))
      (check (> (length history) 0))
      (check (= (length history) (execution-result-steps-executed result))))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(def integration-tests
  (test-suite "Integration Tests"

    (test-case "Full execution cycle"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      ;; Execute agent
      (def result (execute-agent executor context))
      ;; Check result
      (check (execution-result-success result))
      (check (> (execution-result-steps-executed result) 0))
      ;; Check state
      (def state (execution-result-final-state result))
      (check (or (eq? (agent-state-status state) agent-status-completed)
                (eq? (agent-state-status state) agent-status-running)))
      (check (> (agent-state-step-count state) 0)))

    (test-case "Execution with conversation history"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      ;; Add conversation history
      (execution-context-conversation-history-set!
       context
       (list (make-message
              id: "msg-1"
              agent-id: test-agent-id
              role: 'user
              content: "Hello"
              timestamp: (current-seconds)
              metadata: (make-hash-table))))
      (def result (execute-agent executor context))
      (check (execution-result-success result)))

    (test-case "Execution with memory blocks"
      (def executor (setup-test-executor))
      (def context (setup-test-context))
      ;; Add memory blocks
      (execution-context-memory-blocks-set!
       context
       (list (make-memory-block
              name: "persona"
              label: "Persona"
              value: "I am helpful"
              template: #f
              limit: 1000
              metadata: (make-hash-table))))
      (def result (execute-agent executor context))
      (check (execution-result-success result)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(def executor-test-suite
  (test-suite "Executor Test Suite"
    step-executor-tests
    helper-function-tests
    execution-loop-tests
    integration-tests))

;;; Run tests
#|
(import :std/test)
(test-run! executor-test-suite)
|#
