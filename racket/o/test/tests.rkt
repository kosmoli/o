#lang racket

;;; test/tests.rkt - Test Suite for Project O Racket Implementation

(require rackunit rackunit/text-ui
         "../agent/types.rkt"
         "../memory/types.rkt"
         "../memory/blocks.rkt"
         (prefix-in llm: "../llm/types.rkt")
         "../tools/types.rkt"
         (prefix-in msg: "../message/types.rkt"))

;;; ============================================================================
;;; Agent Types Tests
;;; ============================================================================

(define agent-types-tests
  (test-suite
   "Agent Types Tests"

   (test-case "Create default agent config"
     (define config (make-default-agent-config "agent-123" "TestAgent"))
     (check-equal? (agent-config-id config) "agent-123")
     (check-equal? (agent-config-name config) "TestAgent")
     (check-equal? (agent-config-llm-provider config) 'anthropic)
     (check-equal? (agent-config-max-steps config) 50))

   (test-case "Create initial agent state"
     (define state (make-initial-agent-state "agent-123"))
     (check-equal? (agent-state-agent-id state) "agent-123")
     (check-equal? (agent-state-step-count state) 0)
     (check-equal? (agent-state-status state) agent-status-idle))

   (test-case "Agent status predicates"
     (define idle-state (agent-state "agent-1" 0 0 0 0 agent-status-idle #f (hash)))
     (define running-state (agent-state "agent-2" 1 0 0 0 agent-status-running #f (hash)))
     (define error-state (agent-state "agent-3" 0 0 0 0 agent-status-error "error" (hash)))

     (check-true (agent-idle? idle-state))
     (check-false (agent-running? idle-state))
     (check-true (agent-running? running-state))
     (check-true (agent-error? error-state)))

   (test-case "Execution step"
     (define step (make-execution-step-record "agent-123" 1 step-type-user-message "hello"))
     (check-true (execution-step? step))
     (check-equal? (execution-step-agent-id step) "agent-123")
     (check-equal? (execution-step-step-number step) 1)
     (check-equal? (execution-step-type step) step-type-user-message))

   (test-case "Agent config to hash"
     (define config (make-default-agent-config "agent-123" "TestAgent"))
     (define h (agent-config->hash config))
     (check-equal? (hash-ref h 'id) "agent-123")
     (check-equal? (hash-ref h 'name) "TestAgent")
     (check-equal? (hash-ref h 'llm_provider) "anthropic"))))

;;; ============================================================================
;;; Memory Types Tests
;;; ============================================================================

(define memory-types-tests
  (test-suite
   "Memory Types Tests"

   (test-case "Create memory block"
     (define block (memory-block
                    "id-123"
                    "agent-456"
                    "persona"
                    "You are helpful."
                    #t
                    #f
                    1234567890
                    1234567890))
     (check-equal? (memory-block-id block) "id-123")
     (check-equal? (memory-block-label block) "persona")
     (check-true (memory-block-is-template block)))

   (test-case "Validate block labels"
     (check-true (valid-block-label? "persona"))
     (check-true (valid-block-label? "custom"))
     (check-false (valid-block-label? ""))
     (check-false (valid-block-label? (make-string 101 #\x))))

   (test-case "Standard block check"
     (check-true (standard-block? "persona"))
     (check-true (standard-block? "human"))
     (check-false (standard-block? "custom")))

   (test-case "Core memory"
     (define memory (make-default-core-memory))
     (check-equal? (core-memory-persona memory) default-persona-template)
     (check-equal? (core-memory-human memory) default-human-template)
     (check-true (hash? (core-memory-custom memory))))

   (test-case "Core memory get/set blocks"
     (define memory (make-default-core-memory))
     (check-equal? (core-memory-get-block memory "persona") default-persona-template)
     (check-equal? (core-memory-get-block memory "human") default-human-template)
     (check-false (core-memory-get-block memory "custom"))

     (core-memory-set-block! memory "persona" "New persona")
     (check-equal? (core-memory-get-block memory "persona") "New persona"))

   (test-case "Core memory has block"
     (define memory (make-default-core-memory))
     (check-true (core-memory-has-block? memory "persona"))
     (check-true (core-memory-has-block? memory "human"))
     (check-false (core-memory-has-block? memory "custom")))

   (test-case "Validate block params"
     (define valid-params (hash 'label "test" 'value "test value"))
     (define result (validate-block-params valid-params))
     (check-true (car result))

     (define invalid-params (hash 'label "test"))
     (define invalid-result (validate-block-params invalid-params))
     (check-false (car invalid-result)))

   (test-case "Block template"
     (check-equal? (get-block-template "persona") default-persona-template)
     (check-equal? (get-block-template "human") default-human-template)
     (check-equal? (get-block-template "unknown") ""))

   (test-case "Memory conversion"
     (define block (memory-block
                    "id-123"
                    "agent-456"
                    "persona"
                    "You are helpful."
                    #t
                    #f
                    1234567890
                    1234567890))
     (define h (memory-block->hash block))
     (check-equal? (hash-ref h 'id) "id-123")
     (check-equal? (hash-ref h 'label) "persona")

     (define block2 (hash->memory-block h))
     (check-equal? (memory-block-id block2) "id-123"))))

;;; ============================================================================
;;; Memory Blocks Tests
;;; ============================================================================

(define memory-blocks-tests
  (test-suite
   "Memory Blocks Tests"

   (test-case "Create block manager"
     (define manager (create-block-manager "agent-123"))
     (check-equal? (block-manager-agent-id manager) "agent-123")
     (check-true (block-manager-cache-enabled manager))
     (check-true (hash? (block-manager-cache manager))))

   (test-case "Create block manager without cache"
     (define manager (create-block-manager "agent-123" #f))
     (check-false (block-manager-cache-enabled manager)))))

;;; ============================================================================
;;; LLM Types Tests
;;; ============================================================================

(define llm-types-tests
  (test-suite
   "LLM Types Tests"

   (test-case "Create messages"
     (define user-msg (llm:make-user-message "Hello"))
     (check-equal? (llm:llm-message-role user-msg) 'user)
     (check-equal? (llm:llm-message-content user-msg) "Hello")

     (define assistant-msg (llm:make-assistant-message "Hi there!"))
     (check-equal? (llm:llm-message-role assistant-msg) 'assistant)

     (define system-msg (llm:make-system-message "You are helpful."))
     (check-equal? (llm:llm-message-role system-msg) 'system)

     (define tool-msg (llm:make-tool-message "Result" "call-123"))
     (check-equal? (llm:llm-message-role tool-msg) 'tool)
     (check-equal? (llm:llm-message-tool-call-id tool-msg) "call-123"))

   (test-case "Tool call structures"
     (define func-call-instance (llm:function-call "test_tool" "{\"arg\":\"value\"}"))
     (check-equal? (llm:function-call-name func-call-instance) "test_tool")
     (check-equal? (llm:function-call-arguments func-call-instance) "{\"arg\":\"value\"}")

     (define tool-call-inst (llm:make-tool-call-instance "call-123" "test_tool" "{\"arg\":\"value\"}"))
     (check-equal? (llm:tool-call-id tool-call-inst) "call-123")
     (check-equal? (llm:tool-call-type tool-call-inst) "function"))

   (test-case "LLM response"
     (define response (llm:llm-response
                       "resp-123"
                       "claude-3"
                       (llm:make-user-message "Hello")
                       'stop
                       (llm:usage-stats 10 20 30)
                       (current-seconds)
                       'anthropic))
     (check-equal? (llm:llm-response-id response) "resp-123")
     (check-equal? (llm:llm-response-finish-reason response) 'stop)
     (check-equal? (llm:usage-stats-total-tokens (llm:llm-response-usage response)) 30))

   (test-case "Message validation"
     (define valid-msg (llm:make-user-message "Hello"))
     (check-true (llm:valid-message? valid-msg))

     (define invalid-msg (llm:llm-message 'invalid "" #f #f #f))
     (check-false (llm:valid-message? invalid-msg)))

   (test-case "Tool call checks"
     (define msg-with-calls (llm:make-assistant-message "Hello" (list (llm:make-tool-call-instance "call-1" "tool" "{}"))))
     (check-true (llm:has-tool-calls? msg-with-calls))

     (define msg-without-calls (llm:make-assistant-message "Hello"))
     (check-false (llm:has-tool-calls? msg-without-calls)))

   (test-case "Finish reason conversion"
     (check-equal? (llm:finish-reason->symbol "stop") 'stop)
     (check-equal? (llm:finish-reason->symbol "tool_calls") 'tool-calls)
     (check-equal? (llm:finish-reason->symbol "length") 'length)
     (check-equal? (llm:finish-reason->symbol "unknown") 'unknown))))

;;; ============================================================================
;;; Tools Types Tests
;;; ============================================================================

(define tools-types-tests
  (test-suite
   "Tools Types Tests"

   (test-case "Tool result"
     (define success-result (make-success-result "Success!"))
     (check-true (tool-result-success success-result))
     (check-equal? (tool-result-value success-result) "Success!")
     (check-false (tool-result-error success-result))

     (define error-result (make-error-result "Error!"))
     (check-false (tool-result-success error-result))
     (check-equal? (tool-result-error error-result) "Error!"))

   (test-case "Validate tool name"
     (define valid-result (validate-tool-name "valid_tool"))
     (check-true (car valid-result))

     (define empty-result (validate-tool-name ""))
     (check-false (car empty-result))

     (define invalid-result (validate-tool-name "1tool"))
     (check-false (car invalid-result)))

   (test-case "Tool execution status predicates"
     (define pending-call (tool-execution "id-1" "tool" (hash) 0 "agent-1" 'pending #f #f))
     (check-true (tool-execution-pending? pending-call))
     (check-false (tool-execution-completed? pending-call))

     (define completed-call (tool-execution "id-2" "tool" (hash) 0 "agent-2" 'completed #f #f))
     (check-true (tool-execution-completed? completed-call))
     (check-false (tool-execution-pending? completed-call))) ))

;;; ============================================================================
;;; Message Types Tests
;;; ============================================================================

(define message-types-tests
  (test-suite
   "Message Types Tests"

   (test-case "Create messages"
     (define user-msg (msg:make-user-message "agent-123" "Hello"))
     (check-equal? (msg:message-role user-msg) msg:message-role-user)
     (check-equal? (msg:message-content user-msg) "Hello")

     (define assistant-msg (msg:make-assistant-message "agent-123" "Hi!"))
     (check-equal? (msg:message-role assistant-msg) msg:message-role-assistant)

     (define system-msg (msg:make-system-message "agent-123" "You are helpful."))
     (check-equal? (msg:message-role system-msg) msg:message-role-system)

     (define tool-msg (msg:make-tool-message "agent-123" "Result" "call-123"))
     (check-equal? (msg:message-role tool-msg) msg:message-role-tool)
     (check-equal? (msg:message-tool-call-id tool-msg) "call-123"))

   (test-case "Message validation"
     (define valid-msg (msg:make-user-message "agent-123" "Hello"))
     (check-true (msg:valid-message? valid-msg)))

   (test-case "Message to hash"
     (define msg (msg:make-user-message "agent-123" "Hello"))
     (define h (msg:message->hash msg))
     (check-equal? (hash-ref h 'role) "user")
     (check-equal? (hash-ref h 'content) "Hello"))

   (test-case "Filter messages by role"
     (define msg1 (msg:make-user-message "agent-1" "Hello"))
     (define msg2 (msg:make-assistant-message "agent-1" "Hi"))
     (define msg3 (msg:make-user-message "agent-1" "How are you?"))
     (define msgs (list msg1 msg2 msg3))

     (define user-msgs (msg:filter-messages-by-role msgs msg:message-role-user))
     (check-equal? (length user-msgs) 2)

     (define assistant-msgs (msg:filter-messages-by-role msgs msg:message-role-assistant))
     (check-equal? (length assistant-msgs) 1))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(define (run-all-tests)
  (run-tests
   (test-suite
    "Project O Tests"
    agent-types-tests
    memory-types-tests
    memory-blocks-tests
    llm-types-tests
    tools-types-tests
    message-types-tests)))

;;; Run tests if this file is executed directly
(module+ main
  (run-all-tests))
