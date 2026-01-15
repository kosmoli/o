;;; llm/test/types-test.ss - Tests for LLM type definitions
;;;
;;; Unit tests for the unified type system

(export #t)

(import
  :std/test
  :std/misc/hash
  ../types)

;;; ============================================================================
;;; Message Creation Tests
;;; ============================================================================

(def types-test
  (test-suite "LLM Types Tests"

    (test-case "Create user message"
      (let ((msg (make-user-message "Hello, world!")))
        (check (llm-message? msg))
        (check-equal? (llm-message-role msg) :user)
        (check-equal? (llm-message-content msg) "Hello, world!")
        (check-equal? (llm-message-tool-calls msg) #f)))

    (test-case "Create assistant message"
      (let ((msg (make-assistant-message "Hi there!")))
        (check (llm-message? msg))
        (check-equal? (llm-message-role msg) :assistant)
        (check-equal? (llm-message-content msg) "Hi there!")
        (check-equal? (llm-message-tool-calls msg) #f)))

    (test-case "Create system message"
      (let ((msg (make-system-message "You are a helpful assistant.")))
        (check (llm-message? msg))
        (check-equal? (llm-message-role msg) :system)
        (check-equal? (llm-message-content msg) "You are a helpful assistant.")))

    (test-case "Create tool message"
      (let ((msg (make-tool-message "Result: 42" "call-123")))
        (check (llm-message? msg))
        (check-equal? (llm-message-role msg) :tool)
        (check-equal? (llm-message-content msg) "Result: 42")
        (check-equal? (llm-message-tool-call-id msg) "call-123")))

    (test-case "Create assistant message with tool calls"
      (let* ((tc (make-tool-call-instance "call-1" "calculate" "{\"x\": 5}"))
             (msg (make-assistant-message "Let me calculate that." (list tc))))
        (check (llm-message? msg))
        (check-equal? (llm-message-role msg) :assistant)
        (check (has-tool-calls? msg))
        (check-equal? (length (llm-message-tool-calls msg)) 1)))

    ;;; ========================================================================
    ;;; Tool Call Tests
    ;;; ========================================================================

    (test-case "Create tool call"
      (let ((tc (make-tool-call-instance "call-123" "get_weather" "{\"city\": \"SF\"}")))
        (check (tool-call? tc))
        (check-equal? (tool-call-id tc) "call-123")
        (check-equal? (tool-call-type tc) "function")
        (check (function-call? (tool-call-function tc)))
        (check-equal? (function-call-name (tool-call-function tc)) "get_weather")
        (check-equal? (function-call-arguments (tool-call-function tc)) "{\"city\": \"SF\"}")))

    (test-case "Create tool definition"
      (let ((td (make-tool-definition-instance
                 "calculate"
                 "Perform calculation"
                 (hash 'type "object"
                       'properties (hash 'x (hash 'type "number"))))))
        (check (tool-definition? td))
        (check-equal? (tool-definition-type td) "function")
        (check (function-definition? (tool-definition-function td)))
        (check-equal? (function-definition-name (tool-definition-function td)) "calculate")
        (check-equal? (function-definition-description (tool-definition-function td)) "Perform calculation")))

    ;;; ========================================================================
    ;;; Response Tests
    ;;; ========================================================================

    (test-case "Create llm-response"
      (let* ((msg (make-assistant-message "Hello!"))
             (usage (make-usage-stats prompt-tokens: 10 completion-tokens: 5 total-tokens: 15))
             (response (make-llm-response
                        id: "resp-123"
                        model: "gpt-4"
                        message: msg
                        finish-reason: :stop
                        usage: usage
                        created: 1234567890
                        provider: :openai)))
        (check (llm-response? response))
        (check-equal? (llm-response-id response) "resp-123")
        (check-equal? (llm-response-model response) "gpt-4")
        (check-equal? (llm-response-finish-reason response) :stop)
        (check-equal? (llm-response-provider response) :openai)
        (check-equal? (usage-stats-prompt-tokens (llm-response-usage response)) 10)
        (check-equal? (usage-stats-completion-tokens (llm-response-usage response)) 5)
        (check-equal? (usage-stats-total-tokens (llm-response-usage response)) 15)))

    ;;; ========================================================================
    ;;; Conversion Tests
    ;;; ========================================================================

    (test-case "Convert message to hash"
      (let* ((msg (make-user-message "Hello"))
             (h (message->hash msg)))
        (check (hash? h))
        (check-equal? (hash-ref h 'role) "user")
        (check-equal? (hash-ref h 'content) "Hello")))

    (test-case "Convert hash to message"
      (let* ((h (hash 'role "assistant" 'content "Hi there!"))
             (msg (hash->message h)))
        (check (llm-message? msg))
        (check-equal? (llm-message-role msg) :assistant)
        (check-equal? (llm-message-content msg) "Hi there!")))

    (test-case "Convert tool call to hash"
      (let* ((tc (make-tool-call-instance "call-1" "func" "{\"a\": 1}"))
             (h (tool-call->hash tc)))
        (check (hash? h))
        (check-equal? (hash-ref h 'id) "call-1")
        (check-equal? (hash-ref h 'type) "function")
        (check (hash? (hash-ref h 'function)))
        (check-equal? (hash-ref (hash-ref h 'function) 'name) "func")))

    (test-case "Convert tool definition to hash"
      (let* ((td (make-tool-definition-instance
                  "calc"
                  "Calculate"
                  (hash 'type "object")))
             (h (tool-definition->hash td)))
        (check (hash? h))
        (check-equal? (hash-ref h 'type) "function")
        (check (hash? (hash-ref h 'function)))
        (check-equal? (hash-ref (hash-ref h 'function) 'name) "calc")))

    ;;; ========================================================================
    ;;; Validation Tests
    ;;; ========================================================================

    (test-case "Validate message"
      (let ((msg (make-user-message "Test")))
        (check (valid-message? msg))))

    (test-case "Validate tool definition"
      (let ((td (make-tool-definition-instance
                 "test"
                 "Test function"
                 (hash 'type "object"))))
        (check (valid-tool-definition? td))))

    ;;; ========================================================================
    ;;; Utility Tests
    ;;; ========================================================================

    (test-case "Extract text content from string"
      (let ((msg (make-assistant-message "Hello, world!")))
        (check-equal? (extract-text-content msg) "Hello, world!")))

    (test-case "Check has-tool-calls?"
      (let* ((tc (make-tool-call-instance "call-1" "func" "{}"))
             (msg-with-tools (make-assistant-message "Calling tool" (list tc)))
             (msg-without-tools (make-assistant-message "No tools")))
        (check (has-tool-calls? msg-with-tools))
        (check-not (has-tool-calls? msg-without-tools))))

    (test-case "Convert finish reason to symbol"
      (check-equal? (finish-reason->symbol "stop") :stop)
      (check-equal? (finish-reason->symbol "tool_calls") :tool-calls)
      (check-equal? (finish-reason->symbol "length") :length)
      (check-equal? (finish-reason->symbol "content_filter") :content-filter)
      (check-equal? (finish-reason->symbol "unknown") :unknown))

    ;;; ========================================================================
    ;;; Error Type Tests
    ;;; ========================================================================

    (test-case "Create llm-error"
      (let ((err (make-llm-error
                  type: :api-error
                  message: "Request failed"
                  status: 500
                  provider: :openai
                  details: "Internal server error")))
        (check (llm-error? err))
        (check-equal? (llm-error-type err) :api-error)
        (check-equal? (llm-error-message err) "Request failed")
        (check-equal? (llm-error-status err) 500)
        (check-equal? (llm-error-provider err) :openai)))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(def (run-types-tests)
  "Run all types tests"
  (test types-test))
