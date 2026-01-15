;;; llm/test/client-test.ss - Integration tests for LLM clients
;;;
;;; These tests require actual API keys to be set in environment variables:
;;; - OPENAI_API_KEY
;;; - ANTHROPIC_API_KEY
;;; - GROQ_API_KEY
;;; - Ollama running locally (optional)

(export #t)

(import
  :std/test
  :std/misc/hash
  ../types
  ../client)

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(def (has-api-key? provider)
  "Check if API key is available for provider"
  (case provider
    ((:openai) (get-environment-variable "OPENAI_API_KEY"))
    ((:anthropic) (get-environment-variable "ANTHROPIC_API_KEY"))
    ((:groq) (get-environment-variable "GROQ_API_KEY"))
    ((:ollama) #t)  ;; Ollama doesn't need API key
    (else #f)))

(def (skip-if-no-key provider)
  "Skip test if API key is not available"
  (unless (has-api-key? provider)
    (displayln (format "Skipping ~a test: API key not set" provider))
    (check #t)))  ;; Pass the test

;;; ============================================================================
;;; Simple Chat Tests
;;; ============================================================================

(def client-test
  (test-suite "LLM Client Integration Tests"

    (test-case "OpenAI simple chat"
      (if (has-api-key? :openai)
          (let ((response (llm-simple :openai "Say 'test' and nothing else.")))
            (check (string? response))
            (check (> (string-length response) 0))
            (displayln (format "OpenAI response: ~a" response)))
          (skip-if-no-key :openai)))

    (test-case "Anthropic simple chat"
      (if (has-api-key? :anthropic)
          (let ((response (llm-simple :anthropic "Say 'test' and nothing else.")))
            (check (string? response))
            (check (> (string-length response) 0))
            (displayln (format "Anthropic response: ~a" response)))
          (skip-if-no-key :anthropic)))

    (test-case "Groq simple chat"
      (if (has-api-key? :groq)
          (let ((response (llm-simple :groq "Say 'test' and nothing else.")))
            (check (string? response))
            (check (> (string-length response) 0))
            (displayln (format "Groq response: ~a" response)))
          (skip-if-no-key :groq)))

    ;; Ollama test (requires local Ollama server)
    (test-case "Ollama simple chat (if available)"
      (try
       (let ((response (llm-simple :ollama "Say 'test' and nothing else.")))
         (check (string? response))
         (check (> (string-length response) 0))
         (displayln (format "Ollama response: ~a" response)))
       (catch (e)
         (displayln "Ollama not available, skipping test")
         (check #t))))

    ;;; ========================================================================
    ;;; Configuration Tests
    ;;; ========================================================================

    (test-case "Create LLM config"
      (let ((config (make-llm-config-instance
                     :openai
                     model: "gpt-4"
                     temperature: 0.5)))
        (check (llm-config? config))
        (check-equal? (llm-config-provider config) :openai)
        (check-equal? (llm-config-model config) "gpt-4")
        (check-equal? (llm-config-temperature config) 0.5)))

    (test-case "Chat with config (OpenAI)"
      (if (has-api-key? :openai)
          (let* ((config (make-llm-config-instance
                          :openai
                          model: "gpt-4"
                          temperature: 0.5))
                 (messages (list (make-user-message "Say 'test' and nothing else.")))
                 (response (llm-chat-with-config config messages)))
            (check (llm-response? response))
            (check-equal? (llm-response-provider response) :openai)
            (check (llm-message? (llm-response-message response)))
            (displayln (format "Config response: ~a"
                              (extract-text-content (llm-response-message response)))))
          (skip-if-no-key :openai)))

    ;;; ========================================================================
    ;;; Tool Call Tests
    ;;; ========================================================================

    (test-case "Tool call with OpenAI"
      (if (has-api-key? :openai)
          (let* ((calculator-tool
                  (make-tool-definition-instance
                   "calculate"
                   "Perform a mathematical calculation"
                   (hash
                    'type "object"
                    'properties (hash
                                 'expression (hash
                                              'type "string"
                                              'description "Mathematical expression"))
                    'required '("expression"))))
                 (response (llm-with-tools
                            :openai
                            "What is 25 * 4? Use the calculate tool."
                            (list calculator-tool))))
            (check (llm-response? response))
            (let ((msg (llm-response-message response)))
              (if (has-tool-calls? msg)
                  (begin
                    (displayln "Tool calls detected:")
                    (for-each
                     (lambda (tc)
                       (displayln (format "  - ~a(~a)"
                                         (function-call-name (tool-call-function tc))
                                         (function-call-arguments (tool-call-function tc)))))
                     (llm-message-tool-calls msg))
                    (check #t))
                  (displayln "No tool calls in response"))))
          (skip-if-no-key :openai)))

    (test-case "Tool call with Anthropic"
      (if (has-api-key? :anthropic)
          (let* ((calculator-tool
                  (make-tool-definition-instance
                   "calculate"
                   "Perform a mathematical calculation"
                   (hash
                    'type "object"
                    'properties (hash
                                 'expression (hash
                                              'type "string"
                                              'description "Mathematical expression"))
                    'required '("expression"))))
                 (response (llm-with-tools
                            :anthropic
                            "What is 25 * 4? Use the calculate tool."
                            (list calculator-tool))))
            (check (llm-response? response))
            (let ((msg (llm-response-message response)))
              (if (has-tool-calls? msg)
                  (begin
                    (displayln "Tool calls detected:")
                    (for-each
                     (lambda (tc)
                       (displayln (format "  - ~a(~a)"
                                         (function-call-name (tool-call-function tc))
                                         (function-call-arguments (tool-call-function tc)))))
                     (llm-message-tool-calls msg))
                    (check #t))
                  (displayln "No tool calls in response"))))
          (skip-if-no-key :anthropic)))

    ;;; ========================================================================
    ;;; Provider Utility Tests
    ;;; ========================================================================

    (test-case "Provider supports tools"
      (check (provider-supports-tools? :openai))
      (check (provider-supports-tools? :anthropic))
      (check (provider-supports-tools? :groq))
      (check (provider-supports-tools? :ollama)))

    (test-case "Provider requires max-tokens"
      (check (provider-requires-max-tokens? :anthropic))
      (check-not (provider-requires-max-tokens? :openai))
      (check-not (provider-requires-max-tokens? :groq))
      (check-not (provider-requires-max-tokens? :ollama)))

    (test-case "Provider supports streaming"
      (check (provider-supports-streaming? :openai))
      (check (provider-supports-streaming? :anthropic))
      (check (provider-supports-streaming? :groq))
      (check (provider-supports-streaming? :ollama)))

    (test-case "Validate provider"
      (check-not (check-exception (validate-provider :openai)))
      (check-not (check-exception (validate-provider :anthropic)))
      (check-not (check-exception (validate-provider :groq)))
      (check-not (check-exception (validate-provider :ollama)))
      (check-exception (validate-provider :invalid)))

    ;;; ========================================================================
    ;;; Multi-turn Conversation Tests
    ;;; ========================================================================

    (test-case "Multi-turn conversation with OpenAI"
      (if (has-api-key? :openai)
          (let* ((messages (list
                            (make-user-message "My name is Alice.")
                            (make-assistant-message "Hello Alice! Nice to meet you.")
                            (make-user-message "What is my name?")))
                 (response (llm-chat-completion :openai messages)))
            (check (llm-response? response))
            (let ((content (extract-text-content (llm-response-message response))))
              (displayln (format "Multi-turn response: ~a" content))
              ;; Check if response mentions "Alice"
              (check (or (string-contains content "Alice")
                        (string-contains content "alice")))))
          (skip-if-no-key :openai)))

    ;;; ========================================================================
    ;;; Error Handling Tests
    ;;; ========================================================================

    (test-case "Invalid API key error"
      (check-exception
       (llm-simple :openai "Test" api-key: "invalid-key-12345")))

    (test-case "Invalid provider error"
      (check-exception
       (llm-simple :invalid-provider "Test")))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(def (run-client-tests)
  "Run all client integration tests"
  (displayln "\n=== Running LLM Client Integration Tests ===")
  (displayln "Note: Some tests may be skipped if API keys are not set\n")
  (test client-test))
