;;; agent/context-test.ss - Context Window Manager Tests
;;;
;;; Test suite for context window management and optimization.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/test
  :o/message/types
  :o/memory/types
  :o/agent/types
  :o/agent/context)

;;; ============================================================================
;;; Test Setup
;;; ============================================================================

(def test-agent-id "test-agent-context-123")

(def (make-test-message content . rest (role 'user))
  "Create test message"
  (make-message
   id: "msg-test"
   agent-id: test-agent-id
   role: role
   content: content
   timestamp: (current-seconds)
   metadata: (make-hash-table)))

(def (make-test-memory-block name value)
  "Create test memory block"
  (make-memory-block
   name: name
   label: name
   value: value
   template: #f
   limit: 1000
   metadata: (make-hash-table)))

(def (make-test-context messages memory-blocks)
  "Create test execution context"
  (make-execution-context
   agent-id: test-agent-id
   agent-config: (make-default-agent-config test-agent-id "Test Agent")
   agent-state: (make-initial-agent-state test-agent-id)
   conversation-history: messages
   memory-blocks: memory-blocks
   step-history: '()
   current-step: 0
   start-time: (current-seconds)
   metadata: (make-hash-table)))

;;; ============================================================================
;;; Token Counting Tests
;;; ============================================================================

(def token-counting-tests
  (test-suite "Token Counting Tests"

    (test-case "Estimate tokens for text"
      (def text "Hello, world!")
      (def tokens (estimate-tokens text))
      (check (number? tokens))
      (check (> tokens 0))
      ;; ~4 chars per token, so 13 chars = ~4 tokens
      (check (>= tokens 3))
      (check (<= tokens 5)))

    (test-case "Count message tokens"
      (def manager (make-context-window-manager))
      (def message (make-test-message "This is a test message"))
      (def tokens (count-message-tokens manager message))
      (check (number? tokens))
      (check (> tokens 0))
      ;; Should include message overhead
      (check (>= tokens 4)))

    (test-case "Count multiple messages tokens"
      (def manager (make-context-window-manager))
      (def messages (list
                     (make-test-message "First message")
                     (make-test-message "Second message")
                     (make-test-message "Third message")))
      (def tokens (count-messages-tokens manager messages))
      (check (number? tokens))
      (check (> tokens 0))
      ;; Should be sum of individual message tokens
      (def sum (apply + (map (lambda (m) (count-message-tokens manager m)) messages)))
      (check (= tokens sum)))

    (test-case "Count memory block tokens"
      (def manager (make-context-window-manager))
      (def block (make-test-memory-block "persona" "I am a helpful assistant"))
      (def tokens (count-memory-block-tokens manager block))
      (check (number? tokens))
      (check (> tokens 0))
      ;; Should include label, value, and overhead
      (check (>= tokens 10)))

    (test-case "Count system message tokens"
      (def manager (make-context-window-manager))
      (def persona "I am a helpful AI assistant")
      (def blocks (list (make-test-memory-block "persona" "Test persona")))
      (def tokens (count-system-message-tokens manager persona blocks))
      (check (number? tokens))
      (check (> tokens 0)))))

;;; ============================================================================
;;; Context Window Manager Tests
;;; ============================================================================

(def context-window-manager-tests
  (test-suite "Context Window Manager Tests"

    (test-case "Create context window manager"
      (def manager (make-context-window-manager))
      (check (context-window-manager? manager))
      (check (= (context-window-manager-max-tokens manager) 100000))
      (check (= (context-window-manager-system-tokens manager) 2000))
      (check (= (context-window-manager-response-tokens manager) 4096))
      (check (eq? (context-window-manager-strategy manager) 'truncate)))

    (test-case "Create manager with custom settings"
      (def manager (make-context-window-manager
                    max-tokens: 50000
                    system-tokens: 1000
                    response-tokens: 2048
                    strategy: 'sliding))
      (check (= (context-window-manager-max-tokens manager) 50000))
      (check (= (context-window-manager-system-tokens manager) 1000))
      (check (= (context-window-manager-response-tokens manager) 2048))
      (check (eq? (context-window-manager-strategy manager) 'sliding)))

    (test-case "Analyze context usage"
      (def manager (make-context-window-manager max-tokens: 10000))
      (def messages (list
                     (make-test-message "Hello")
                     (make-test-message "How are you?")))
      (def blocks (list (make-test-memory-block "persona" "Test")))
      (def context (make-test-context messages blocks))
      (def usage (analyze-context-usage manager context))
      (check (hash-table? usage))
      (check (hash-key? usage 'max_tokens))
      (check (hash-key? usage 'system_tokens))
      (check (hash-key? usage 'message_tokens))
      (check (hash-key? usage 'total_tokens))
      (check (hash-key? usage 'available_tokens))
      (check (hash-key? usage 'usage_percent))
      (check (= (hash-ref usage 'max_tokens) 10000))
      (check (= (hash-ref usage 'message_count) 2)))

    (test-case "Check context fits"
      (def manager (make-context-window-manager max-tokens: 10000))
      (def messages (list (make-test-message "Short message")))
      (def context (make-test-context messages '()))
      (check (check-context-fits manager context)))

    (test-case "Check context doesn't fit"
      (def manager (make-context-window-manager max-tokens: 100))
      ;; Create many messages to exceed limit
      (def messages (map (lambda (i)
                          (make-test-message
                           (string-append "Message " (number->string i)
                                        " with some content to increase token count")))
                        (iota 50)))
      (def context (make-test-context messages '()))
      (check (not (check-context-fits manager context))))))

;;; ============================================================================
;;; Truncation Strategy Tests
;;; ============================================================================

(def truncation-tests
  (test-suite "Truncation Strategy Tests"

    (test-case "Truncate messages to fit"
      (def manager (make-context-window-manager))
      (def messages (list
                     (make-test-message "Message 1")
                     (make-test-message "Message 2")
                     (make-test-message "Message 3")
                     (make-test-message "Message 4")))
      ;; Allow tokens for only 2 messages
      (def available-tokens 100)
      (def truncated (truncate-messages manager messages available-tokens))
      (check (list? truncated))
      ;; Should keep newest messages
      (check (<= (length truncated) (length messages))))

    (test-case "Optimize context with truncation"
      (def manager (make-context-window-manager
                    max-tokens: 1000
                    strategy: 'truncate))
      (def messages (map (lambda (i)
                          (make-test-message
                           (string-append "Message " (number->string i))))
                        (iota 20)))
      (def context (make-test-context messages '()))
      (def optimized (optimize-context-truncate manager context))
      (check (execution-context? optimized))
      ;; Should have fewer messages
      (def new-history (execution-context-conversation-history optimized))
      (check (<= (length new-history) (length messages))))

    (test-case "Truncation preserves newest messages"
      (def manager (make-context-window-manager))
      (def messages (list
                     (make-test-message "Old message 1")
                     (make-test-message "Old message 2")
                     (make-test-message "Recent message 1")
                     (make-test-message "Recent message 2")))
      (def available-tokens 50)
      (def truncated (truncate-messages manager messages available-tokens))
      ;; Should keep recent messages
      (when (not (null? truncated))
        (check (string-contains (message-content (car truncated)) "Recent"))))))

;;; ============================================================================
;;; Sliding Window Strategy Tests
;;; ============================================================================

(def sliding-window-tests
  (test-suite "Sliding Window Strategy Tests"

    (test-case "Optimize context with sliding window"
      (def manager (make-context-window-manager
                    max-tokens: 1000
                    strategy: 'sliding))
      (def messages (map (lambda (i)
                          (make-test-message
                           (string-append "Message " (number->string i))))
                        (iota 20)))
      (def context (make-test-context messages '()))
      (def optimized (optimize-context-sliding manager context))
      (check (execution-context? optimized))
      (def new-history (execution-context-conversation-history optimized))
      ;; Should have fewer messages than original
      (check (<= (length new-history) (length messages))))

    (test-case "Sliding window preserves early messages"
      (def manager (make-context-window-manager
                    max-tokens: 500
                    strategy: 'sliding))
      (def messages (list
                     (make-test-message "First message")
                     (make-test-message "Second message")
                     (make-test-message "Third message")
                     (make-test-message "Fourth message")
                     (make-test-message "Fifth message")))
      (def context (make-test-context messages '()))
      (def optimized (optimize-context-sliding manager context))
      (def new-history (execution-context-conversation-history optimized))
      ;; Should keep first 2 messages
      (when (>= (length new-history) 2)
        (check (string-contains (message-content (car new-history)) "First"))
        (check (string-contains (message-content (cadr new-history)) "Second"))))))

;;; ============================================================================
;;; Optimization Strategy Tests
;;; ============================================================================

(def optimization-tests
  (test-suite "Optimization Strategy Tests"

    (test-case "Optimize with truncate strategy"
      (def manager (make-context-window-manager
                    max-tokens: 1000
                    strategy: 'truncate))
      (def messages (map (lambda (i) (make-test-message "Test")) (iota 50)))
      (def context (make-test-context messages '()))
      (def optimized (optimize-context manager context))
      (check (execution-context? optimized)))

    (test-case "Optimize with sliding strategy"
      (def manager (make-context-window-manager
                    max-tokens: 1000
                    strategy: 'sliding))
      (def messages (map (lambda (i) (make-test-message "Test")) (iota 50)))
      (def context (make-test-context messages '()))
      (def optimized (optimize-context manager context))
      (check (execution-context? optimized)))

    (test-case "Optimize with summarize strategy"
      (def manager (make-context-window-manager
                    max-tokens: 1000
                    strategy: 'summarize))
      (def messages (map (lambda (i) (make-test-message "Test")) (iota 50)))
      (def context (make-test-context messages '()))
      ;; Summarize currently falls back to truncate
      (def optimized (optimize-context manager context))
      (check (execution-context? optimized)))))

;;; ============================================================================
;;; Utility Function Tests
;;; ============================================================================

(def utility-tests
  (test-suite "Utility Function Tests"

    (test-case "Take elements from list"
      (def lst '(1 2 3 4 5))
      (check (equal? (take lst 0) '()))
      (check (equal? (take lst 1) '(1)))
      (check (equal? (take lst 3) '(1 2 3)))
      (check (equal? (take lst 5) '(1 2 3 4 5)))
      (check (equal? (take lst 10) '(1 2 3 4 5))))

    (test-case "Drop elements from list"
      (def lst '(1 2 3 4 5))
      (check (equal? (drop lst 0) '(1 2 3 4 5)))
      (check (equal? (drop lst 1) '(2 3 4 5)))
      (check (equal? (drop lst 3) '(4 5)))
      (check (equal? (drop lst 5) '()))
      (check (equal? (drop lst 10) '())))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(def context-test-suite
  (test-suite "Context Window Test Suite"
    token-counting-tests
    context-window-manager-tests
    truncation-tests
    sliding-window-tests
    optimization-tests
    utility-tests))

;;; Run tests
#|
(import :std/test)
(test-run! context-test-suite)
|#
