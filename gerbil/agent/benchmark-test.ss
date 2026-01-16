;;; agent/benchmark-test.ss - Benchmark Tests
;;;
;;; Test suite for performance benchmarking system.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/test
  :gerbil/llm/types
  :gerbil/tools/types
  :gerbil/tools/core
  :gerbil/message/types
  :gerbil/memory/types
  ./types
  ./executor
  ./benchmark)

;;; ============================================================================
;;; Test Setup
;;; ============================================================================

(def test-agent-id "test-agent-benchmark-123")

(def (make-mock-llm-client)
  "Create mock LLM client for testing"
  (lambda (method . args)
    (case method
      ((:chat)
       (make-llm-response
        content: "Test response"
        tool-calls: '()
        usage: (hash 'prompt_tokens 10 'completion_tokens 20 'total_tokens 30)
        finish-reason: "stop"
        metadata: (hash))))))

(def (make-mock-tool-dispatcher)
  "Create mock tool dispatcher for testing"
  (let ((dispatcher (make-tool-dispatcher)))
    (dispatcher-register-tool! dispatcher
                              (make-tool-definition
                               name: "test_tool"
                               description: "Test tool"
                               parameters: (hash 'input (hash 'type :string 'required #t))
                               handler: (lambda (args ctx)
                                         (make-success-result
                                          (hash 'output (hash-ref args 'input))))
                               category: :custom
                               requires-approval: #f
                               metadata: (hash)))
    dispatcher))

(def (make-mock-message-manager)
  "Create mock message manager for testing"
  (lambda (method . args)
    (case method
      ((:create-message)
       (make-message
        id: "msg-123"
        agent-id: test-agent-id
        role: :user
        content: "Test message"
        timestamp: (current-seconds)
        metadata: (hash))))))

(def (make-mock-memory-manager)
  "Create mock memory manager for testing"
  (let ((blocks (hash)))
    (lambda (method . args)
      (case method
        ((:get-block)
         (let ((agent-id (car args))
               (block-name (cadr args)))
           (hash-ref blocks block-name
                    (make-memory-block
                     name: block-name
                     label: block-name
                     value: "Test memory"
                     template: #f
                     limit: 1000
                     metadata: (hash)))))))))

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
   metadata: (hash)))

;;; ============================================================================
;;; Benchmark Configuration Tests
;;; ============================================================================

(def benchmark-config-tests
  (test-suite "Benchmark Configuration Tests"

    (test-case "Create default benchmark config"
      (def config (make-default-benchmark-config "test-benchmark"))
      (check (benchmark-config? config))
      (check (equal? (benchmark-config-name config) "test-benchmark"))
      (check (= (benchmark-config-iterations config) 10))
      (check (= (benchmark-config-warmup-iterations config) 2))
      (check (= (benchmark-config-timeout config) 60))
      (check (not (benchmark-config-collect-memory config))))

    (test-case "Create custom benchmark config"
      (def config (make-benchmark-config
                   name: "custom"
                   iterations: 100
                   warmup-iterations: 10
                   timeout: 120
                   collect-memory: #t
                   metadata: (hash 'test "data")))
      (check (benchmark-config? config))
      (check (= (benchmark-config-iterations config) 100))
      (check (= (benchmark-config-warmup-iterations config) 10))
      (check (benchmark-config-collect-memory config)))))

;;; ============================================================================
;;; Timing Tests
;;; ============================================================================

(def timing-tests
  (test-suite "Timing Tests"

    (test-case "Measure time of simple operation"
      (def time (measure-time (lambda () (+ 1 1))))
      (check (number? time))
      (check (>= time 0)))

    (test-case "Measure time with result"
      (def-values (result time)
        (measure-time-with-result (lambda () (+ 2 3))))
      (check (= result 5))
      (check (number? time))
      (check (>= time 0)))))

;;; ============================================================================
;;; Statistics Tests
;;; ============================================================================

(def statistics-tests
  (test-suite "Statistics Tests"

    (test-case "Calculate mean"
      (check (= (calculate-mean '(1 2 3 4 5)) 3))
      (check (= (calculate-mean '(10 20 30)) 20))
      (check (= (calculate-mean '()) 0.0)))

    (test-case "Calculate min"
      (check (= (calculate-min '(5 2 8 1 9)) 1))
      (check (= (calculate-min '(10)) 10))
      (check (= (calculate-min '()) 0.0)))

    (test-case "Calculate max"
      (check (= (calculate-max '(5 2 8 1 9)) 9))
      (check (= (calculate-max '(10)) 10))
      (check (= (calculate-max '()) 0.0)))

    (test-case "Calculate standard deviation"
      (def values '(2 4 4 4 5 5 7 9))
      (def mean (calculate-mean values))
      (def stddev (calculate-stddev values mean))
      (check (number? stddev))
      (check (> stddev 0)))))

;;; ============================================================================
;;; Benchmark Execution Tests
;;; ============================================================================

(def benchmark-execution-tests
  (test-suite "Benchmark Execution Tests"

    (test-case "Run simple benchmark"
      (def config (make-benchmark-config
                   name: "simple-test"
                   iterations: 5
                   warmup-iterations: 1
                   timeout: 60
                   collect-memory: #f
                   metadata: (hash)))
      (def result (run-benchmark config (lambda () (+ 1 1))))
      (check (benchmark-result? result))
      (check (equal? (benchmark-result-name result) "simple-test"))
      (check (= (benchmark-result-iterations result) 5))
      (check (> (benchmark-result-total-time result) 0))
      (check (> (benchmark-result-mean-time result) 0))
      (check (> (benchmark-result-throughput result) 0)))

    (test-case "Benchmark result statistics"
      (def config (make-benchmark-config
                   name: "stats-test"
                   iterations: 10
                   warmup-iterations: 2
                   timeout: 60
                   collect-memory: #f
                   metadata: (hash)))
      (def result (run-benchmark config (lambda () (+ 1 1))))
      (check (>= (benchmark-result-min-time result) 0))
      (check (>= (benchmark-result-max-time result)
                (benchmark-result-min-time result)))
      (check (>= (benchmark-result-mean-time result)
                (benchmark-result-min-time result)))
      (check (<= (benchmark-result-mean-time result)
                (benchmark-result-max-time result))))))

;;; ============================================================================
;;; Format Tests
;;; ============================================================================

(def format-tests
  (test-suite "Format Tests"

    (test-case "Format microseconds"
      (def formatted (format-time 0.0001))
      (check (string? formatted))
      (check (string-contains formatted "us")))

    (test-case "Format milliseconds"
      (def formatted (format-time 0.1))
      (check (string? formatted))
      (check (string-contains formatted "ms")))

    (test-case "Format seconds"
      (def formatted (format-time 1.5))
      (check (string? formatted))
      (check (string-contains formatted "s")))))

;;; ============================================================================
;;; Conversion Tests
;;; ============================================================================

(def conversion-tests
  (test-suite "Conversion Tests"

    (test-case "Convert benchmark result to hash"
      (def result (make-benchmark-result
                   name: "test"
                   iterations: 10
                   total-time: 1.0
                   mean-time: 0.1
                   min-time: 0.05
                   max-time: 0.15
                   stddev-time: 0.02
                   throughput: 10.0
                   memory-used: #f
                   metadata: (hash)))
      (def hash-result (benchmark-result->hash result))
      (check (hash? hash-result))
      (check (equal? (hash-ref hash-result 'name) "test"))
      (check (= (hash-ref hash-result 'iterations) 10))
      (check (= (hash-ref hash-result 'total_time) 1.0)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(def benchmark-test-suite
  (test-suite "Benchmark Test Suite"
    benchmark-config-tests
    timing-tests
    statistics-tests
    benchmark-execution-tests
    format-tests
    conversion-tests))

;;; Run tests
#|
(import :std/test)
(test-run! benchmark-test-suite)
|#
