;;; tools/sandbox-test.ss - Tool Execution Sandbox Tests
;;;
;;; Test suite for sandbox implementation.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/test
  :o/tools/types
  :o/tools/core
  :o/tools/memory
  :o/tools/sandbox)

;;; ============================================================================
;;; Test Setup
;;; ============================================================================

(def test-agent-id "test-agent-sandbox-123")

(def (setup-test-dispatcher)
  "Create test dispatcher with all tools registered"
  (let ((dispatcher (make-tool-dispatcher)))
    (register-core-tools! dispatcher)
    (register-memory-tools! dispatcher)
    dispatcher))

;;; ============================================================================
;;; Sandbox Configuration Tests
;;; ============================================================================

(def sandbox-config-tests
  (test-suite "Sandbox Configuration Tests"

    (test-case "Create default sandbox config"
      (def config (make-default-sandbox-config))
      (check (sandbox-config? config))
      (check (= (sandbox-config-max-execution-time config) 30))
      (check (= (sandbox-config-max-memory-mb config) 100))
      (check (not (sandbox-config-allow-network config)))
      (check (not (sandbox-config-allow-file-read config)))
      (check (not (sandbox-config-allow-file-write config))))

    (test-case "Create strict sandbox config"
      (def config (make-strict-sandbox-config))
      (check (sandbox-config? config))
      (check (= (sandbox-config-max-execution-time config) 10))
      (check (= (sandbox-config-max-memory-mb config) 50))
      (check (member "send_message" (sandbox-config-allowed-tools config)))
      (check (member "conversation_search" (sandbox-config-allowed-tools config))))

    (test-case "Custom sandbox config"
      (def config (make-sandbox-config
                   max-execution-time: 60
                   max-memory-mb: 200
                   allow-network: #t
                   allow-file-read: #t
                   allow-file-write: #f
                   allowed-tools: '("send_message")
                   blocked-tools: '("archival_memory_insert")))
      (check (= (sandbox-config-max-execution-time config) 60))
      (check (sandbox-config-allow-network config))
      (check (member "archival_memory_insert" (sandbox-config-blocked-tools config))))))

;;; ============================================================================
;;; Sandbox Validation Tests
;;; ============================================================================

(def sandbox-validation-tests
  (test-suite "Sandbox Validation Tests"

    (test-case "Validate tool allowed - default config"
      (def config (make-default-sandbox-config))
      (check (validate-tool-allowed config "send_message"))
      (check (validate-tool-allowed config "archival_memory_insert")))

    (test-case "Validate tool allowed - strict config"
      (def config (make-strict-sandbox-config))
      (check (validate-tool-allowed config "send_message"))
      (check (validate-tool-allowed config "conversation_search"))
      (check (not (validate-tool-allowed config "archival_memory_insert"))))

    (test-case "Validate tool blocked"
      (def config (make-sandbox-config
                   max-execution-time: 30
                   max-memory-mb: 100
                   allow-network: #f
                   allow-file-read: #f
                   allow-file-write: #f
                   allowed-tools: '()
                   blocked-tools: '("dangerous_tool")))
      (check (not (validate-tool-allowed config "dangerous_tool")))
      (check (validate-tool-allowed config "send_message")))

    (test-case "Validate execution time"
      (def execution (make-sandbox-execution
                      tool-name: "test"
                      start-time: (current-seconds)
                      timeout-time: (+ (current-seconds) 10)
                      config: (make-default-sandbox-config)
                      status: 'running
                      result: #f
                      error: #f))
      (check (validate-execution-time execution)))))

;;; ============================================================================
;;; Sandboxed Execution Tests
;;; ============================================================================

(def sandboxed-execution-tests
  (test-suite "Sandboxed Execution Tests"

    (test-case "Execute allowed tool in sandbox"
      (def dispatcher (setup-test-dispatcher))
      (def config (make-default-sandbox-config))
      (def execution (sandbox-execute-tool dispatcher
                                          "send_message"
                                          (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test message")
  ht)
                                          test-agent-id
                                          config: config))
      (check (sandbox-execution? execution))
      (check (sandbox-execution-succeeded? execution))
      (check (tool-call? (sandbox-execution-result execution))))

    (test-case "Execute blocked tool in sandbox"
      (def dispatcher (setup-test-dispatcher))
      (def config (make-strict-sandbox-config))
      (def execution (sandbox-execute-tool dispatcher
                                          "archival_memory_insert"
                                          (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Test")
  ht)
                                          test-agent-id
                                          config: config))
      (check (sandbox-execution-failed? execution))
      (check (string-contains (sandbox-execution-error execution) "not allowed")))

    (test-case "Execute tool with error"
      (def dispatcher (setup-test-dispatcher))
      (def config (make-default-sandbox-config))
      (def execution (sandbox-execute-tool dispatcher
                                          "send_message"
                                          (make-hash-table)  ; Missing required 'message'
                                          test-agent-id
                                          config: config))
      (check (sandbox-execution? execution)))

    (test-case "Sandbox execution status checks"
      (def dispatcher (setup-test-dispatcher))
      (def config (make-default-sandbox-config))
      (def execution (sandbox-execute-tool dispatcher
                                          "send_message"
                                          (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                                          test-agent-id
                                          config: config))
      (check (sandbox-execution-succeeded? execution))
      (check (not (sandbox-execution-timed-out? execution)))
      (check (not (sandbox-execution-failed? execution))))))

;;; ============================================================================
;;; Sandboxed Dispatcher Tests
;;; ============================================================================

(def sandboxed-dispatcher-tests
  (test-suite "Sandboxed Dispatcher Tests"

    (test-case "Create sandboxed dispatcher"
      (def dispatcher (setup-test-dispatcher))
      (def sandboxed (make-sandboxed-dispatcher dispatcher))
      (check (sandboxed-dispatcher? sandboxed))
      (check (sandbox-config? (sandboxed-dispatcher-config sandboxed))))

    (test-case "Create sandboxed dispatcher with custom config"
      (def dispatcher (setup-test-dispatcher))
      (def config (make-strict-sandbox-config))
      (def sandboxed (make-sandboxed-dispatcher dispatcher config: config))
      (check (eq? (sandboxed-dispatcher-config sandboxed) config)))

    (test-case "Call tool through sandboxed dispatcher"
      (def dispatcher (setup-test-dispatcher))
      (def sandboxed (make-sandboxed-dispatcher dispatcher))
      (def execution (sandboxed-dispatcher-call-tool sandboxed
                                                    "send_message"
                                                    (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                                                    test-agent-id))
      (check (sandbox-execution-succeeded? execution)))

    (test-case "Sandboxed dispatcher tracks execution history"
      (def dispatcher (setup-test-dispatcher))
      (def sandboxed (make-sandboxed-dispatcher dispatcher))

      ;; Execute multiple tools
      (sandboxed-dispatcher-call-tool sandboxed
                                     "send_message"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test 1")
  ht)
                                     test-agent-id)
      (sandboxed-dispatcher-call-tool sandboxed
                                     "send_message"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test 2")
  ht)
                                     test-agent-id)

      ;; Check history
      (def history (sandboxed-dispatcher-get-history sandboxed limit: 10))
      (check (>= (length history) 2)))

    (test-case "Sandboxed dispatcher execution statistics"
      (def dispatcher (setup-test-dispatcher))
      (def sandboxed (make-sandboxed-dispatcher dispatcher))

      ;; Execute some tools
      (sandboxed-dispatcher-call-tool sandboxed
                                     "send_message"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test 1")
  ht)
                                     test-agent-id)
      (sandboxed-dispatcher-call-tool sandboxed
                                     "send_message"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test 2")
  ht)
                                     test-agent-id)

      ;; Get statistics
      (def stats (sandboxed-dispatcher-get-stats sandboxed))
      (check (hash-key? stats 'total))
      (check (hash-key? stats 'completed))
      (check (hash-key? stats 'success_rate))
      (check (>= (hash-ref stats 'total) 2)))))

;;; ============================================================================
;;; Sandbox Utilities Tests
;;; ============================================================================

(def sandbox-utilities-tests
  (test-suite "Sandbox Utilities Tests"

    (test-case "Sandbox execution duration"
      (def execution (make-sandbox-execution
                      tool-name: "test"
                      start-time: (- (current-seconds) 5)
                      timeout-time: (+ (current-seconds) 10)
                      config: (make-default-sandbox-config)
                      status: 'completed
                      result: #f
                      error: #f))
      (def duration (sandbox-execution-duration execution))
      (check (>= duration 5)))

    (test-case "Sandbox execution to hash"
      (def dispatcher (setup-test-dispatcher))
      (def config (make-default-sandbox-config))
      (def execution (sandbox-execute-tool dispatcher
                                          "send_message"
                                          (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                                          test-agent-id
                                          config: config))
      (def hash-repr (sandbox-execution->hash execution))
      (check (hash-table? hash-repr))
      (check (hash-key? hash-repr 'tool_name))
      (check (hash-key? hash-repr 'status))
      (check (hash-key? hash-repr 'duration)))

    (test-case "Sandbox execution status predicates"
      (def succeeded (make-sandbox-execution
                      tool-name: "test"
                      start-time: (current-seconds)
                      timeout-time: (+ (current-seconds) 10)
                      config: (make-default-sandbox-config)
                      status: 'completed
                      result: #f
                      error: #f))
      (check (sandbox-execution-succeeded? succeeded))
      (check (not (sandbox-execution-timed-out? succeeded)))
      (check (not (sandbox-execution-failed? succeeded)))

      (def timed-out (make-sandbox-execution
                      tool-name: "test"
                      start-time: (current-seconds)
                      timeout-time: (+ (current-seconds) 10)
                      config: (make-default-sandbox-config)
                      status: 'timeout
                      result: #f
                      error: "Timeout"))
      (check (not (sandbox-execution-succeeded? timed-out)))
      (check (sandbox-execution-timed-out? timed-out))
      (check (not (sandbox-execution-failed? timed-out)))

      (def failed (make-sandbox-execution
                   tool-name: "test"
                   start-time: (current-seconds)
                   timeout-time: (+ (current-seconds) 10)
                   config: (make-default-sandbox-config)
                   status: 'error
                   result: #f
                   error: "Error"))
      (check (not (sandbox-execution-succeeded? failed)))
      (check (not (sandbox-execution-timed-out? failed)))
      (check (sandbox-execution-failed? failed)))))

;;; ============================================================================
;;; Sandbox Integration Tests
;;; ============================================================================

(def sandbox-integration-tests
  (test-suite "Sandbox Integration Tests"

    (test-case "Sandbox with core tools"
      (def dispatcher (setup-test-dispatcher))
      (def sandboxed (make-sandboxed-dispatcher dispatcher))

      ;; Execute send_message
      (def msg-execution (sandboxed-dispatcher-call-tool sandboxed
                                                        "send_message"
                                                        (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Hello!")
  ht)
                                                        test-agent-id))
      (check (sandbox-execution-succeeded? msg-execution))

      ;; Execute conversation_search
      (def search-execution (sandboxed-dispatcher-call-tool sandboxed
                                                           "conversation_search"
                                                           (let ((ht (make-hash-table)))
  (hash-put! ht 'query "test")
  ht)
                                                           test-agent-id))
      (check (sandbox-execution-succeeded? search-execution)))

    (test-case "Sandbox with memory tools"
      (def dispatcher (setup-test-dispatcher))
      (def sandboxed (make-sandboxed-dispatcher dispatcher))

      ;; Execute core_memory_append
      (def append-execution (sandboxed-dispatcher-call-tool sandboxed
                                                           "core_memory_append"
                                                           (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'content "Test")
  ht)
                                                           test-agent-id))
      (check (sandbox-execution-succeeded? append-execution))

      ;; Execute archival_memory_insert
      (def insert-execution (sandboxed-dispatcher-call-tool sandboxed
                                                           "archival_memory_insert"
                                                           (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Test memory")
  ht)
                                                           test-agent-id))
      (check (sandbox-execution-succeeded? insert-execution)))

    (test-case "Strict sandbox blocks memory tools"
      (def dispatcher (setup-test-dispatcher))
      (def config (make-strict-sandbox-config))
      (def sandboxed (make-sandboxed-dispatcher dispatcher config: config))

      ;; Try to execute blocked tool
      (def execution (sandboxed-dispatcher-call-tool sandboxed
                                                    "archival_memory_insert"
                                                    (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Test")
  ht)
                                                    test-agent-id))
      (check (sandbox-execution-failed? execution)))

    (test-case "Sandbox statistics tracking"
      (def dispatcher (setup-test-dispatcher))
      (def sandboxed (make-sandboxed-dispatcher dispatcher))

      ;; Execute successful tools
      (sandboxed-dispatcher-call-tool sandboxed
                                     "send_message"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test 1")
  ht)
                                     test-agent-id)
      (sandboxed-dispatcher-call-tool sandboxed
                                     "send_message"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test 2")
  ht)
                                     test-agent-id)

      ;; Get statistics
      (def stats (sandboxed-dispatcher-get-stats sandboxed))
      (check (= (hash-ref stats 'total) 2))
      (check (= (hash-ref stats 'completed) 2))
      (check (= (hash-ref stats 'success_rate) 100.0)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(def sandbox-test-suite
  (test-suite "Sandbox Test Suite"
    sandbox-config-tests
    sandbox-validation-tests
    sandboxed-execution-tests
    sandboxed-dispatcher-tests
    sandbox-utilities-tests
    sandbox-integration-tests))

;;; Run tests
#|
(import :std/test)
(test-run! sandbox-test-suite)
|#
