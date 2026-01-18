;;; tools/memory-test.ss - Memory Tools Tests
;;;
;;; Test suite for memory tool implementations.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/test
  :o/database/client
  :o/memory/blocks
  :o/memory/core
  :o/memory/archival
  :o/tools/types
  :o/tools/core
  :o/tools/memory)

;;; ============================================================================
;;; Test Setup
;;; ============================================================================

(def test-agent-id "test-agent-memory-tools-123")

(def (setup-test-dispatcher)
  "Create test dispatcher with core and memory tools registered"
  (let ((dispatcher (make-tool-dispatcher)))
    (register-core-tools! dispatcher)
    (register-memory-tools! dispatcher)
    dispatcher))

;;; ============================================================================
;;; Core Memory Append Tool Tests
;;; ============================================================================

(def core-memory-append-tests
  (test-suite "Core Memory Append Tool Tests"

    (test-case "Core memory append tool definition"
      (check (tool-definition? core-memory-append-tool))
      (check (equal? (tool-definition-name core-memory-append-tool) "core_memory_append"))
      (check (eq? (tool-definition-category core-memory-append-tool) 'memory))
      (check (not (tool-definition-requires-approval core-memory-append-tool))))

    (test-case "Append to persona memory"
      (def dispatcher (setup-test-dispatcher))
      (def call (dispatcher-call-tool dispatcher
                                     "core_memory_append"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'content "I am helpful and friendly.")
  ht)
                                     test-agent-id))
      (check (tool-call-completed? call))
      (check (hash-key? (tool-call-result call) 'block_name))
      (check (equal? (hash-ref (tool-call-result call) 'block_name) "persona")))

    (test-case "Append to human memory"
      (def dispatcher (setup-test-dispatcher))
      (def call (dispatcher-call-tool dispatcher
                                     "core_memory_append"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'name "human")
  (hash-put! ht 'content "User prefers Python.")
  ht)
                                     test-agent-id))
      (check (tool-call-completed? call))
      (check (hash-key? (tool-call-result call) 'new_value)))

    (test-case "Append validates arguments"
      (def valid-args (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'content "Test")
  ht))
      (check (car (validate-tool-arguments core-memory-append-tool valid-args)))

      (def missing-name (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Test")
  ht))
      (check (not (car (validate-tool-arguments core-memory-append-tool missing-name))))

      (def missing-content (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  ht))
      (check (not (car (validate-tool-arguments core-memory-append-tool missing-content)))))))

;;; ============================================================================
;;; Core Memory Replace Tool Tests
;;; ============================================================================

(def core-memory-replace-tests
  (test-suite "Core Memory Replace Tool Tests"

    (test-case "Core memory replace tool definition"
      (check (tool-definition? core-memory-replace-tool))
      (check (equal? (tool-definition-name core-memory-replace-tool) "core_memory_replace"))
      (check (eq? (tool-definition-category core-memory-replace-tool) 'memory))
      (check (not (tool-definition-requires-approval core-memory-replace-tool))))

    (test-case "Replace in persona memory"
      (def dispatcher (setup-test-dispatcher))

      ;; First append some content
      (dispatcher-call-tool dispatcher
                           "core_memory_append"
                           (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'content "I am helpful.")
  ht)
                           test-agent-id)

      ;; Then replace
      (def call (dispatcher-call-tool dispatcher
                                     "core_memory_replace"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'old_content "helpful")
  (hash-put! ht 'new_content "very helpful")
  ht)
                                     test-agent-id))
      (check (tool-call-completed? call))
      (check (hash-key? (tool-call-result call) 'block_name)))

    (test-case "Replace validates arguments"
      (def valid-args (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'old_content "old")
  (hash-put! ht 'new_content "new")
  ht))
      (check (car (validate-tool-arguments core-memory-replace-tool valid-args)))

      (def missing-old (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'new_content "new")
  ht))
      (check (not (car (validate-tool-arguments core-memory-replace-tool missing-old)))))))

;;; ============================================================================
;;; Archival Memory Insert Tool Tests
;;; ============================================================================

(def archival-memory-insert-tests
  (test-suite "Archival Memory Insert Tool Tests"

    (test-case "Archival memory insert tool definition"
      (check (tool-definition? archival-memory-insert-tool))
      (check (equal? (tool-definition-name archival-memory-insert-tool) "archival_memory_insert"))
      (check (eq? (tool-definition-category archival-memory-insert-tool) 'memory))
      (check (not (tool-definition-requires-approval archival-memory-insert-tool))))

    (test-case "Insert into archival memory"
      (def dispatcher (setup-test-dispatcher))
      (def call (dispatcher-call-tool dispatcher
                                     "archival_memory_insert"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'content "User prefers Python over JavaScript")
  (hash-put! ht 'importance 8)
  (hash-put! ht 'tags '("preferences"))
  ht)
                                     test-agent-id))
      (check (tool-call-completed? call))
      (check (hash-key? (tool-call-result call) 'id))
      (check (hash-key? (tool-call-result call) 'content))
      (check (equal? (hash-ref (tool-call-result call) 'importance) 8)))

    (test-case "Insert with default importance"
      (def dispatcher (setup-test-dispatcher))
      (def call (dispatcher-call-tool dispatcher
                                     "archival_memory_insert"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Test content")
  ht)
                                     test-agent-id))
      (check (tool-call-completed? call))
      (check (equal? (hash-ref (tool-call-result call) 'importance) 5)))

    (test-case "Insert validates arguments"
      (def valid-args (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Test content")
  ht))
      (check (car (validate-tool-arguments archival-memory-insert-tool valid-args)))

      (def missing-content (let ((ht (make-hash-table)))
  (hash-put! ht 'importance 5)
  ht))
      (check (not (car (validate-tool-arguments archival-memory-insert-tool missing-content)))))))

;;; ============================================================================
;;; Archival Memory Search Tool Tests
;;; ============================================================================

(def archival-memory-search-tests
  (test-suite "Archival Memory Search Tool Tests"

    (test-case "Archival memory search tool definition"
      (check (tool-definition? archival-memory-search-tool))
      (check (equal? (tool-definition-name archival-memory-search-tool) "archival_memory_search"))
      (check (eq? (tool-definition-category archival-memory-search-tool) 'memory))
      (check (not (tool-definition-requires-approval archival-memory-search-tool))))

    (test-case "Search archival memory"
      (def dispatcher (setup-test-dispatcher))

      ;; Insert some entries
      (dispatcher-call-tool dispatcher
                           "archival_memory_insert"
                           (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Python is a great language")
  (hash-put! ht 'tags '("programming"))
  ht)
                           test-agent-id)

      (dispatcher-call-tool dispatcher
                           "archival_memory_insert"
                           (let ((ht (make-hash-table)))
  (hash-put! ht 'content "JavaScript is also useful")
  (hash-put! ht 'tags '("programming"))
  ht)
                           test-agent-id)

      ;; Search
      (def call (dispatcher-call-tool dispatcher
                                     "archival_memory_search"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'query "Python")
  (hash-put! ht 'limit 5)
  ht)
                                     test-agent-id))
      (check (tool-call-completed? call))
      (check (hash-key? (tool-call-result call) 'results))
      (check (hash-key? (tool-call-result call) 'count)))

    (test-case "Search with pagination"
      (def dispatcher (setup-test-dispatcher))
      (def call (dispatcher-call-tool dispatcher
                                     "archival_memory_search"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'query "test")
  (hash-put! ht 'limit 10)
  (hash-put! ht 'page 0)
  ht)
                                     test-agent-id))
      (check (tool-call-completed? call))
      (check (equal? (hash-ref (tool-call-result call) 'page) 0)))

    (test-case "Search validates arguments"
      (def valid-args (let ((ht (make-hash-table)))
  (hash-put! ht 'query "test")
  ht))
      (check (car (validate-tool-arguments archival-memory-search-tool valid-args)))

      (def missing-query (let ((ht (make-hash-table)))
  (hash-put! ht 'limit 10)
  ht))
      (check (not (car (validate-tool-arguments archival-memory-search-tool missing-query)))))))

;;; ============================================================================
;;; Archival Memory Semantic Search Tool Tests
;;; ============================================================================

(def archival-memory-semantic-search-tests
  (test-suite "Archival Memory Semantic Search Tool Tests"

    (test-case "Archival memory semantic search tool definition"
      (check (tool-definition? archival-memory-semantic-search-tool))
      (check (equal? (tool-definition-name archival-memory-semantic-search-tool) "archival_memory_semantic_search"))
      (check (eq? (tool-definition-category archival-memory-semantic-search-tool) 'memory))
      (check (not (tool-definition-requires-approval archival-memory-semantic-search-tool))))

    (test-case "Semantic search archival memory"
      (def dispatcher (setup-test-dispatcher))

      ;; Insert entries with embeddings
      (dispatcher-call-tool dispatcher
                           "archival_memory_insert"
                           (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Python programming language")
  (hash-put! ht 'importance 8)
  ht)
                           test-agent-id)

      ;; Semantic search
      (def call (dispatcher-call-tool dispatcher
                                     "archival_memory_semantic_search"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'query "coding in Python")
  (hash-put! ht 'limit 5)
  (hash-put! ht 'min_similarity 0.7)
  ht)
                                     test-agent-id))
      (check (tool-call-completed? call))
      (check (hash-key? (tool-call-result call) 'results))
      (check (hash-key? (tool-call-result call) 'count)))

    (test-case "Semantic search with default min_similarity"
      (def dispatcher (setup-test-dispatcher))
      (def call (dispatcher-call-tool dispatcher
                                     "archival_memory_semantic_search"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'query "test query")
  ht)
                                     test-agent-id))
      (check (tool-call-completed? call))
      (check (equal? (hash-ref (tool-call-result call) 'min_similarity) 0.7)))

    (test-case "Semantic search validates arguments"
      (def valid-args (let ((ht (make-hash-table)))
  (hash-put! ht 'query "test")
  ht))
      (check (car (validate-tool-arguments archival-memory-semantic-search-tool valid-args)))

      (def missing-query (let ((ht (make-hash-table)))
  (hash-put! ht 'limit 10)
  ht))
      (check (not (car (validate-tool-arguments archival-memory-semantic-search-tool missing-query)))))))

;;; ============================================================================
;;; Memory Tools Integration Tests
;;; ============================================================================

(def memory-tools-integration-tests
  (test-suite "Memory Tools Integration Tests"

    (test-case "Register memory tools with dispatcher"
      (def dispatcher (make-tool-dispatcher))
      (register-memory-tools! dispatcher)
      (check (registry-has-tool? (tool-dispatcher-registry dispatcher) "core_memory_append"))
      (check (registry-has-tool? (tool-dispatcher-registry dispatcher) "core_memory_replace"))
      (check (registry-has-tool? (tool-dispatcher-registry dispatcher) "archival_memory_insert"))
      (check (registry-has-tool? (tool-dispatcher-registry dispatcher) "archival_memory_search"))
      (check (registry-has-tool? (tool-dispatcher-registry dispatcher) "archival_memory_semantic_search")))

    (test-case "List memory tools by category"
      (def dispatcher (setup-test-dispatcher))
      (def memory-tools (registry-list-tools (tool-dispatcher-registry dispatcher)
                                            category: 'memory))
      (check (>= (length memory-tools) 5)))

    (test-case "Core memory workflow"
      (def dispatcher (setup-test-dispatcher))

      ;; Append to persona
      (def append-call (dispatcher-call-tool dispatcher
                                            "core_memory_append"
                                            (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'content "I am helpful.")
  ht)
                                            test-agent-id))
      (check (tool-call-completed? append-call))

      ;; Replace in persona
      (def replace-call (dispatcher-call-tool dispatcher
                                             "core_memory_replace"
                                             (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'old_content "helpful")
  (hash-put! ht 'new_content "very helpful")
  ht)
                                             test-agent-id))
      (check (tool-call-completed? replace-call)))

    (test-case "Archival memory workflow"
      (def dispatcher (setup-test-dispatcher))

      ;; Insert entry
      (def insert-call (dispatcher-call-tool dispatcher
                                            "archival_memory_insert"
                                            (let ((ht (make-hash-table)))
  (hash-put! ht 'content "User likes Python")
  (hash-put! ht 'importance 8)
  (hash-put! ht 'tags '("preferences"))
  ht)
                                            test-agent-id))
      (check (tool-call-completed? insert-call))

      ;; Search for entry
      (def search-call (dispatcher-call-tool dispatcher
                                            "archival_memory_search"
                                            (let ((ht (make-hash-table)))
  (hash-put! ht 'query "Python")
  ht)
                                            test-agent-id))
      (check (tool-call-completed? search-call))
      (check (> (hash-ref (tool-call-result search-call) 'count) 0)))

    (test-case "Tool call history tracking"
      (def dispatcher (setup-test-dispatcher))

      ;; Make several tool calls
      (dispatcher-call-tool dispatcher "core_memory_append"
                           (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'content "Test 1")
  ht)
                           test-agent-id)
      (dispatcher-call-tool dispatcher "archival_memory_insert"
                           (let ((ht (make-hash-table)))
  (hash-put! ht 'content "Test 2")
  ht)
                           test-agent-id)

      ;; Check history
      (def history (dispatcher-get-history dispatcher limit: 10))
      (check (>= (length history) 2)))))

;;; ============================================================================
;;; Memory Tools Error Handling Tests
;;; ============================================================================

(def memory-tools-error-tests
  (test-suite "Memory Tools Error Handling Tests"

    (test-case "Core memory append with invalid block name"
      (def dispatcher (setup-test-dispatcher))
      (def call (dispatcher-call-tool dispatcher
                                     "core_memory_append"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'name "nonexistent_block")
  (hash-put! ht 'content "Test")
  ht)
                                     test-agent-id))
      ;; Should handle gracefully
      (check (tool-call? call)))

    (test-case "Core memory replace with non-matching content"
      (def dispatcher (setup-test-dispatcher))

      ;; Append first
      (dispatcher-call-tool dispatcher
                           "core_memory_append"
                           (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'content "I am helpful.")
  ht)
                           test-agent-id)

      ;; Try to replace non-existent content
      (def call (dispatcher-call-tool dispatcher
                                     "core_memory_replace"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'old_content "nonexistent")
  (hash-put! ht 'new_content "new")
  ht)
                                     test-agent-id))
      ;; Should handle gracefully
      (check (tool-call? call)))

    (test-case "Archival memory search with empty query"
      (def dispatcher (setup-test-dispatcher))
      (def call (dispatcher-call-tool dispatcher
                                     "archival_memory_search"
                                     (let ((ht (make-hash-table)))
  (hash-put! ht 'query "")
  ht)
                                     test-agent-id))
      ;; Should handle gracefully
      (check (tool-call? call)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(def memory-tools-test-suite
  (test-suite "Memory Tools Test Suite"
    core-memory-append-tests
    core-memory-replace-tests
    archival-memory-insert-tests
    archival-memory-search-tests
    archival-memory-semantic-search-tests
    memory-tools-integration-tests
    memory-tools-error-tests))

;;; Run tests
#|
(import :std/test)
(test-run! memory-tools-test-suite)
|#
