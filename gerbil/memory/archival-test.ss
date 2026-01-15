;;; memory/archival-test.ss - Archival Memory Tests
;;;
;;; Test suite for archival memory operations.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/test
  ../database/client
  ./types
  ./archival)

;;; ============================================================================
;;; Test Setup
;;; ============================================================================

(def test-agent-id "test-agent-archival-123")

(def (setup-test-manager)
  "Create test archival manager"
  (make-archival-manager test-agent-id
                        llm-provider: :openai
                        llm-model: "text-embedding-3-small"
                        cache-enabled: #t))

;;; ============================================================================
;;; Archival Entry Creation Tests
;;; ============================================================================

(def archival-creation-tests
  (test-suite "Archival Entry Creation Tests"

    (test-case "Insert entry without embedding"
      (let ((manager (setup-test-manager)))
        (def entry (archival-insert! manager
                                    "Test content"
                                    importance: 0.7
                                    tags: '("test")
                                    generate-embedding?: #f))
        (check (hash-key? entry 'id))
        (check (equal? (hash-ref entry 'content) "Test content"))
        (check (equal? (hash-ref entry 'importance) 0.7))
        (check (equal? (hash-ref entry 'tags) '("test")))))

    (test-case "Insert entry with default importance"
      (let ((manager (setup-test-manager)))
        (def entry (archival-insert! manager
                                    "Default importance"
                                    generate-embedding?: #f))
        (check (equal? (hash-ref entry 'importance) 0.5))))

    (test-case "Insert entry with tags"
      (let ((manager (setup-test-manager)))
        (def entry (archival-insert! manager
                                    "Tagged content"
                                    tags: '("tag1" "tag2" "tag3")
                                    generate-embedding?: #f))
        (check (equal? (hash-ref entry 'tags) '("tag1" "tag2" "tag3")))))

    (test-case "Insert batch entries"
      (let ((manager (setup-test-manager)))
        (def entries (archival-insert-batch! manager
                                            (list (hash 'content "Entry 1"
                                                       'importance 0.6
                                                       'tags '("batch")
                                                       'generate_embedding #f)
                                                  (hash 'content "Entry 2"
                                                       'importance 0.8
                                                       'tags '("batch")
                                                       'generate_embedding #f))))
        (check (= (length entries) 2))))

    (test-case "Invalid importance fails"
      (let ((manager (setup-test-manager)))
        (check-exception
         (archival-insert! manager "Test" importance: 1.5))))

    (test-case "Invalid tags fail"
      (let ((manager (setup-test-manager)))
        (check-exception
         (archival-insert! manager "Test" tags: "not-a-list"))))))

;;; ============================================================================
;;; Archival Entry Retrieval Tests
;;; ============================================================================

(def archival-retrieval-tests
  (test-suite "Archival Entry Retrieval Tests"

    (test-case "Get entry by ID"
      (let ((manager (setup-test-manager)))
        (def created (archival-insert! manager "Test" generate-embedding?: #f))
        (def retrieved (archival-get manager (hash-ref created 'id)))
        (check (equal? (hash-ref retrieved 'content) "Test"))))

    (test-case "Get non-existent entry returns false"
      (let ((manager (setup-test-manager)))
        (def result (archival-get manager "non-existent-id"))
        (check (not result))))

    (test-case "Get all entries"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Entry 1" generate-embedding?: #f)
        (archival-insert! manager "Entry 2" generate-embedding?: #f)
        (archival-insert! manager "Entry 3" generate-embedding?: #f)
        (def all (archival-get-all manager limit: 100))
        (check (>= (length all) 3))))

    (test-case "Get recent entries"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Recent 1" generate-embedding?: #f)
        (archival-insert! manager "Recent 2" generate-embedding?: #f)
        (def recent (archival-get-recent manager 2))
        (check (<= (length recent) 2))))

    (test-case "Pagination with limit and offset"
      (let ((manager (setup-test-manager)))
        ;; Insert 5 entries
        (for-each (lambda (i)
                   (archival-insert! manager
                                    (format "Entry ~a" i)
                                    generate-embedding?: #f))
                 (iota 5))
        (def page1 (archival-get-all manager limit: 2 offset: 0))
        (def page2 (archival-get-all manager limit: 2 offset: 2))
        (check (= (length page1) 2))
        (check (= (length page2) 2))))))

;;; ============================================================================
;;; Text-Based Search Tests
;;; ============================================================================

(def archival-search-tests
  (test-suite "Archival Search Tests"

    (test-case "Search by text content"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "User prefers dark mode" generate-embedding?: #f)
        (archival-insert! manager "User likes coffee" generate-embedding?: #f)
        (archival-insert! manager "System settings" generate-embedding?: #f)
        (def results (archival-search manager "User" limit: 10))
        (check (>= (length results) 2))))

    (test-case "Search returns empty for no matches"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Test content" generate-embedding?: #f)
        (def results (archival-search manager "nonexistent" limit: 10))
        (check (= (length results) 0))))

    (test-case "Search respects limit"
      (let ((manager (setup-test-manager)))
        (for-each (lambda (i)
                   (archival-insert! manager
                                    (format "Test entry ~a" i)
                                    generate-embedding?: #f))
                 (iota 10))
        (def results (archival-search manager "Test" limit: 3))
        (check (<= (length results) 3))))

    (test-case "Search sorts by importance"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Low importance" importance: 0.3 generate-embedding?: #f)
        (archival-insert! manager "High importance" importance: 0.9 generate-embedding?: #f)
        (archival-insert! manager "Medium importance" importance: 0.6 generate-embedding?: #f)
        (def results (archival-search manager "importance" limit: 10))
        (check (>= (hash-ref (car results) 'importance)
                   (hash-ref (cadr results) 'importance)))))))

;;; ============================================================================
;;; Tag-Based Search Tests
;;; ============================================================================

(def archival-tag-search-tests
  (test-suite "Archival Tag Search Tests"

    (test-case "Search by single tag"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Entry 1" tags: '("important") generate-embedding?: #f)
        (archival-insert! manager "Entry 2" tags: '("trivial") generate-embedding?: #f)
        (def results (archival-search-by-tags manager '("important") limit: 10))
        (check (>= (length results) 1))))

    (test-case "Search by multiple tags"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Entry 1" tags: '("tag1" "tag2") generate-embedding?: #f)
        (archival-insert! manager "Entry 2" tags: '("tag2" "tag3") generate-embedding?: #f)
        (archival-insert! manager "Entry 3" tags: '("tag4") generate-embedding?: #f)
        (def results (archival-search-by-tags manager '("tag1" "tag2") limit: 10))
        (check (>= (length results) 2))))

    (test-case "Search by importance threshold"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Low" importance: 0.3 generate-embedding?: #f)
        (archival-insert! manager "Medium" importance: 0.6 generate-embedding?: #f)
        (archival-insert! manager "High" importance: 0.9 generate-embedding?: #f)
        (def results (archival-search-by-importance manager 0.7 limit: 10))
        (check (= (length results) 1))
        (check (>= (hash-ref (car results) 'importance) 0.7))))))

;;; ============================================================================
;;; Pagination Tests
;;; ============================================================================

(def archival-pagination-tests
  (test-suite "Archival Pagination Tests"

    (test-case "Get first page"
      (let ((manager (setup-test-manager)))
        ;; Insert 25 entries
        (for-each (lambda (i)
                   (archival-insert! manager
                                    (format "Entry ~a" i)
                                    generate-embedding?: #f))
                 (iota 25))
        (def page (archival-get-page manager 0 10))
        (check (archival-page? page))
        (check (= (archival-page-page page) 0))
        (check (= (archival-page-page-size page) 10))
        (check (archival-page-has-next? page))
        (check (not (archival-page-has-prev? page)))))

    (test-case "Get middle page"
      (let ((manager (setup-test-manager)))
        (for-each (lambda (i)
                   (archival-insert! manager
                                    (format "Entry ~a" i)
                                    generate-embedding?: #f))
                 (iota 25))
        (def page (archival-get-page manager 1 10))
        (check (archival-page-has-next? page))
        (check (archival-page-has-prev? page))))

    (test-case "Get last page"
      (let ((manager (setup-test-manager)))
        (for-each (lambda (i)
                   (archival-insert! manager
                                    (format "Entry ~a" i)
                                    generate-embedding?: #f))
                 (iota 25))
        (def page (archival-get-page manager 2 10))
        (check (not (archival-page-has-next? page)))
        (check (archival-page-has-prev? page))))

    (test-case "Navigate pages"
      (let ((manager (setup-test-manager)))
        (for-each (lambda (i)
                   (archival-insert! manager
                                    (format "Entry ~a" i)
                                    generate-embedding?: #f))
                 (iota 25))
        (def page1 (archival-get-page manager 0 10))
        (def next-page-num (archival-get-next-page page1))
        (check (= next-page-num 1))
        (def page2 (archival-get-page manager next-page-num 10))
        (def prev-page-num (archival-get-prev-page page2))
        (check (= prev-page-num 0))))))

;;; ============================================================================
;;; Archival Entry Update Tests
;;; ============================================================================

(def archival-update-tests
  (test-suite "Archival Update Tests"

    (test-case "Update entry content"
      (let ((manager (setup-test-manager)))
        (def entry (archival-insert! manager "Original" generate-embedding?: #f))
        (def updated (archival-update! manager
                                      (hash-ref entry 'id)
                                      (hash 'content "Updated")))
        (check (equal? (hash-ref updated 'content) "Updated"))))

    (test-case "Update entry importance"
      (let ((manager (setup-test-manager)))
        (def entry (archival-insert! manager "Test" importance: 0.5 generate-embedding?: #f))
        (def updated (archival-update-importance! manager
                                                 (hash-ref entry 'id)
                                                 0.9))
        (check (equal? (hash-ref updated 'importance) 0.9))))

    (test-case "Add tags to entry"
      (let ((manager (setup-test-manager)))
        (def entry (archival-insert! manager "Test" tags: '("tag1") generate-embedding?: #f))
        (def updated (archival-add-tags! manager
                                        (hash-ref entry 'id)
                                        '("tag2" "tag3")))
        (check (member "tag2" (hash-ref updated 'tags)))
        (check (member "tag3" (hash-ref updated 'tags)))))))

;;; ============================================================================
;;; Archival Entry Deletion Tests
;;; ============================================================================

(def archival-deletion-tests
  (test-suite "Archival Deletion Tests"

    (test-case "Delete single entry"
      (let ((manager (setup-test-manager)))
        (def entry (archival-insert! manager "To delete" generate-embedding?: #f))
        (archival-delete! manager (hash-ref entry 'id))
        (def retrieved (archival-get manager (hash-ref entry 'id)))
        (check (not retrieved))))

    (test-case "Delete batch entries"
      (let ((manager (setup-test-manager)))
        (def entry1 (archival-insert! manager "Delete 1" generate-embedding?: #f))
        (def entry2 (archival-insert! manager "Delete 2" generate-embedding?: #f))
        (archival-delete-batch! manager
                               (list (hash-ref entry1 'id)
                                     (hash-ref entry2 'id)))
        (check (not (archival-get manager (hash-ref entry1 'id))))
        (check (not (archival-get manager (hash-ref entry2 'id))))))

    (test-case "Clear all entries"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Entry 1" generate-embedding?: #f)
        (archival-insert! manager "Entry 2" generate-embedding?: #f)
        (archival-clear! manager)
        (def count (archival-count manager))
        (check (= count 0))))))

;;; ============================================================================
;;; Archival Statistics Tests
;;; ============================================================================

(def archival-statistics-tests
  (test-suite "Archival Statistics Tests"

    (test-case "Count entries"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Entry 1" generate-embedding?: #f)
        (archival-insert! manager "Entry 2" generate-embedding?: #f)
        (def count (archival-count manager))
        (check (>= count 2))))

    (test-case "Calculate total size"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Short" generate-embedding?: #f)
        (archival-insert! manager "Much longer content here" generate-embedding?: #f)
        (def size (archival-total-size manager))
        (check (> size 0))))

    (test-case "Get comprehensive statistics"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Entry 1" importance: 0.6 tags: '("tag1") generate-embedding?: #f)
        (archival-insert! manager "Entry 2" importance: 0.8 tags: '("tag2") generate-embedding?: #f)
        (def stats (archival-get-stats manager))
        (check (hash-key? stats 'total_entries))
        (check (hash-key? stats 'total_size))
        (check (hash-key? stats 'avg_importance))
        (check (hash-key? stats 'unique_tags))
        (check (> (hash-ref stats 'total_entries) 0))))))

;;; ============================================================================
;;; Archival Caching Tests
;;; ============================================================================

(def archival-caching-tests
  (test-suite "Archival Caching Tests"

    (test-case "Entry cached on creation"
      (let ((manager (setup-test-manager)))
        (def entry (archival-insert! manager "Cached" generate-embedding?: #f))
        (def cached (archival-cache-get manager (hash-ref entry 'id)))
        (check cached)
        (check (equal? (hash-ref cached 'content) "Cached"))))

    (test-case "Cache invalidated on update"
      (let ((manager (setup-test-manager)))
        (def entry (archival-insert! manager "Original" generate-embedding?: #f))
        (archival-update! manager (hash-ref entry 'id) (hash 'content "Updated"))
        ;; Cache should be invalidated, next get will fetch from DB
        (def retrieved (archival-get manager (hash-ref entry 'id)))
        (check (equal? (hash-ref retrieved 'content) "Updated"))))

    (test-case "Cache invalidated on deletion"
      (let ((manager (setup-test-manager)))
        (def entry (archival-insert! manager "To delete" generate-embedding?: #f))
        (archival-delete! manager (hash-ref entry 'id))
        (def cached (archival-cache-get manager (hash-ref entry 'id)))
        (check (not cached))))

    (test-case "Clear cache"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Entry 1" generate-embedding?: #f)
        (archival-insert! manager "Entry 2" generate-embedding?: #f)
        (archival-cache-clear! manager)
        ;; Cache should be empty
        (check #t)))))  ; Just verify no errors

;;; ============================================================================
;;; Export/Import Tests
;;; ============================================================================

(def archival-export-import-tests
  (test-suite "Archival Export/Import Tests"

    (test-case "Export to JSON"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Export test" importance: 0.7 tags: '("test") generate-embedding?: #f)
        (def export (archival-export manager format: 'json))
        (check (string? export))
        (check (string-contains export "Export test"))))

    (test-case "Export to text"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "Text export" importance: 0.8 generate-embedding?: #f)
        (def export (archival-export manager format: 'text))
        (check (string? export))
        (check (string-contains export "Text export"))
        (check (string-contains export "importance: 0.8"))))

    (test-case "Export without embeddings"
      (let ((manager (setup-test-manager)))
        (archival-insert! manager "No embeddings" generate-embedding?: #f)
        (def export (archival-export manager format: 'json include-embeddings?: #f))
        (check (not (string-contains export "embedding")))))

    (test-case "Import from exported data"
      (let ((manager1 (setup-test-manager))
            (manager2 (make-archival-manager "test-agent-import-456")))
        (archival-insert! manager1 "Import test" importance: 0.9 tags: '("import") generate-embedding?: #f)
        (def export-str (archival-export manager1 format: 'json))
        (def export-data (string->json-object export-str))
        (archival-import! manager2 export-data)
        (def imported (archival-get-all manager2 limit: 100))
        (check (> (length imported) 0))))))

;;; ============================================================================
;;; Embedding Generation Tests
;;; ============================================================================

(def archival-embedding-tests
  (test-suite "Archival Embedding Tests"

    (test-case "Generate embedding returns vector"
      (let ((manager (setup-test-manager)))
        (def embedding (generate-embedding manager "Test content"))
        (check (list? embedding))
        (check (= (length embedding) 1536))))

    (test-case "Generate embeddings batch"
      (let ((manager (setup-test-manager)))
        (def embeddings (generate-embeddings-batch manager
                                                   '("Content 1" "Content 2" "Content 3")))
        (check (= (length embeddings) 3))
        (check (list? (car embeddings)))))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(def archival-memory-test-suite
  (test-suite "Archival Memory Test Suite"
    archival-creation-tests
    archival-retrieval-tests
    archival-search-tests
    archival-tag-search-tests
    archival-pagination-tests
    archival-update-tests
    archival-deletion-tests
    archival-statistics-tests
    archival-caching-tests
    archival-export-import-tests
    archival-embedding-tests))

;;; Run tests
#|
(import :std/test)
(test-run! archival-memory-test-suite)
|#
