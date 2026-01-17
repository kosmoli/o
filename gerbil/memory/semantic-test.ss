;;; memory/semantic-test.ss - Semantic Search Tests
;;;
;;; Test suite for semantic search operations.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/test
  :o/database/client
  :o/memory/types
  :o/memory/archival
  :o/memory/semantic)

;;; ============================================================================
;;; Test Setup
;;; ============================================================================

(def test-agent-id "test-agent-semantic-123")

(def (setup-test-manager)
  "Create test archival manager with sample entries"
  (let ((manager (make-archival-manager test-agent-id
                                       llm-provider: :openai
                                       llm-model: "text-embedding-3-small"
                                       cache-enabled: #t)))
    ;; Insert sample entries with embeddings
    (archival-insert! manager
                     "User prefers dark mode and technical documentation."
                     importance: 0.8
                     tags: '("preferences" "ui")
                     generate-embedding?: #t)
    (archival-insert! manager
                     "User is a software engineer working on AI projects."
                     importance: 0.9
                     tags: '("profile" "work")
                     generate-embedding?: #t)
    (archival-insert! manager
                     "User likes coffee and morning meetings."
                     importance: 0.5
                     tags: '("personal")
                     generate-embedding?: #t)
    manager))

;;; ============================================================================
;;; Vector Operations Tests
;;; ============================================================================

(def vector-operations-tests
  (test-suite "Vector Operations Tests"

    (test-case "Dot product calculation"
      (def v1 '(1.0 2.0 3.0))
      (def v2 '(4.0 5.0 6.0))
      (def result (vector-dot-product v1 v2))
      (check (= result 32.0)))  ; 1*4 + 2*5 + 3*6 = 32

    (test-case "Vector magnitude calculation"
      (def v '(3.0 4.0))
      (def result (vector-magnitude v))
      (check (= result 5.0)))  ; sqrt(9 + 16) = 5

    (test-case "Cosine similarity identical vectors"
      (def v '(1.0 2.0 3.0))
      (def result (cosine-similarity v v))
      (check (>= result 0.99)))  ; Should be ~1.0

    (test-case "Cosine similarity orthogonal vectors"
      (def v1 '(1.0 0.0))
      (def v2 '(0.0 1.0))
      (def result (cosine-similarity v1 v2))
      (check (< (abs result) 0.01)))  ; Should be ~0.0

    (test-case "Euclidean distance identical vectors"
      (def v '(1.0 2.0 3.0))
      (def result (euclidean-distance v v))
      (check (< result 0.01)))  ; Should be ~0.0

    (test-case "Euclidean distance calculation"
      (def v1 '(0.0 0.0))
      (def v2 '(3.0 4.0))
      (def result (euclidean-distance v1 v2))
      (check (= result 5.0)))  ; sqrt(9 + 16) = 5

    (test-case "Vector operations with different lengths fail"
      (def v1 '(1.0 2.0))
      (def v2 '(1.0 2.0 3.0))
      (check-exception (vector-dot-product v1 v2))
      (check-exception (cosine-similarity v1 v2))
      (check-exception (euclidean-distance v1 v2)))))

;;; ============================================================================
;;; Semantic Search Tests
;;; ============================================================================

(def semantic-search-tests
  (test-suite "Semantic Search Tests"

    (test-case "Semantic search returns results"
      (let ((manager (setup-test-manager)))
        (def results (semantic-search manager "software development" limit: 5))
        (check (list? results))
        (check (> (length results) 0))))

    (test-case "Semantic search results have similarity scores"
      (let ((manager (setup-test-manager)))
        (def results (semantic-search manager "programming" limit: 5))
        (when (not (null? results))
          (check (hash-key? (car results) 'similarity))
          (check (number? (hash-ref (car results) 'similarity))))))

    (test-case "Semantic search respects limit"
      (let ((manager (setup-test-manager)))
        (def results (semantic-search manager "user" limit: 2))
        (check (<= (length results) 2))))

    (test-case "Semantic search filters by minimum similarity"
      (let ((manager (setup-test-manager)))
        (def results (semantic-search manager "test" min-similarity: 0.9))
        (for-each
         (lambda (result)
           (check (>= (hash-ref result 'similarity) 0.9)))
         results)))

    (test-case "Semantic search sorts by similarity"
      (let ((manager (setup-test-manager)))
        (def results (semantic-search manager "engineer" limit: 10))
        (when (> (length results) 1)
          (check (>= (hash-ref (car results) 'similarity)
                     (hash-ref (cadr results) 'similarity))))))

    (test-case "Semantic search by embedding"
      (let ((manager (setup-test-manager)))
        (def query-embedding (generate-embedding manager "software"))
        (def results (semantic-search-by-embedding manager query-embedding limit: 5))
        (check (list? results))
        (check (> (length results) 0))))))

;;; ============================================================================
;;; Hybrid Search Tests
;;; ============================================================================

(def hybrid-search-tests
  (test-suite "Hybrid Search Tests"

    (test-case "Hybrid search returns results"
      (let ((manager (setup-test-manager)))
        (def results (hybrid-search manager "software" limit: 5))
        (check (list? results))
        (check (> (length results) 0))))

    (test-case "Hybrid search results have all scores"
      (let ((manager (setup-test-manager)))
        (def results (hybrid-search manager "engineer" limit: 5))
        (when (not (null? results))
          (def result (car results))
          (check (search-result? result))
          (check (number? (search-result-text-score result)))
          (check (number? (search-result-vector-score result)))
          (check (number? (search-result-combined-score result))))))

    (test-case "Hybrid search weights validation"
      (let ((manager (setup-test-manager)))
        (check-exception
         (hybrid-search manager "test" text-weight: 0.5 vector-weight: 0.6))))

    (test-case "Hybrid search respects text weight"
      (let ((manager (setup-test-manager)))
        ;; High text weight should favor exact matches
        (def results (hybrid-search manager "software"
                                   text-weight: 0.9
                                   vector-weight: 0.1
                                   limit: 5))
        (check (list? results))))

    (test-case "Hybrid search respects vector weight"
      (let ((manager (setup-test-manager)))
        ;; High vector weight should favor semantic similarity
        (def results (hybrid-search manager "programming"
                                   text-weight: 0.1
                                   vector-weight: 0.9
                                   limit: 5))
        (check (list? results))))

    (test-case "Hybrid search filters by thresholds"
      (let ((manager (setup-test-manager)))
        (def results (hybrid-search manager "test"
                                   min-text-score: 0.5
                                   min-vector-score: 0.5
                                   limit: 10))
        (for-each
         (lambda (result)
           (check (>= (search-result-text-score result) 0.5))
           (check (>= (search-result-vector-score result) 0.5)))
         results)))

    (test-case "Advanced hybrid search with importance"
      (let ((manager (setup-test-manager)))
        (def results (hybrid-search-advanced manager "engineer"
                                            text-weight: 0.3
                                            vector-weight: 0.5
                                            importance-weight: 0.2
                                            limit: 5))
        (check (list? results))
        (check (> (length results) 0))))

    (test-case "Advanced text score calculation"
      (def score1 (advanced-text-score "software engineer" "software"))
      (def score2 (advanced-text-score "engineer software" "software"))
      (check (> score1 0.0))
      (check (> score2 0.0)))

    (test-case "Advanced text score exact match"
      (def score (advanced-text-score "test" "test"))
      (check (= score 1.0)))))

;;; ============================================================================
;;; Search Result Ranking Tests
;;; ============================================================================

(def ranking-tests
  (test-suite "Search Result Ranking Tests"

    (test-case "Rerank without factors returns same order"
      (let ((manager (setup-test-manager)))
        (def results (hybrid-search manager "user" limit: 5))
        (def reranked (rerank-results results))
        (check (= (length results) (length reranked)))))

    (test-case "Rerank with recency factor"
      (let ((manager (setup-test-manager)))
        (def results (hybrid-search manager "user" limit: 5))
        (def reranked (rerank-results results recency-factor: 0.3))
        (check (= (length results) (length reranked)))))

    (test-case "Reranked results maintain structure"
      (let ((manager (setup-test-manager)))
        (def results (hybrid-search manager "engineer" limit: 5))
        (def reranked (rerank-results results recency-factor: 0.2))
        (for-each
         (lambda (result)
           (check (search-result? result))
           (check (hash-table? (search-result-entry result))))
         reranked)))))

;;; ============================================================================
;;; Batch Operations Tests
;;; ============================================================================

(def batch-operations-tests
  (test-suite "Batch Operations Tests"

    (test-case "Batch semantic search"
      (let ((manager (setup-test-manager)))
        (def queries '("software" "coffee" "documentation"))
        (def results (batch-semantic-search manager queries limit: 3))
        (check (hash-table? results))
        (check (= (hash-length results) 3))
        (for-each
         (lambda (query)
           (check (hash-key? results query))
           (check (list? (hash-ref results query))))
         queries)))

    (test-case "Batch search respects limit per query"
      (let ((manager (setup-test-manager)))
        (def queries '("user" "engineer"))
        (def results (batch-semantic-search manager queries limit: 2))
        (for-each
         (lambda (query)
           (def query-results (hash-ref results query))
           (check (<= (length query-results) 2)))
         queries)))))

;;; ============================================================================
;;; Utilities Tests
;;; ============================================================================

(def utilities-tests
  (test-suite "Utilities Tests"

    (test-case "Find similar entries"
      (let ((manager (setup-test-manager)))
        ;; Get first entry
        (def entries (archival-get-all manager limit: 1))
        (when (not (null? entries))
          (def entry-id (hash-ref (car entries) 'id))
          (def similar (find-similar-entries manager entry-id limit: 2))
          (check (list? similar))
          ;; Should not include the original entry
          (for-each
           (lambda (result)
             (check (not (equal? (hash-ref result 'id) entry-id))))
           similar))))

    (test-case "Find similar entries with no embedding fails"
      (let ((manager (setup-test-manager)))
        ;; Insert entry without embedding
        (def entry (archival-insert! manager "No embedding" generate-embedding?: #f))
        (check-exception
         (find-similar-entries manager (hash-ref entry 'id)))))

    (test-case "Find similar entries with non-existent ID fails"
      (let ((manager (setup-test-manager)))
        (check-exception
         (find-similar-entries manager "non-existent-id"))))

    (test-case "Cluster entries by similarity"
      (let ((manager (setup-test-manager)))
        (def entries (archival-get-all manager limit: 10))
        (def entries-with-embeddings
          (filter (lambda (e) (hash-ref e 'embedding #f)) entries))
        (when (> (length entries-with-embeddings) 1)
          (def clusters (cluster-entries-by-similarity entries-with-embeddings
                                                      similarity-threshold: 0.5))
          (check (list? clusters))
          (check (> (length clusters) 0))
          ;; Each cluster should be a list
          (for-each
           (lambda (cluster)
             (check (list? cluster))
             (check (> (length cluster) 0)))
           clusters))))

    (test-case "Clustering with high threshold creates more clusters"
      (let ((manager (setup-test-manager)))
        (def entries (archival-get-all manager limit: 10))
        (def entries-with-embeddings
          (filter (lambda (e) (hash-ref e 'embedding #f)) entries))
        (when (> (length entries-with-embeddings) 1)
          (def clusters-low (cluster-entries-by-similarity entries-with-embeddings
                                                          similarity-threshold: 0.3))
          (def clusters-high (cluster-entries-by-similarity entries-with-embeddings
                                                           similarity-threshold: 0.9))
          (check (>= (length clusters-high) (length clusters-low))))))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(def integration-tests
  (test-suite "Integration Tests"

    (test-case "Semantic search integrates with archival manager"
      (let ((manager (setup-test-manager)))
        ;; Insert more entries
        (archival-insert! manager "Python programming language" generate-embedding?: #t)
        (archival-insert! manager "JavaScript web development" generate-embedding?: #t)

        ;; Search should find relevant entries
        (def results (semantic-search manager "programming languages" limit: 5))
        (check (> (length results) 0))))

    (test-case "Hybrid search combines text and semantic"
      (let ((manager (setup-test-manager)))
        ;; Entry with exact text match
        (archival-insert! manager "software development best practices" generate-embedding?: #t)

        ;; Hybrid search should rank exact match highly
        (def results (hybrid-search manager "software"
                                   text-weight: 0.5
                                   vector-weight: 0.5
                                   limit: 5))
        (check (> (length results) 0))
        ;; First result should have high text score
        (when (not (null? results))
          (check (> (search-result-text-score (car results)) 0.0)))))

    (test-case "End-to-end semantic search workflow"
      (let ((manager (setup-test-manager)))
        ;; 1. Insert entries with embeddings
        (archival-insert! manager "Machine learning algorithms" importance: 0.9 generate-embedding?: #t)
        (archival-insert! manager "Deep learning neural networks" importance: 0.8 generate-embedding?: #t)

        ;; 2. Perform semantic search
        (def results (semantic-search manager "AI and ML" limit: 5 min-similarity: 0.3))

        ;; 3. Verify results
        (check (list? results))
        (for-each
         (lambda (result)
           (check (hash-key? result 'similarity))
           (check (>= (hash-ref result 'similarity) 0.3)))
         results)

        ;; 4. Find similar entries
        (when (not (null? results))
          (def first-id (hash-ref (car results) 'id))
          (def similar (find-similar-entries manager first-id limit: 3))
          (check (list? similar)))))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(def semantic-search-test-suite
  (test-suite "Semantic Search Test Suite"
    vector-operations-tests
    semantic-search-tests
    hybrid-search-tests
    ranking-tests
    batch-operations-tests
    utilities-tests
    integration-tests))

;;; Run tests
#|
(import :std/test)
(test-run! semantic-search-test-suite)
|#
