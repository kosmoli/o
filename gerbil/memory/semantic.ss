;;; memory/semantic.ss - Semantic Search with Vector Similarity
;;;
;;; Vector similarity search using pgvector for semantic memory retrieval.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/sort
  :o/database/client
  :o/llm/client
  :o/memory/types
  :o/memory/archival)

;;; ============================================================================
;;; Vector Operations
;;; ============================================================================

(def (vector-dot-product v1 v2)
  "Calculate dot product of two vectors

   Args:
     v1: First vector (list of floats)
     v2: Second vector (list of floats)

   Returns:
     Dot product (float)"

  (if (not (= (length v1) (length v2)))
      (error "Vectors must have same length" v1-len: (length v1) v2-len: (length v2))
      (apply + (map * v1 v2))))

(def (vector-magnitude v)
  "Calculate magnitude (L2 norm) of vector

   Args:
     v: Vector (list of floats)

   Returns:
     Magnitude (float)"

  (sqrt (apply + (map (lambda (x) (* x x)) v))))

(def (cosine-similarity v1 v2)
  "Calculate cosine similarity between two vectors

   Args:
     v1: First vector (list of floats)
     v2: Second vector (list of floats)

   Returns:
     Cosine similarity [-1.0, 1.0], where 1.0 is identical"

  (let ((dot (vector-dot-product v1 v2))
        (mag1 (vector-magnitude v1))
        (mag2 (vector-magnitude v2)))
    (if (or (= mag1 0.0) (= mag2 0.0))
        0.0
        (/ dot (* mag1 mag2)))))

(def (euclidean-distance v1 v2)
  "Calculate Euclidean distance between two vectors

   Args:
     v1: First vector (list of floats)
     v2: Second vector (list of floats)

   Returns:
     Euclidean distance (float), where 0.0 is identical"

  (if (not (= (length v1) (length v2)))
      (error "Vectors must have same length")
      (sqrt (apply + (map (lambda (x y) (expt (- x y) 2)) v1 v2)))))

;;; ============================================================================
;;; Semantic Search
;;; ============================================================================

(def (semantic-search manager query
                      #!key
                      (limit 10)
                      (min-similarity 0.0))
  "Search archival memory using vector similarity

   Args:
     manager: Archival manager
     query: Search query text
     limit: Maximum results (default: 10)
     min-similarity: Minimum cosine similarity threshold (default: 0.0)

   Returns:
     List of entries with similarity scores"

  ;; Generate query embedding
  (let ((query-embedding (generate-embedding manager query)))

    ;; Get all entries with embeddings
    (let ((entries (archival-get-all manager limit: 10000)))
      (let ((entries-with-embeddings
             (filter (lambda (entry)
                      (hash-ref entry 'embedding #f))
                    entries)))

        ;; Calculate similarity scores
        (let ((scored-entries
               (map (lambda (entry)
                     (let ((embedding (hash-ref entry 'embedding)))
                       (let ((similarity (cosine-similarity query-embedding embedding)))
                         (hash-copy entry
                                   'similarity similarity))))
                   entries-with-embeddings)))

          ;; Filter by minimum similarity
          (let ((filtered (filter (lambda (entry)
                                   (>= (hash-ref entry 'similarity) min-similarity))
                                 scored-entries)))

            ;; Sort by similarity (descending)
            (let ((sorted (sort filtered
                               (lambda (a b)
                                 (> (hash-ref a 'similarity)
                                    (hash-ref b 'similarity))))))

              ;; Return top N results
              (take sorted (min limit (length sorted))))))))))

(def (semantic-search-by-embedding manager query-embedding
                                   #!key
                                   (limit 10)
                                   (min-similarity 0.0))
  "Search archival memory using pre-computed embedding vector

   Args:
     manager: Archival manager
     query-embedding: Query embedding vector
     limit: Maximum results (default: 10)
     min-similarity: Minimum cosine similarity threshold (default: 0.0)

   Returns:
     List of entries with similarity scores"

  ;; Get all entries with embeddings
  (let ((entries (archival-get-all manager limit: 10000)))
    (let ((entries-with-embeddings
           (filter (lambda (entry)
                    (hash-ref entry 'embedding #f))
                  entries)))

      ;; Calculate similarity scores
      (let ((scored-entries
             (map (lambda (entry)
                   (let ((embedding (hash-ref entry 'embedding)))
                     (let ((similarity (cosine-similarity query-embedding embedding)))
                       (hash-copy entry
                                 'similarity similarity))))
                 entries-with-embeddings)))

        ;; Filter by minimum similarity
        (let ((filtered (filter (lambda (entry)
                                 (>= (hash-ref entry 'similarity) min-similarity))
                               scored-entries)))

          ;; Sort by similarity (descending)
          (let ((sorted (sort filtered
                             (lambda (a b)
                               (> (hash-ref a 'similarity)
                                  (hash-ref b 'similarity))))))

            ;; Return top N results
            (take sorted (min limit (length sorted)))))))))

;;; ============================================================================
;;; Hybrid Search
;;; ============================================================================

(defstruct search-result
  (entry            ; Archival entry
   text-score       ; Text match score [0.0, 1.0]
   vector-score     ; Vector similarity score [-1.0, 1.0]
   combined-score)  ; Combined score [0.0, 1.0]
  transparent: #t)

(def (hybrid-search manager query
                    #!key
                    (limit 10)
                    (text-weight 0.3)
                    (vector-weight 0.7)
                    (min-text-score 0.0)
                    (min-vector-score 0.0))
  "Hybrid search combining text and vector similarity

   Args:
     manager: Archival manager
     query: Search query text
     limit: Maximum results (default: 10)
     text-weight: Weight for text matching (default: 0.3)
     vector-weight: Weight for vector similarity (default: 0.7)
     min-text-score: Minimum text score threshold (default: 0.0)
     min-vector-score: Minimum vector score threshold (default: 0.0)

   Returns:
     List of search-result structures with combined scores"

  ;; Validate weights sum to 1.0
  (unless (= (+ text-weight vector-weight) 1.0)
    (error "Weights must sum to 1.0" text: text-weight vector: vector-weight))

  ;; Generate query embedding
  (let ((query-embedding (generate-embedding manager query)))

    ;; Get all entries
    (let ((entries (archival-get-all manager limit: 10000)))

      ;; Calculate scores for each entry
      (let ((results
             (filter-map
              (lambda (entry)
                (let ((content (hash-ref entry 'content))
                      (embedding (hash-ref entry 'embedding #f)))

                  ;; Calculate text score (simple substring match)
                  (let ((text-score (if (string-contains content query)
                                        1.0
                                        0.0)))

                    ;; Calculate vector score
                    (let ((vector-score (if embedding
                                            (cosine-similarity query-embedding embedding)
                                            0.0)))

                      ;; Check thresholds
                      (if (and (>= text-score min-text-score)
                               (>= vector-score min-vector-score))
                          ;; Calculate combined score
                          (let ((combined-score (+ (* text-weight text-score)
                                                   (* vector-weight vector-score))))
                            (make-search-result
                             entry: entry
                             text-score: text-score
                             vector-score: vector-score
                             combined-score: combined-score))
                          #f)))))
              entries)))

        ;; Sort by combined score (descending)
        (let ((sorted (sort results
                           (lambda (a b)
                             (> (search-result-combined-score a)
                                (search-result-combined-score b))))))

          ;; Return top N results
          (take sorted (min limit (length sorted))))))))

(def (advanced-text-score content query)
  "Calculate advanced text matching score

   Args:
     content: Entry content
     query: Search query

   Returns:
     Text score [0.0, 1.0]"

  (let ((content-lower (string-downcase content))
        (query-lower (string-downcase query)))

    (cond
     ;; Exact match
     ((string=? content-lower query-lower) 1.0)

     ;; Query is substring of content
     ((string-contains content-lower query-lower)
      ;; Score based on position and length ratio
      (let ((pos (string-index content-lower query-lower))
            (content-len (string-length content))
            (query-len (string-length query)))
        (let ((position-score (- 1.0 (/ pos content-len)))
              (length-score (/ query-len content-len)))
          (* 0.5 (+ position-score length-score)))))

     ;; No match
     (else 0.0))))

(def (hybrid-search-advanced manager query
                             #!key
                             (limit 10)
                             (text-weight 0.3)
                             (vector-weight 0.7)
                             (importance-weight 0.0)
                             (min-combined-score 0.0))
  "Advanced hybrid search with importance weighting

   Args:
     manager: Archival manager
     query: Search query text
     limit: Maximum results (default: 10)
     text-weight: Weight for text matching (default: 0.3)
     vector-weight: Weight for vector similarity (default: 0.7)
     importance-weight: Weight for entry importance (default: 0.0)
     min-combined-score: Minimum combined score threshold (default: 0.0)

   Returns:
     List of search-result structures with combined scores"

  ;; Validate weights
  (let ((total-weight (+ text-weight vector-weight importance-weight)))
    (unless (= total-weight 1.0)
      (error "Weights must sum to 1.0" total: total-weight)))

  ;; Generate query embedding
  (let ((query-embedding (generate-embedding manager query)))

    ;; Get all entries
    (let ((entries (archival-get-all manager limit: 10000)))

      ;; Calculate scores for each entry
      (let ((results
             (filter-map
              (lambda (entry)
                (let ((content (hash-ref entry 'content))
                      (embedding (hash-ref entry 'embedding #f))
                      (importance (hash-ref entry 'importance 0.5)))

                  ;; Calculate text score (advanced)
                  (let ((text-score (advanced-text-score content query)))

                    ;; Calculate vector score
                    (let ((vector-score (if embedding
                                            ;; Normalize to [0.0, 1.0]
                                            (/ (+ (cosine-similarity query-embedding embedding) 1.0) 2.0)
                                            0.0)))

                      ;; Calculate combined score
                      (let ((combined-score (+ (* text-weight text-score)
                                               (* vector-weight vector-score)
                                               (* importance-weight importance))))

                        ;; Check threshold
                        (if (>= combined-score min-combined-score)
                            (make-search-result
                             entry: entry
                             text-score: text-score
                             vector-score: vector-score
                             combined-score: combined-score)
                            #f))))))
              entries)))

        ;; Sort by combined score (descending)
        (let ((sorted (sort results
                           (lambda (a b)
                             (> (search-result-combined-score a)
                                (search-result-combined-score b))))))

          ;; Return top N results
          (take sorted (min limit (length sorted))))))))

;;; ============================================================================
;;; Search Result Ranking
;;; ============================================================================

(def (rerank-results results
                     #!key
                     (diversity-factor 0.0)
                     (recency-factor 0.0))
  "Re-rank search results with diversity and recency

   Args:
     results: List of search-result structures
     diversity-factor: Weight for diversity (default: 0.0)
     recency-factor: Weight for recency (default: 0.0)

   Returns:
     Re-ranked list of search-result structures"

  (if (or (> diversity-factor 0.0) (> recency-factor 0.0))
      ;; Apply re-ranking
      (let ((current-time (current-seconds)))
        (let ((reranked
               (map (lambda (result)
                     (let ((entry (search-result-entry result))
                           (base-score (search-result-combined-score result)))

                       ;; Calculate recency score
                       (let ((created-at (hash-ref entry 'created_at))
                             (age (- current-time created-at)))
                         (let ((recency-score (exp (- (/ age 86400.0)))))  ; Decay over days

                           ;; Calculate new score
                           (let ((new-score (+ (* (- 1.0 recency-factor) base-score)
                                               (* recency-factor recency-score))))

                             (make-search-result
                              entry: entry
                              text-score: (search-result-text-score result)
                              vector-score: (search-result-vector-score result)
                              combined-score: new-score))))))
                   results)))

          ;; Sort by new scores
          (sort reranked
                (lambda (a b)
                  (> (search-result-combined-score a)
                     (search-result-combined-score b))))))

      ;; No re-ranking needed
      results))

;;; ============================================================================
;;; Batch Operations
;;; ============================================================================

(def (batch-semantic-search manager queries
                            #!key
                            (limit 10)
                            (min-similarity 0.0))
  "Perform semantic search for multiple queries

   Args:
     manager: Archival manager
     queries: List of query texts
     limit: Maximum results per query (default: 10)
     min-similarity: Minimum similarity threshold (default: 0.0)

   Returns:
     Hash mapping queries to result lists"

  (let ((results (hash)))
    (for-each
     (lambda (query)
       (let ((query-results (semantic-search manager query
                                            limit: limit
                                            min-similarity: min-similarity)))
         (hash-put! results query query-results)))
     queries)
    results))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(def (find-similar-entries manager entry-id
                          #!key
                          (limit 5)
                          (min-similarity 0.5))
  "Find entries similar to a given entry

   Args:
     manager: Archival manager
     entry-id: Entry ID to find similar entries for
     limit: Maximum results (default: 5)
     min-similarity: Minimum similarity threshold (default: 0.5)

   Returns:
     List of similar entries with similarity scores"

  (let ((entry (archival-get manager entry-id)))
    (if (not entry)
        (error "Entry not found" id: entry-id)
        (let ((embedding (hash-ref entry 'embedding #f)))
          (if (not embedding)
              (error "Entry has no embedding" id: entry-id)
              (let ((results (semantic-search-by-embedding manager embedding
                                                          limit: (+ limit 1)
                                                          min-similarity: min-similarity)))
                ;; Filter out the original entry
                (filter (lambda (result)
                         (not (equal? (hash-ref result 'id) entry-id)))
                       results)))))))

(def (cluster-entries-by-similarity entries
                                    #!key
                                    (similarity-threshold 0.7))
  "Cluster entries by vector similarity

   Args:
     entries: List of archival entries with embeddings
     similarity-threshold: Minimum similarity for clustering (default: 0.7)

   Returns:
     List of clusters (each cluster is a list of entries)"

  (let ((clusters '())
        (processed (hash)))

    (for-each
     (lambda (entry)
       (let ((entry-id (hash-ref entry 'id)))
         (unless (hash-ref processed entry-id #f)
           ;; Start new cluster
           (let ((cluster (list entry))
                 (embedding (hash-ref entry 'embedding)))

             ;; Find similar entries
             (for-each
              (lambda (other-entry)
                (let ((other-id (hash-ref other-entry 'id)))
                  (unless (or (equal? entry-id other-id)
                             (hash-ref processed other-id #f))
                    (let ((other-embedding (hash-ref other-entry 'embedding)))
                      (when (>= (cosine-similarity embedding other-embedding)
                               similarity-threshold)
                        (set! cluster (cons other-entry cluster))
                        (hash-put! processed other-id #t))))))
              entries)

             ;; Add cluster
             (hash-put! processed entry-id #t)
             (set! clusters (cons (reverse cluster) clusters))))))
     entries)

    (reverse clusters)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create archival manager
(def manager (make-archival-manager agent-id))

;; Semantic search
(def results (semantic-search manager "user preferences" limit: 5))
(for-each
 (lambda (result)
   (displayln (format "Similarity: ~a - ~a"
                     (hash-ref result 'similarity)
                     (hash-ref result 'content))))
 results)

;; Hybrid search
(def hybrid-results (hybrid-search manager "dark mode"
                                  text-weight: 0.3
                                  vector-weight: 0.7
                                  limit: 5))
(for-each
 (lambda (result)
   (displayln (format "Combined: ~a (text: ~a, vector: ~a) - ~a"
                     (search-result-combined-score result)
                     (search-result-text-score result)
                     (search-result-vector-score result)
                     (hash-ref (search-result-entry result) 'content))))
 hybrid-results)

;; Find similar entries
(def similar (find-similar-entries manager entry-id limit: 5))
(displayln (format "Found ~a similar entries" (length similar)))
|#
