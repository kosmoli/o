#lang racket

;;; memory/semantic.rkt - Semantic Search with Vector Similarity
;;;
;;; Vector similarity search for semantic memory retrieval.

(provide vector-dot-product
         vector-magnitude
         cosine-similarity
         euclidean-distance
         semantic-search
         semantic-search-by-embedding
         (struct-out search-result)
         hybrid-search
         advanced-text-score
         hybrid-search-advanced
         rerank-results
         batch-semantic-search
         find-similar-entries
         cluster-entries-by-similarity
         batch-embeddings
         compute-similarity-matrix
         find-similar-vectors)

(require racket/hash
        racket/format
        racket/list
        racket/math
        racket/string
        racket/match
        "../database/client.rkt"
        "../llm/client.rkt"
        "./types.rkt"
        "./archival.rkt")

;;; ============================================================================
;;; Vector Operations
;;; ============================================================================

(define (vector-dot-product v1 v2)
  "Calculate dot product of two vectors

   Args:
     v1: First vector (list of floats)
     v2: Second vector (list of floats)

   Returns:
     Dot product (float)"

  (if (not (= (length v1) (length v2)))
      (error 'vector-dot-product
             "Vectors must have same length"
             (list 'v1-len (length v1) 'v2-len (length v2)))
      (apply + (map * v1 v2))))

(define (vector-magnitude v)
  "Calculate magnitude (L2 norm) of vector

   Args:
     v: Vector (list of floats)

   Returns:
     Magnitude (float)"

  (sqrt (apply + (map (lambda (x) (* x x)) v))))

(define (cosine-similarity v1 v2)
  "Calculate cosine similarity between two vectors

   Args:
     v1: First vector (list of floats)
     v2: Second vector (list of floats)

   Returns:
     Cosine similarity [-1.0, 1.0], where 1.0 is identical"

  (let ([dot (vector-dot-product v1 v2)]
        [mag1 (vector-magnitude v1)]
        [mag2 (vector-magnitude v2)])
    (if (or (= mag1 0.0) (= mag2 0.0))
        0.0
        (/ dot (* mag1 mag2)))))

(define (euclidean-distance v1 v2)
  "Calculate Euclidean distance between two vectors

   Args:
     v1: First vector (list of floats)
     v2: Second vector (list of floats)

   Returns:
     Euclidean distance (float), where 0.0 is identical"

  (if (not (= (length v1) (length v2)))
      (error 'euclidean-distance "Vectors must have same length")
      (sqrt (apply + (map (lambda (x y) (expt (- x y) 2)) v1 v2)))))

;;; ============================================================================
;;; Semantic Search
;;; ============================================================================

(define (semantic-search manager query
                         #:limit [limit 10]
                         #:min-similarity [min-similarity 0.0])
  "Search archival memory using vector similarity

   Args:
     manager: Archival manager
     query: Search query text
     limit: Maximum results (default: 10)
     min-similarity: Minimum cosine similarity threshold (default: 0.0)

   Returns:
     List of entries with similarity scores"

  ;; Generate query embedding
  (define query-embedding (generate-embedding manager query))

  ;; Get all entries with embeddings
  (define entries (archival-get-all manager #:limit 10000))
  (define entries-with-embeddings
    (filter (lambda (entry)
              (hash-ref entry 'embedding #f))
            entries))

  ;; Calculate similarity scores
  (define scored-entries
    (map (lambda (entry)
           (define embedding (hash-ref entry 'embedding))
           (define similarity (cosine-similarity query-embedding embedding))
           (hash-set entry 'similarity similarity))
         entries-with-embeddings))

  ;; Filter by minimum similarity
  (define filtered
    (filter (lambda (entry)
              (>= (hash-ref entry 'similarity) min-similarity))
            scored-entries))

  ;; Sort by similarity (descending)
  (define sorted
    (sort filtered >
          #:key (lambda (entry)
                  (hash-ref entry 'similarity))))

  ;; Return top N results
  (take sorted (min limit (length sorted))))

(define (semantic-search-by-embedding manager query-embedding
                                       #:limit [limit 10]
                                       #:min-similarity [min-similarity 0.0])
  "Search archival memory using pre-computed embedding vector

   Args:
     manager: Archival manager
     query-embedding: Query embedding vector
     limit: Maximum results (default: 10)
     min-similarity: Minimum cosine similarity threshold (default: 0.0)

   Returns:
     List of entries with similarity scores"

  ;; Get all entries with embeddings
  (define entries (archival-get-all manager #:limit 10000))
  (define entries-with-embeddings
    (filter (lambda (entry)
              (hash-ref entry 'embedding #f))
            entries))

  ;; Calculate similarity scores
  (define scored-entries
    (map (lambda (entry)
           (define embedding (hash-ref entry 'embedding))
           (define similarity (cosine-similarity query-embedding embedding))
           (hash-set entry 'similarity similarity))
         entries-with-embeddings))

  ;; Filter by minimum similarity
  (define filtered
    (filter (lambda (entry)
              (>= (hash-ref entry 'similarity) min-similarity))
            scored-entries))

  ;; Sort by similarity (descending)
  (define sorted
    (sort filtered >
          #:key (lambda (entry)
                  (hash-ref entry 'similarity))))

  ;; Return top N results
  (take sorted (min limit (length sorted))))

;;; ============================================================================
;;; Hybrid Search
;;; ============================================================================

(struct search-result
  (entry            ; Archival entry
   text-score       ; Text match score [0.0, 1.0]
   vector-score     ; Vector similarity score [-1.0, 1.0]
   combined-score)  ; Combined score [0.0, 1.0]
  #:transparent)

(define (hybrid-search manager query
                       #:limit [limit 10]
                       #:text-weight [text-weight 0.3]
                       #:vector-weight [vector-weight 0.7]
                       #:min-text-score [min-text-score 0.0]
                       #:min-vector-score [min-vector-score 0.0])
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
    (error 'hybrid-search
           "Weights must sum to 1.0"
           (list 'text text-weight 'vector vector-weight)))

  ;; Generate query embedding
  (define query-embedding (generate-embedding manager query))

  ;; Get all entries
  (define entries (archival-get-all manager #:limit 10000))

  ;; Calculate scores for each entry
  (define results
    (filter-map
     (lambda (entry)
       (define content (hash-ref entry 'content))
       (define embedding (hash-ref entry 'embedding #f))

       ;; Calculate text score (simple substring match)
       (define text-score
         (if (string-contains? content query)
             1.0
             0.0))

       ;; Calculate vector score
       (define vector-score
         (if embedding
             (cosine-similarity query-embedding embedding)
             0.0))

       ;; Check thresholds
       (if (and (>= text-score min-text-score)
                (>= vector-score min-vector-score))
           ;; Calculate combined score
           (let ([combined-score (+ (* text-weight text-score)
                                    (* vector-weight vector-score))])
             (search-result entry text-score vector-score combined-score))
           #f))
     entries))

  ;; Sort by combined score (descending)
  (define sorted
    (sort results >
          #:key search-result-combined-score))

  ;; Return top N results
  (take sorted (min limit (length sorted))))

(define (advanced-text-score content query)
  "Calculate advanced text matching score

   Args:
     content: Entry content
     query: Search query

   Returns:
     Text score [0.0, 1.0]"

  (define content-lower (string-downcase content))
  (define query-lower (string-downcase query))

  (cond
   ;; Exact match
   [(string=? content-lower query-lower) 1.0]

   ;; Query is substring of content
   [(string-contains? content-lower query-lower)
    ;; Score based on position and length ratio
    (define pos (substring-index content-lower query-lower))
    (define content-len (string-length content))
    (define query-len (string-length query))
    (define position-score (- 1.0 (/ pos content-len)))
    (define length-score (/ query-len content-len))
    (* 0.5 (+ position-score length-score))]

   ;; No match
   [else 0.0]))

(define (hybrid-search-advanced manager query
                                #:limit [limit 10]
                                #:text-weight [text-weight 0.3]
                                #:vector-weight [vector-weight 0.7]
                                #:importance-weight [importance-weight 0.0]
                                #:min-combined-score [min-combined-score 0.0])
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
  (define total-weight (+ text-weight vector-weight importance-weight))
  (unless (= total-weight 1.0)
    (error 'hybrid-search-advanced
           "Weights must sum to 1.0"
           (list 'total total-weight)))

  ;; Generate query embedding
  (define query-embedding (generate-embedding manager query))

  ;; Get all entries
  (define entries (archival-get-all manager #:limit 10000))

  ;; Calculate scores for each entry
  (define results
    (filter-map
     (lambda (entry)
       (define content (hash-ref entry 'content))
       (define embedding (hash-ref entry 'embedding #f))
       (define importance (hash-ref entry 'importance 0.5))

       ;; Calculate text score (advanced)
       (define text-score (advanced-text-score content query))

       ;; Calculate vector score (normalize to [0.0, 1.0])
       (define vector-score
         (if embedding
             (/ (+ (cosine-similarity query-embedding embedding) 1.0) 2.0)
             0.0))

       ;; Calculate combined score
       (define combined-score
         (+ (* text-weight text-score)
            (* vector-weight vector-score)
            (* importance-weight importance)))

       ;; Check threshold
       (if (>= combined-score min-combined-score)
           (search-result entry text-score vector-score combined-score)
           #f))
     entries))

  ;; Sort by combined score (descending)
  (define sorted
    (sort results >
          #:key search-result-combined-score))

  ;; Return top N results
  (take sorted (min limit (length sorted))))

;;; ============================================================================
;;; Search Result Ranking
;;; ============================================================================

(define (rerank-results results
                       #:diversity-factor [diversity-factor 0.0]
                       #:recency-factor [recency-factor 0.0])
  "Re-rank search results with diversity and recency

   Args:
     results: List of search-result structures
     diversity-factor: Weight for diversity (default: 0.0)
     recency-factor: Weight for recency (default: 0.0)

   Returns:
     Re-ranked list of search-result structures"

  (if (or (> diversity-factor 0.0) (> recency-factor 0.0))
      ;; Apply re-ranking
      (let ([current-time (current-seconds)])
        (define reranked
          (map (lambda (result)
                 (define entry (search-result-entry result))
                 (define base-score (search-result-combined-score result))

                 ;; Calculate recency score
                 (define created-at (hash-ref entry 'created_at))
                 (define age (- current-time created-at))
                 (define recency-score (exp (- (/ age 86400.0))))  ; Decay over days

                 ;; Calculate new score
                 (define new-score
                   (+ (* (- 1.0 recency-factor) base-score)
                      (* recency-factor recency-score)))

                 (search-result entry
                                (search-result-text-score result)
                                (search-result-vector-score result)
                                new-score))
               results))

        ;; Sort by new scores
        (sort reranked >
              #:key search-result-combined-score))

      ;; No re-ranking needed
      results))

;;; ============================================================================
;;; Batch Operations
;;; ============================================================================

(define (batch-semantic-search manager queries
                                #:limit [limit 10]
                                #:min-similarity [min-similarity 0.0])
  "Perform semantic search for multiple queries

   Args:
     manager: Archival manager
     queries: List of query texts
     limit: Maximum results per query (default: 10)
     min-similarity: Minimum similarity threshold (default: 0.0)

   Returns:
     Hash mapping queries to result lists"

  (define results (make-hash))
  (for ([query (in-list queries)])
    (define query-results
      (semantic-search manager query
                       #:limit limit
                       #:min-similarity min-similarity))
    (hash-set! results query query-results))
  results)

(define (batch-embeddings manager texts)
  "Generate embeddings for multiple texts

   Args:
     manager: Archival manager
     texts: List of text strings

   Returns:
     List of embedding vectors"

  (map (lambda (text)
         (generate-embedding manager text))
       texts))

(define (compute-similarity-matrix vectors)
  "Compute pairwise similarity matrix for vectors

   Args:
     vectors: List of embedding vectors

   Returns:
     Matrix (list of lists) of similarity scores"

  (define (compute-row v1)
    (map (lambda (v2)
           (cosine-similarity v1 v2))
         vectors))
  (map compute-row vectors))

(define (find-similar-vectors target-vectors candidates
                             #:top-k [top-k 5]
                             #:min-similarity [min-similarity 0.7])
  "Find most similar vectors for each target

   Args:
     target-vectors: List of target vectors to find matches for
     candidates: Pool of candidate vectors
     top-k: Number of top matches to return per target
     min-similarity: Minimum similarity threshold

   Returns:
     List of lists of (vector-index . similarity) pairs"

  (define (find-matches-for-target target)
    (define similarities
      (for/list ([candidate (in-list candidates)]
                 [i (in-naturals)])
        (cons i (cosine-similarity target candidate))))
    (define filtered
      (filter (lambda (pair)
                (>= (cdr pair) min-similarity))
              similarities))
    (define sorted
      (sort filtered >
            #:key cdr))
    (take sorted (min top-k (length sorted))))

  (map find-matches-for-target target-vectors))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(define (find-similar-entries manager entry-id
                             #:limit [limit 5]
                             #:min-similarity [min-similarity 0.5])
  "Find entries similar to a given entry

   Args:
     manager: Archival manager
     entry-id: Entry ID to find similar entries for
     limit: Maximum results (default: 5)
     min-similarity: Minimum similarity threshold (default: 0.5)

   Returns:
     List of similar entries with similarity scores"

  (define entry (archival-get manager entry-id))
  (if (not entry)
      (error 'find-similar-entries
             "Entry not found"
             (list 'id entry-id))
      (let ([embedding (hash-ref entry 'embedding #f)])
        (if (not embedding)
            (error 'find-similar-entries
                   "Entry has no embedding"
                   (list 'id entry-id))
            (let ([results (semantic-search-by-embedding manager embedding
                                                         #:limit (+ limit 1)
                                                         #:min-similarity min-similarity)])
              ;; Filter out the original entry
              (filter (lambda (result)
                       (not (equal? (hash-ref result 'id) entry-id)))
                      results))))))

(define (cluster-entries-by-similarity entries
                                      #:similarity-threshold [similarity-threshold 0.7])
  "Cluster entries by vector similarity

   Args:
     entries: List of archival entries with embeddings
     similarity-threshold: Minimum similarity for clustering (default: 0.7)

   Returns:
     List of clusters (each cluster is a list of entries)"

  (define clusters '())
  (define processed (make-hash))

  (for ([entry (in-list entries)])
    (define entry-id (hash-ref entry 'id))
    (unless (hash-ref processed entry-id #f)
      ;; Start new cluster
      (define cluster (list entry))
      (define embedding (hash-ref entry 'embedding))

      ;; Find similar entries
      (for ([other-entry (in-list entries)])
        (define other-id (hash-ref other-entry 'id))
        (unless (or (equal? entry-id other-id)
                    (hash-ref processed other-id #f))
          (define other-embedding (hash-ref other-entry 'embedding))
          (when (>= (cosine-similarity embedding other-embedding)
                    similarity-threshold)
            (set! cluster (cons other-entry cluster))
            (hash-set! processed other-id #t))))

      ;; Add cluster
      (hash-set! processed entry-id #t)
      (set! clusters (cons (reverse cluster) clusters))))

  (reverse clusters))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

;; String contains check
(define (string-contains? str substr)
  (not (not (substring-contains? str substr))))

;; Substring index (returns 0-based index or #f)
(define (substring-index str substr)
  (for/first ([i (in-range (add1 (- (string-length str) (string-length substr))))]
              #:when (substring=? str i (+ i (string-length substr)) substr))
    i))

;; Substring contains check
(define (substring-contains? str substr)
  (for/or ([i (in-range (add1 (- (string-length str) (string-length substr))))])
    (substring=? str i (+ i (string-length substr)) substr)))

;; Substring equality check at position
(define (substring=? str start end substr)
  (string=? (substring str start end) substr))
