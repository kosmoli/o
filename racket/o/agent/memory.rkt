#lang racket

;;; agent/memory.rkt - Agent Memory System
;;;
;;; This module implements the agent's memory system with support for:
;;; - Short-term and long-term memory
;;; - Memory blocks with metadata
;;; - Vector embeddings for semantic search (placeholder)
;;; - Memory consolidation and pruning
;;; - Persistent storage integration

(provide (struct-out agent-memory-system)
         (struct-out memory-block-internal)
         default-memory-config
         make-agent-memory-instance
         make-memory-block-instance
         add-to-short-term!
         get-short-term-memory
         clear-short-term!
         add-to-long-term!
         get-from-long-term
         remove-from-long-term!
         add-to-working!
         get-working-memory
         clear-working!
         consolidate-memory!
         search-memory
         semantic-search
         generate-embedding
         sort-memory-blocks
         memory-block-score
         prune-memory!
         memory-stats
         serialize-memory
         serialize-memory-block
         deserialize-memory
         deserialize-memory-block)

(require racket/hash
        racket/format
        racket/list
        racket/date
        "../elixir-bridge.rkt")

;;; ============================================================================
;;; Memory Structure
;;; ============================================================================

(struct agent-memory-system
  (id                ; Unique memory identifier
   short-term        ; Short-term memory (recent, limited capacity)
   long-term         ; Long-term memory (persistent, unlimited)
   working           ; Working memory (current context)
   episodic          ; Episodic memory (experiences)
   semantic          ; Semantic memory (facts and knowledge)
   procedural        ; Procedural memory (skills and procedures)
   metadata          ; Memory metadata
   config            ; Memory configuration
   created-at        ; Creation timestamp
   updated-at)       ; Last update timestamp
  #:transparent
  #:mutable)

;;; ============================================================================
;;; Memory Block Structure
;;; ============================================================================

(struct memory-block-internal
  (id                ; Unique block identifier
   type              ; Block type: 'episodic 'semantic 'procedural
   content           ; Block content (any data)
   embedding         ; Vector embedding (for semantic search)
   importance        ; Importance score (0.0 - 1.0)
   access-count      ; Number of times accessed
   last-accessed     ; Last access timestamp
   tags              ; Tags for categorization
   metadata          ; Additional metadata
   created-at)       ; Creation timestamp
  #:transparent
  #:mutable)

;;; ============================================================================
;;; Memory Configuration
;;; ============================================================================

(define default-memory-config
  (hash
   'short-term-capacity 100        ; Max items in short-term memory
   'working-memory-capacity 10     ; Max items in working memory
   'importance-threshold 0.5       ; Min importance for long-term storage
   'consolidation-interval 300     ; Consolidation interval (seconds)
   'embedding-dimensions 1536      ; Vector embedding dimensions
   'enable-embeddings #f))         ; Enable vector embeddings (requires external service)

;;; ============================================================================
;;; Memory Creation
;;; ============================================================================

(define (make-agent-memory-instance [config default-memory-config])
  "Create a new agent memory instance"
  (define now (current-seconds))
  (agent-memory-system
   (uuid-generate)
   '()
   (hash)  ; Hash table for efficient lookup
   '()
   '()
   (hash)
   (hash)
   (hash)
   config
   now
   now))

;;; ============================================================================
;;; Memory Block Creation
;;; ============================================================================

(define (make-memory-block-instance type content
                                   #:importance [importance 0.5]
                                   #:tags [tags '()]
                                   #:metadata [metadata (hash)]
                                   #:embedding [embedding #f])
  "Create a new memory block"
  (memory-block-internal
   (uuid-generate)
   type
   content
   embedding
   importance
   0
   (current-seconds)
   tags
   metadata
   (current-seconds)))

;;; ============================================================================
;;; Short-Term Memory Operations
;;; ============================================================================

(define (add-to-short-term! memory block)
  "Add a memory block to short-term memory"
  (define short-term (agent-memory-system-short-term memory))
  (define capacity (hash-ref (agent-memory-system-config memory) 'short-term-capacity))
  (define new-short-term (cons block short-term))

  ;; Limit capacity (FIFO)
  (set-agent-memory-system-short-term! memory (take new-short-term capacity))
  (set-agent-memory-system-updated-at! memory (current-seconds))

  ;; Log to WAL
  (elixir-wal-log! 'memory-add-short-term
                   (hash 'memory_id (agent-memory-system-id memory)
                         'block_id (memory-block-internal-id block)
                         'type (symbol->string (memory-block-internal-type block))))

  block)

(define (get-short-term-memory memory #:limit [limit 100])
  "Get short-term memory (most recent first)"
  (take (agent-memory-system-short-term memory) limit))

(define (clear-short-term! memory)
  "Clear short-term memory"
  (set-agent-memory-system-short-term! memory '())
  (set-agent-memory-system-updated-at! memory (current-seconds)))

;;; ============================================================================
;;; Long-Term Memory Operations
;;; ============================================================================

(define (add-to-long-term! memory block)
  "Add a memory block to long-term memory"
  (define block-id (memory-block-internal-id block))
  (define long-term (agent-memory-system-long-term memory))
  (set-agent-memory-system-long-term! memory (hash-set long-term block-id block))
  (set-agent-memory-system-updated-at! memory (current-seconds))

  ;; Also add to type-specific storage
  (case (memory-block-internal-type block)
    [(episodic)
     (define episodic (agent-memory-system-episodic memory))
     (set-agent-memory-system-episodic! memory (cons block episodic))]
    [(semantic)
     (define semantic (agent-memory-system-semantic memory))
     (set-agent-memory-system-semantic! memory (hash-set semantic block-id block))]
    [(procedural)
     (define procedural (agent-memory-system-procedural memory))
     (set-agent-memory-system-procedural! memory (hash-set procedural block-id block))])

  ;; Log to WAL
  (elixir-wal-log! 'memory-add-long-term
                   (hash 'memory_id (agent-memory-system-id memory)
                         'block_id block-id
                         'type (symbol->string (memory-block-internal-type block))
                         'importance (memory-block-internal-importance block)))

  block)

(define (get-from-long-term memory block-id)
  "Retrieve a memory block from long-term memory"
  (define block (hash-ref (agent-memory-system-long-term memory) block-id #f))
  (when block
    ;; Update access statistics
    (set-memory-block-internal-access-count! block
                                              (+ (memory-block-internal-access-count block) 1))
    (set-memory-block-internal-last-accessed! block (current-seconds)))
  block)

(define (remove-from-long-term! memory block-id)
  "Remove a memory block from long-term memory"
  (define block (hash-ref (agent-memory-system-long-term memory) block-id #f))
  (when block
    (define long-term (agent-memory-system-long-term memory))
    (set-agent-memory-system-long-term! memory (hash-remove long-term block-id))

    ;; Remove from type-specific storage
    (case (memory-block-internal-type block)
      [(episodic)
       (define episodic (agent-memory-system-episodic memory))
       (set-agent-memory-system-episodic!
        memory
        (filter (lambda (b) (not (equal? (memory-block-internal-id b) block-id)))
                episodic))]
      [(semantic)
       (define semantic (agent-memory-system-semantic memory))
       (set-agent-memory-system-semantic! memory (hash-remove semantic block-id))]
      [(procedural)
       (define procedural (agent-memory-system-procedural memory))
       (set-agent-memory-system-procedural! memory (hash-remove procedural block-id))])

    (set-agent-memory-system-updated-at! memory (current-seconds))

    ;; Log to WAL
    (elixir-wal-log! 'memory-remove-long-term
                     (hash 'memory_id (agent-memory-system-id memory)
                           'block_id block-id))))

;;; ============================================================================
;;; Working Memory Operations
;;; ============================================================================

(define (add-to-working! memory block)
  "Add a memory block to working memory"
  (define working (agent-memory-system-working memory))
  (define capacity (hash-ref (agent-memory-system-config memory) 'working-memory-capacity))
  (define new-working (cons block working))

  ;; Limit capacity (FIFO)
  (set-agent-memory-system-working! memory (take new-working capacity))
  (set-agent-memory-system-updated-at! memory (current-seconds))

  block)

(define (get-working-memory memory)
  "Get working memory"
  (agent-memory-system-working memory))

(define (clear-working! memory)
  "Clear working memory"
  (set-agent-memory-system-working! memory '())
  (set-agent-memory-system-updated-at! memory (current-seconds)))

;;; ============================================================================
;;; Memory Consolidation
;;; ============================================================================

(define (consolidate-memory! memory)
  "Consolidate short-term memory to long-term memory"
  (displayln "Consolidating memory...")

  (define short-term (agent-memory-system-short-term memory))
  (define threshold (hash-ref (agent-memory-system-config memory) 'importance-threshold))
  (define important-blocks
    (filter (lambda (block)
              (>= (memory-block-internal-importance block) threshold))
            short-term))

  ;; Move important blocks to long-term memory
  (for-each (lambda (block)
              (add-to-long-term! memory block))
            important-blocks)

  ;; Clear short-term memory
  (clear-short-term! memory)

  ;; Log consolidation
  (elixir-wal-log! 'memory-consolidation
                   (hash 'memory_id (agent-memory-system-id memory)
                         'num_consolidated (length important-blocks)
                         'timestamp (current-seconds)))

  (displayln (format "Consolidated ~a blocks to long-term memory"
                     (length important-blocks)))

  important-blocks)

;;; ============================================================================
;;; Memory Search
;;; ============================================================================

(define (search-memory memory query #:type [type #f] #:limit [limit 10])
  "Search memory by content (simple text search)"
  (define all-blocks
    (append (agent-memory-system-short-term memory)
            (hash-values (agent-memory-system-long-term memory))))
  (define filtered
    (if type
        (filter (lambda (b) (eq? (memory-block-internal-type b) type))
                all-blocks)
        all-blocks))
  (define matches
    (filter (lambda (block)
              (memory-block-matches? block query))
            filtered))

  ;; Sort by importance and recency
  (take (sort-memory-blocks matches) limit))

(define (memory-block-matches? block query)
  "Check if memory block matches query (simple substring search)"
  (define content (memory-block-internal-content block))
  (cond
   [(string? content)
    (string-contains? content query)]
   [(hash? content)
    (for/or ([(k v) (in-hash content)])
      (or (and (string? v) (string-contains? v query))
          (and (symbol? v) (string-contains? (symbol->string v) query))))]
   [else #f]))

;;; ============================================================================
;;; Semantic Search (Placeholder)
;;; ============================================================================

(define (semantic-search memory query-embedding #:limit [limit 10])
  "Search memory using vector similarity (placeholder)"
  ;; This is a placeholder. In a real implementation, this would:
  ;; 1. Generate embedding for query
  ;; 2. Compute cosine similarity with all memory block embeddings
  ;; 3. Return top-k most similar blocks

  (displayln "Semantic search not yet implemented (requires embedding service)")

  ;; For now, return empty list
  '())

(define (generate-embedding text)
  "Generate vector embedding for text (placeholder)"
  ;; This would call an external embedding service (OpenAI, etc.)
  ;; For now, return dummy embedding
  (make-vector 1536 0.0))

;;; ============================================================================
;;; Memory Sorting and Ranking
;;; ============================================================================

(define (sort-memory-blocks blocks)
  "Sort memory blocks by importance and recency"
  (sort blocks >
        #:key memory-block-score))

(define (memory-block-score block)
  "Calculate relevance score for memory block"
  (define importance (memory-block-internal-importance block))
  (define access-count (memory-block-internal-access-count block))
  (define recency (- (current-seconds) (memory-block-internal-last-accessed block)))
  (define recency-score (/ 1.0 (+ 1.0 (/ recency 86400.0))))  ; Decay over days

  ;; Weighted combination
  (+ (* 0.5 importance)
     (* 0.3 (min 1.0 (/ access-count 10.0)))
     (* 0.2 recency-score)))

;;; ============================================================================
;;; Memory Pruning
;;; ============================================================================

(define (prune-memory! memory #:min-importance [min-importance 0.3] #:max-age-days [max-age-days 30])
  "Prune low-importance and old memory blocks"
  (displayln "Pruning memory...")

  (define long-term (agent-memory-system-long-term memory))
  (define now (current-seconds))
  (define max-age-seconds (* max-age-days 86400))
  (define blocks-to-remove '())

  ;; Find blocks to remove
  (for ([(block-id block) (in-hash long-term)])
    (define age (- now (memory-block-internal-created-at block)))
    (define importance (memory-block-internal-importance block))
    (when (or (< importance min-importance)
              (> age max-age-seconds))
      (set! blocks-to-remove (cons block-id blocks-to-remove))))

  ;; Remove blocks
  (for-each (lambda (block-id)
              (remove-from-long-term! memory block-id))
            blocks-to-remove)

  (displayln (format "Pruned ~a blocks from memory" (length blocks-to-remove)))

  blocks-to-remove)

;;; ============================================================================
;;; Memory Statistics
;;; ============================================================================

(define (memory-stats memory)
  "Get memory statistics"
  (hash
   'id (agent-memory-system-id memory)
   'short_term_count (length (agent-memory-system-short-term memory))
   'long_term_count (hash-count (agent-memory-system-long-term memory))
   'working_count (length (agent-memory-system-working memory))
   'episodic_count (length (agent-memory-system-episodic memory))
   'semantic_count (hash-count (agent-memory-system-semantic memory))
   'procedural_count (hash-count (agent-memory-system-procedural memory))
   'total_blocks (+ (length (agent-memory-system-short-term memory))
                    (hash-count (agent-memory-system-long-term memory)))
   'created_at (agent-memory-system-created-at memory)
   'updated_at (agent-memory-system-updated-at memory)))

;;; ============================================================================
;;; Memory Serialization
;;; ============================================================================

(define (serialize-memory memory)
  "Serialize memory to hash for checkpointing"
  (hash
   'id (agent-memory-system-id memory)
   'short_term (map serialize-memory-block (agent-memory-system-short-term memory))
   'long_term (for/hash ([(k v) (in-hash (agent-memory-system-long-term memory))])
                 (values k (serialize-memory-block v)))
   'working (map serialize-memory-block (agent-memory-system-working memory))
   'episodic (map serialize-memory-block (agent-memory-system-episodic memory))
   'semantic (for/hash ([(k v) (in-hash (agent-memory-system-semantic memory))])
                 (values k (serialize-memory-block v)))
   'procedural (for/hash ([(k v) (in-hash (agent-memory-system-procedural memory))])
                   (values k (serialize-memory-block v)))
   'metadata (hash->list (agent-memory-system-metadata memory))
   'config (hash->list (agent-memory-system-config memory))
   'created_at (agent-memory-system-created-at memory)
   'updated_at (agent-memory-system-updated-at memory)))

(define (serialize-memory-block block)
  "Serialize memory block to hash"
  (hash
   'id (memory-block-internal-id block)
   'type (symbol->string (memory-block-internal-type block))
   'content (memory-block-internal-content block)
   'embedding (memory-block-internal-embedding block)
   'importance (memory-block-internal-importance block)
   'access_count (memory-block-internal-access-count block)
   'last_accessed (memory-block-internal-last-accessed block)
   'tags (memory-block-internal-tags block)
   'metadata (hash->list (memory-block-internal-metadata block))
   'created_at (memory-block-internal-created-at block)))

(define (deserialize-memory data)
  "Deserialize memory from hash"
  (agent-memory-system
   (hash-ref data 'id)
   (map deserialize-memory-block (hash-ref data 'short_term))
   (list->hash-helper (hash-ref data 'long_term))
   (map deserialize-memory-block (hash-ref data 'working))
   (map deserialize-memory-block (hash-ref data 'episodic))
   (list->hash-helper (hash-ref data 'semantic))
   (list->hash-helper (hash-ref data 'procedural))
   (list->hash-helper (hash-ref data 'metadata))
   (list->hash-helper (hash-ref data 'config))
   (hash-ref data 'created_at)
   (hash-ref data 'updated_at)))

(define (deserialize-memory-block data)
  "Deserialize memory block from hash"
  (memory-block-internal
   (hash-ref data 'id)
   (string->symbol (hash-ref data 'type))
   (hash-ref data 'content)
   (hash-ref data 'embedding)
   (hash-ref data 'importance)
   (hash-ref data 'access_count)
   (hash-ref data 'last_accessed)
   (hash-ref data 'tags)
   (list->hash-helper (hash-ref data 'metadata))
   (hash-ref data 'created_at)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (uuid-generate)
  "Generate UUID (simple implementation)"
  (format "~a-~a" (current-seconds) (random 1000000)))

(define (string-contains? str substr)
  "Check if string contains substring"
  (not (not (substring? substr str))))

(define (substring? substr str)
  "Check if substr is in str"
  (define len-str (string-length str))
  (define len-sub (string-length substr))
  (cond
   [(> len-sub len-str) #f]
   [(= len-sub 0) #t]
   [else
    (define (loop i)
      (cond
       [(> i (- len-str len-sub)) #f]
       [(string=? (substring str i (+ i len-sub)) substr) #t]
       [else (loop (+ i 1))]))
    (loop 0)]))

(define (list->hash-helper lst)
  "Convert list of pairs to hash"
  (for/hash ([pair (in-list lst)])
    (values (car pair) (cdr pair))))
