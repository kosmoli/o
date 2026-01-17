;;; agent/memory.ss - Agent Memory System
;;;
;;; This module implements the agent's memory system with support for:
;;; - Short-term and long-term memory
;;; - Memory blocks with metadata
;;; - Vector embeddings for semantic search (placeholder)
;;; - Memory consolidation and pruning
;;; - Persistent storage integration

(export #t)

(import
  :std/misc/threads
  :std/sugar
  :std/format
  :std/sort
  :std/srfi/1
  :std/srfi/13
  :std/misc/hash
  :std/misc/uuid
  :o/agent/elixir-bridge)

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

;;; Helper function to convert association list to hash table
(def (list->hash lst)
  "Convert association list to hash table"
  (let ((h (hash)))
    (for-each (lambda (pair)
                (hash-put! h (car pair) (cdr pair)))
              lst)
    h))

;;; ============================================================================
;;; Memory Structure
;;; ============================================================================

(defstruct agent-memory
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
  transparent: #t)

;;; ============================================================================
;;; Memory Block Structure
;;; ============================================================================

(defstruct memory-block
  (id                ; Unique block identifier
   type              ; Block type: :episodic :semantic :procedural
   content           ; Block content (any data)
   embedding         ; Vector embedding (for semantic search)
   importance        ; Importance score (0.0 - 1.0)
   access-count      ; Number of times accessed
   last-accessed     ; Last access timestamp
   tags              ; Tags for categorization
   metadata          ; Additional metadata
   created-at)       ; Creation timestamp
  transparent: #t)

;;; ============================================================================
;;; Memory Configuration
;;; ============================================================================

(def default-memory-config
  (hash
   ('short-term-capacity 100)        ; Max items in short-term memory
   ('working-memory-capacity 10)     ; Max items in working memory
   ('importance-threshold 0.5)       ; Min importance for long-term storage
   ('consolidation-interval 300)     ; Consolidation interval (seconds)
   ('embedding-dimensions 1536)      ; Vector embedding dimensions
   ('enable-embeddings #f)))         ; Enable vector embeddings (requires external service)

;;; ============================================================================
;;; Memory Creation
;;; ============================================================================

(def (make-agent-memory-instance (config default-memory-config))
  "Create a new agent memory instance"
  (let ((now (time->seconds (current-time))))
    (make-agent-memory
     (uuid->string (make-uuid))
     '()
     (hash)
     '()
     '()
     (hash)
     (hash)
     (hash)
     config
     now
     now)))

;;; ============================================================================
;;; Memory Block Creation
;;; ============================================================================

(def (make-memory-block-instance type content
                                 importance: (importance 0.5)
                                 tags: (tags '())
                                 metadata: (metadata (hash))
                                 embedding: (embedding #f))
  "Create a new memory block"
  (make-memory-block
   id: (uuid->string (make-uuid))
   type: type
   content: content
   embedding: embedding
   importance: importance
   access-count: 0
   last-accessed: (time->seconds (current-time))
   tags: tags
   metadata: metadata
   created-at: (time->seconds (current-time))))

;;; ============================================================================
;;; Short-Term Memory Operations
;;; ============================================================================

(def (add-to-short-term! memory block)
  "Add a memory block to short-term memory"
  (let* ((short-term (agent-memory-short-term memory))
         (capacity (hash-ref (agent-memory-config memory) 'short-term-capacity))
         (new-short-term (cons block short-term)))

    ;; Limit capacity (FIFO)
    (set! (agent-memory-short-term memory)
          (take new-short-term capacity))

    (set! (agent-memory-updated-at memory) (time->seconds (current-time)))

    ;; Log to WAL
    (elixir-wal-log! 'memory-add-short-term
                     (hash ('memory_id (agent-memory-id memory))
                           ('block_id (memory-block-id block))
                           ('type (symbol->string (memory-block-type block)))))

    block))

(def (get-short-term-memory memory (limit 100))
  "Get short-term memory (most recent first)"
  (take (agent-memory-short-term memory) limit))

(def (clear-short-term! memory)
  "Clear short-term memory"
  (set! (agent-memory-short-term memory) '())
  (set! (agent-memory-updated-at memory) (time->seconds (current-time))))

;;; ============================================================================
;;; Long-Term Memory Operations
;;; ============================================================================

(def (add-to-long-term! memory block)
  "Add a memory block to long-term memory"
  (let ((block-id (memory-block-id block)))
    (hash-put! (agent-memory-long-term memory) block-id block)
    (set! (agent-memory-updated-at memory) (time->seconds (current-time)))

    ;; Also add to type-specific storage
    (case (memory-block-type block)
      ((:episodic)
       (set! (agent-memory-episodic memory)
             (cons block (agent-memory-episodic memory))))
      ((:semantic)
       (hash-put! (agent-memory-semantic memory) block-id block))
      ((:procedural)
       (hash-put! (agent-memory-procedural memory) block-id block)))

    ;; Log to WAL
    (elixir-wal-log! 'memory-add-long-term
                     (hash ('memory_id (agent-memory-id memory))
                           ('block_id block-id)
                           ('type (symbol->string (memory-block-type block)))
                           ('importance (memory-block-importance block))))

    block))

(def (get-from-long-term memory block-id)
  "Retrieve a memory block from long-term memory"
  (let ((block (hash-ref (agent-memory-long-term memory) block-id #f)))
    (when block
      ;; Update access statistics
      (set! (memory-block-access-count block)
            (+ (memory-block-access-count block) 1))
      (set! (memory-block-last-accessed block)
            (time->seconds (current-time))))
    block))

(def (remove-from-long-term! memory block-id)
  "Remove a memory block from long-term memory"
  (let ((block (hash-ref (agent-memory-long-term memory) block-id #f)))
    (when block
      (hash-remove! (agent-memory-long-term memory) block-id)

      ;; Remove from type-specific storage
      (case (memory-block-type block)
        ((:episodic)
         (set! (agent-memory-episodic memory)
               (filter (lambda (b) (not (equal? (memory-block-id b) block-id)))
                       (agent-memory-episodic memory))))
        ((:semantic)
         (hash-remove! (agent-memory-semantic memory) block-id))
        ((:procedural)
         (hash-remove! (agent-memory-procedural memory) block-id)))

      (set! (agent-memory-updated-at memory) (time->seconds (current-time)))

      ;; Log to WAL
      (elixir-wal-log! 'memory-remove-long-term
                       (hash ('memory_id (agent-memory-id memory))
                             ('block_id block-id))))))

;;; ============================================================================
;;; Working Memory Operations
;;; ============================================================================

(def (add-to-working! memory block)
  "Add a memory block to working memory"
  (let* ((working (agent-memory-working memory))
         (capacity (hash-ref (agent-memory-config memory) 'working-memory-capacity))
         (new-working (cons block working)))

    ;; Limit capacity (FIFO)
    (set! (agent-memory-working memory)
          (take new-working capacity))

    (set! (agent-memory-updated-at memory) (time->seconds (current-time)))

    block))

(def (get-working-memory memory)
  "Get working memory"
  (agent-memory-working memory))

(def (clear-working! memory)
  "Clear working memory"
  (set! (agent-memory-working memory) '())
  (set! (agent-memory-updated-at memory) (time->seconds (current-time))))

;;; ============================================================================
;;; Memory Consolidation
;;; ============================================================================

(def (consolidate-memory! memory)
  "Consolidate short-term memory to long-term memory"
  (displayln "Consolidating memory...")

  (let* ((short-term (agent-memory-short-term memory))
         (threshold (hash-ref (agent-memory-config memory) 'importance-threshold))
         (important-blocks (filter
                            (lambda (block)
                              (>= (memory-block-importance block) threshold))
                            short-term)))

    ;; Move important blocks to long-term memory
    (for-each
     (lambda (block)
       (add-to-long-term! memory block))
     important-blocks)

    ;; Clear short-term memory
    (clear-short-term! memory)

    ;; Log consolidation
    (elixir-wal-log! 'memory-consolidation
                     (hash ('memory_id (agent-memory-id memory))
                           ('num_consolidated (length important-blocks))
                           ('timestamp (time->seconds (current-time)))))

    (displayln (format "Consolidated ~a blocks to long-term memory"
                      (length important-blocks)))

    important-blocks))

;;; ============================================================================
;;; Memory Search
;;; ============================================================================

(def (search-memory memory query type: (type #f) limit: (limit 10))
  "Search memory by content (simple text search)"
  (let* ((all-blocks (append
                      (agent-memory-short-term memory)
                      (hash-values (agent-memory-long-term memory))))
         (filtered (if type
                       (filter (lambda (b) (eq? (memory-block-type b) type))
                               all-blocks)
                       all-blocks))
         (matches (filter
                   (lambda (block)
                     (memory-block-matches? block query))
                   filtered)))

    ;; Sort by importance and recency
    (take (sort-memory-blocks matches) limit)))

(def (memory-block-matches? block query)
  "Check if memory block matches query (simple substring search)"
  (let ((content (memory-block-content block)))
    (cond
     ((string? content)
      (string-contains content query))
     ((hash-table? content)
      (hash-any? (lambda (k v)
                   (or (and (string? v) (string-contains v query))
                       (and (symbol? v) (string-contains (symbol->string v) query))))
                 content))
     (else #f))))

(def (hash-any? pred h)
  "Check if any key-value pair in hash satisfies predicate"
  (let/cc return
    (hash-for-each
     (lambda (k v)
       (when (pred k v)
         (return #t)))
     h)
    #f))

;;; ============================================================================
;;; Semantic Search (Placeholder)
;;; ============================================================================

(def (semantic-search memory query-embedding limit: (limit 10))
  "Search memory using vector similarity (placeholder)"
  ;; This is a placeholder. In a real implementation, this would:
  ;; 1. Generate embedding for query
  ;; 2. Compute cosine similarity with all memory block embeddings
  ;; 3. Return top-k most similar blocks

  (displayln "Semantic search not yet implemented (requires embedding service)")

  ;; For now, return empty list
  '())

(def (generate-embedding text)
  "Generate vector embedding for text (placeholder)"
  ;; This would call an external embedding service (OpenAI, etc.)
  ;; For now, return dummy embedding
  (make-vector 1536 0.0))

;;; ============================================================================
;;; Memory Sorting and Ranking
;;; ============================================================================

(def (sort-memory-blocks blocks)
  "Sort memory blocks by importance and recency"
  (sort blocks
        (lambda (a b)
          (let ((score-a (memory-block-score a))
                (score-b (memory-block-score b)))
            (> score-a score-b)))))

(def (memory-block-score block)
  "Calculate relevance score for memory block"
  (let* ((importance (memory-block-importance block))
         (access-count (memory-block-access-count block))
         (recency (- (time->seconds (current-time))
                     (memory-block-last-accessed block)))
         (recency-score (/ 1.0 (+ 1.0 (/ recency 86400.0)))))  ; Decay over days

    ;; Weighted combination
    (+ (* 0.5 importance)
       (* 0.3 (min 1.0 (/ access-count 10.0)))
       (* 0.2 recency-score))))

;;; ============================================================================
;;; Memory Pruning
;;; ============================================================================

(def (prune-memory! memory min-importance: (min-importance 0.3) max-age-days: (max-age-days 30))
  "Prune low-importance and old memory blocks"
  (displayln "Pruning memory...")

  (let* ((long-term (agent-memory-long-term memory))
         (now (time->seconds (current-time)))
         (max-age-seconds (* max-age-days 86400))
         (blocks-to-remove '()))

    ;; Find blocks to remove
    (hash-for-each
     (lambda (block-id block)
       (let ((age (- now (memory-block-created-at block)))
             (importance (memory-block-importance block)))
         (when (or (< importance min-importance)
                   (> age max-age-seconds))
           (set! blocks-to-remove (cons block-id blocks-to-remove)))))
     long-term)

    ;; Remove blocks
    (for-each
     (lambda (block-id)
       (remove-from-long-term! memory block-id))
     blocks-to-remove)

    (displayln (format "Pruned ~a blocks from memory" (length blocks-to-remove)))

    blocks-to-remove))

;;; ============================================================================
;;; Memory Statistics
;;; ============================================================================

(def (memory-stats memory)
  "Get memory statistics"
  (hash
   ('id (agent-memory-id memory))
   ('short_term_count (length (agent-memory-short-term memory)))
   ('long_term_count (hash-length (agent-memory-long-term memory)))
   ('working_count (length (agent-memory-working memory)))
   ('episodic_count (length (agent-memory-episodic memory)))
   ('semantic_count (hash-length (agent-memory-semantic memory)))
   ('procedural_count (hash-length (agent-memory-procedural memory)))
   ('total_blocks (+ (length (agent-memory-short-term memory))
                     (hash-length (agent-memory-long-term memory))))
   ('created_at (agent-memory-created-at memory))
   ('updated_at (agent-memory-updated-at memory))))

;;; ============================================================================
;;; Memory Serialization
;;; ============================================================================

(def (serialize-memory memory)
  "Serialize memory to hash for checkpointing"
  (hash
   ('id (agent-memory-id memory))
   ('short_term (map serialize-memory-block (agent-memory-short-term memory)))
   ('long_term (hash-map (lambda (k v) (cons k (serialize-memory-block v)))
                        (agent-memory-long-term memory)))
   ('working (map serialize-memory-block (agent-memory-working memory)))
   ('episodic (map serialize-memory-block (agent-memory-episodic memory)))
   ('semantic (hash-map (lambda (k v) (cons k (serialize-memory-block v)))
                       (agent-memory-semantic memory)))
   ('procedural (hash-map (lambda (k v) (cons k (serialize-memory-block v)))
                         (agent-memory-procedural memory)))
   ('metadata (hash->list (agent-memory-metadata memory)))
   ('config (hash->list (agent-memory-config memory)))
   ('created_at (agent-memory-created-at memory))
   ('updated_at (agent-memory-updated-at memory))))

(def (serialize-memory-block block)
  "Serialize memory block to hash"
  (hash
   ('id (memory-block-id block))
   ('type (symbol->string (memory-block-type block)))
   ('content (memory-block-content block))
   ('embedding (memory-block-embedding block))
   ('importance (memory-block-importance block))
   ('access_count (memory-block-access-count block))
   ('last_accessed (memory-block-last-accessed block))
   ('tags (memory-block-tags block))
   ('metadata (hash->list (memory-block-metadata block)))
   ('created_at (memory-block-created-at block))))

(def (deserialize-memory data)
  "Deserialize memory from hash"
  (make-agent-memory
   id: (hash-ref data 'id)
   short-term: (map deserialize-memory-block (hash-ref data 'short_term))
   long-term: (list->hash
               (map (lambda (pair)
                      (cons (car pair) (deserialize-memory-block (cdr pair))))
                    (hash-ref data 'long_term)))
   working: (map deserialize-memory-block (hash-ref data 'working))
   episodic: (map deserialize-memory-block (hash-ref data 'episodic))
   semantic: (list->hash
              (map (lambda (pair)
                     (cons (car pair) (deserialize-memory-block (cdr pair))))
                   (hash-ref data 'semantic)))
   procedural: (list->hash
                (map (lambda (pair)
                       (cons (car pair) (deserialize-memory-block (cdr pair))))
                     (hash-ref data 'procedural)))
   metadata: (list->hash (hash-ref data 'metadata))
   config: (list->hash (hash-ref data 'config))
   created-at: (hash-ref data 'created_at)
   updated-at: (hash-ref data 'updated_at)))

(def (deserialize-memory-block data)
  "Deserialize memory block from hash"
  (make-memory-block
   id: (hash-ref data 'id)
   type: (string->symbol (hash-ref data 'type))
   content: (hash-ref data 'content)
   embedding: (hash-ref data 'embedding)
   importance: (hash-ref data 'importance)
   access-count: (hash-ref data 'access_count)
   last-accessed: (hash-ref data 'last_accessed)
   tags: (hash-ref data 'tags)
   metadata: (list->hash (hash-ref data 'metadata))
   created-at: (hash-ref data 'created_at)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (take lst n)
  "Take first n elements from list"
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(def (hash-map fn h)
  "Map function over hash table"
  (let ((result '()))
    (hash-for-each
     (lambda (k v)
       (set! result (cons (fn k v) result)))
     h)
    result))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create memory system
(def my-memory (make-agent-memory-instance))

;; Create memory blocks
(def block1 (make-memory-block-instance
             :episodic
             "User asked about weather"
             importance: 0.7
             tags: '(weather user-query)))

(def block2 (make-memory-block-instance
             :semantic
             (hash 'fact "Paris is the capital of France"
                   'confidence 1.0)
             importance: 0.9
             tags: '(geography fact)))

;; Add to short-term memory
(add-to-short-term! my-memory block1)
(add-to-short-term! my-memory block2)

;; Consolidate to long-term
(consolidate-memory! my-memory)

;; Search memory
(def results (search-memory my-memory "weather" limit: 5))

;; Get statistics
(def stats (memory-stats my-memory))
|#
