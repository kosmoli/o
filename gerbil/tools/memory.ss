;;; tools/memory.ss - Memory Tools Implementation
;;;
;;; Memory tools for agent operations: core memory, archival memory, and recall memory.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/text/json
  :o/memory/blocks
  :o/memory/core
  :o/memory/archival
  :o/memory/semantic
  :o/tools/types
  :o/tools/core)

;;; ============================================================================
;;; Core Memory Tools
;;; ============================================================================

(def (core-memory-append-handler arguments context)
  "Handler for core_memory_append tool

   Args:
     arguments: Hash with 'name' and 'content' keys
     context: Execution context

   Returns:
     Tool result with updated memory block"

  (let ((block-name (hash-ref arguments 'name))
        (content (hash-ref arguments 'content))
        (agent-id (tool-execution-context-agent-id context)))

    ;; Create memory manager
    (let ((manager (make-memory-block-manager agent-id)))

      ;; Append to memory block
      (let ((block (memory-block-append! manager block-name content)))

        (make-success-result
         (let ((ht (make-hash-table)))
  (hash-put! ht 'block_name block-name)
  (hash-put! ht 'old_value (hash-ref block 'old_value))
  ht)
         metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'tool "core_memory_append")
  ht))))))

(def core-memory-append-tool
  (make-tool-definition
   name: "core_memory_append"
   description: "Append content to a core memory block. Use this to add new information to persona or human memory blocks without replacing existing content."
   parameters: (let ((ht (make-hash-table)))
  (hash-put! ht 'name (hash 'type 'string
                           'description "Name of the memory block (e.g., 'persona', 'human'))"
                           'required #t)
  ht)
   handler: core-memory-append-handler
   category: 'memory
   requires-approval: #f
   metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'version "1.0")
  ht)))

(def (core-memory-replace-handler arguments context)
  "Handler for core_memory_replace tool

   Args:
     arguments: Hash with 'name', 'old_content', 'new_content' keys
     context: Execution context

   Returns:
     Tool result with updated memory block"

  (let ((block-name (hash-ref arguments 'name))
        (old-content (hash-ref arguments 'old_content))
        (new-content (hash-ref arguments 'new_content))
        (agent-id (tool-execution-context-agent-id context)))

    ;; Create memory manager
    (let ((manager (make-memory-block-manager agent-id)))

      ;; Replace in memory block
      (let ((block (memory-block-replace! manager block-name old-content new-content)))

        (make-success-result
         (let ((ht (make-hash-table)))
  (hash-put! ht 'block_name block-name)
  (hash-put! ht 'old_value (hash-ref block 'old_value))
  ht)
         metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'tool "core_memory_replace")
  ht))))))

(def core-memory-replace-tool
  (make-tool-definition
   name: "core_memory_replace"
   description: "Replace content in a core memory block. Use this to update or correct existing information in persona or human memory blocks."
   parameters: (let ((ht (make-hash-table)))
  (hash-put! ht 'name (hash 'type 'string
                           'description "Name of the memory block (e.g., 'persona', 'human'))"
                           'required #t)
  ht)
   handler: core-memory-replace-handler
   category: 'memory
   requires-approval: #f
   metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'version "1.0")
  ht)))

;;; ============================================================================
;;; Archival Memory Tools
;;; ============================================================================

(def (archival-memory-insert-handler arguments context)
  "Handler for archival_memory_insert tool

   Args:
     arguments: Hash with 'content', 'importance', 'tags' keys
     context: Execution context

   Returns:
     Tool result with created entry"

  (let ((content (hash-ref arguments 'content))
        (importance (hash-ref arguments 'importance 5))
        (tags (hash-ref arguments 'tags '()))
        (agent-id (tool-execution-context-agent-id context)))

    ;; Create archival manager
    (let ((manager (make-archival-memory-manager agent-id)))

      ;; Create entry
      (let ((entry (archival-create! manager content
                                    importance: importance
                                    tags: tags)))

        (make-success-result
         (let ((ht (make-hash-table)))
  (hash-put! ht 'id (hash-ref entry 'id))
  ht)
         metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'tool "archival_memory_insert")
  ht))))))

(def archival-memory-insert-tool
  (make-tool-definition
   name: "archival_memory_insert"
   description: "Insert content into archival memory. Use this to store important information for long-term retrieval. Content will be embedded for semantic search."
   parameters: (let ((ht (make-hash-table)))
  (hash-put! ht 'content (hash 'type 'string
                              'description "Content to store in archival memory"
                              'required #t))
  ht)
   handler: archival-memory-insert-handler
   category: 'memory
   requires-approval: #f
   metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'version "1.0")
  ht)))

(def (archival-memory-search-handler arguments context)
  "Handler for archival_memory_search tool

   Args:
     arguments: Hash with 'query', 'limit', 'page' keys
     context: Execution context

   Returns:
     Tool result with matching entries"

  (let ((query (hash-ref arguments 'query))
        (limit (hash-ref arguments 'limit 10))
        (page (hash-ref arguments 'page 0))
        (agent-id (tool-execution-context-agent-id context)))

    ;; Create archival manager
    (let ((manager (make-archival-memory-manager agent-id)))

      ;; Search entries
      (let ((results (archival-search manager query
                                     limit: limit
                                     offset: (* page limit))))

        ;; Format results
        (let ((formatted-results
               (map (lambda (entry)
                     (let ((ht (make-hash-table)))
  (hash-put! ht 'id (hash-ref entry 'id))
  ht))
                   results)))

          (make-success-result
           (let ((ht (make-hash-table)))
  (hash-put! ht 'results formatted-results)
  (hash-put! ht 'count (length formatted-results))
  ht)
           metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'tool "archival_memory_search")
  ht)))))))

(def archival-memory-search-tool
  (make-tool-definition
   name: "archival_memory_search"
   description: "Search archival memory for relevant information. Use this to retrieve previously stored information based on text matching."
   parameters: (let ((ht (make-hash-table)))
  (hash-put! ht 'query (hash 'type 'string
                            'description "Search query to find matching entries"
                            'required #t))
  ht)
   handler: archival-memory-search-handler
   category: 'memory
   requires-approval: #f
   metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'version "1.0")
  ht)))

(def (archival-memory-semantic-search-handler arguments context)
  "Handler for archival_memory_semantic_search tool

   Args:
     arguments: Hash with 'query', 'limit', 'min_similarity' keys
     context: Execution context

   Returns:
     Tool result with semantically similar entries"

  (let ((query (hash-ref arguments 'query))
        (limit (hash-ref arguments 'limit 10))
        (min-similarity (hash-ref arguments 'min_similarity 0.7))
        (agent-id (tool-execution-context-agent-id context)))

    ;; Create archival manager
    (let ((manager (make-archival-memory-manager agent-id)))

      ;; Semantic search
      (let ((results (semantic-search manager query
                                     limit: limit
                                     min-similarity: min-similarity)))

        ;; Format results
        (let ((formatted-results
               (map (lambda (entry)
                     (let ((ht (make-hash-table)))
  (hash-put! ht 'id (hash-ref entry 'id))
  ht))
                   results)))

          (make-success-result
           (let ((ht (make-hash-table)))
  (hash-put! ht 'results formatted-results)
  (hash-put! ht 'count (length formatted-results))
  ht)
           metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'tool "archival_memory_semantic_search")
  ht)))))))

(def archival-memory-semantic-search-tool
  (make-tool-definition
   name: "archival_memory_semantic_search"
   description: "Search archival memory using semantic similarity. Use this to find conceptually related information even if the exact words don't match."
   parameters: (let ((ht (make-hash-table)))
  (hash-put! ht 'query (hash 'type 'string
                            'description "Search query for semantic matching"
                            'required #t))
  ht)
   handler: archival-memory-semantic-search-handler
   category: 'memory
   requires-approval: #f
   metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'version "1.0")
  ht)))

;;; ============================================================================
;;; Initialize Memory Tools
;;; ============================================================================

(def (register-memory-tools! dispatcher)
  "Register all memory tools with dispatcher

   Args:
     dispatcher: Tool dispatcher

   Returns:
     #t on success"

  (dispatcher-register-tool! dispatcher core-memory-append-tool)
  (dispatcher-register-tool! dispatcher core-memory-replace-tool)
  (dispatcher-register-tool! dispatcher archival-memory-insert-tool)
  (dispatcher-register-tool! dispatcher archival-memory-search-tool)
  (dispatcher-register-tool! dispatcher archival-memory-semantic-search-tool)
  (displayln "Memory tools registered")
  #t)

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create dispatcher
(def dispatcher (make-tool-dispatcher))

;; Register core and memory tools
(register-core-tools! dispatcher)
(register-memory-tools! dispatcher)

;; Append to core memory
(def call (dispatcher-call-tool dispatcher
                               "core_memory_append"
                               (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'content "I am helpful and friendly.")
  ht)
                               agent-id))

;; Replace in core memory
(def call (dispatcher-call-tool dispatcher
                               "core_memory_replace"
                               (let ((ht (make-hash-table)))
  (hash-put! ht 'name "persona")
  (hash-put! ht 'old_content "helpful")
  (hash-put! ht 'new_content "very helpful")
  ht)
                               agent-id))

;; Insert into archival memory
(def call (dispatcher-call-tool dispatcher
                               "archival_memory_insert"
                               (let ((ht (make-hash-table)))
  (hash-put! ht 'content "User prefers Python over JavaScript")
  (hash-put! ht 'importance 8)
  (hash-put! ht 'tags '("preferences"))
  ht)
                               agent-id))

;; Search archival memory
(def call (dispatcher-call-tool dispatcher
                               "archival_memory_search"
                               (let ((ht (make-hash-table)))
  (hash-put! ht 'query "Python")
  (hash-put! ht 'limit 5)
  ht)
                               agent-id))

;; Semantic search archival memory
(def call (dispatcher-call-tool dispatcher
                               "archival_memory_semantic_search"
                               (let ((ht (make-hash-table)))
  (hash-put! ht 'query "programming languages")
  (hash-put! ht 'limit 5)
  (hash-put! ht 'min_similarity 0.7)
  ht)
                               agent-id))
|#
