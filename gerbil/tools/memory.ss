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
         (hash 'block_name block-name
               'old_value (hash-ref block 'old_value)
               'new_value (hash-ref block 'value)
               'updated_at (hash-ref block 'updated_at))
         metadata: (hash 'tool "core_memory_append"))))))

(def core-memory-append-tool
  (make-tool-definition
   name: "core_memory_append"
   description: "Append content to a core memory block. Use this to add new information to persona or human memory blocks without replacing existing content."
   parameters: (hash
                'name (hash 'type :string
                           'description "Name of the memory block (e.g., 'persona', 'human')"
                           'required #t)
                'content (hash 'type :string
                              'description "Content to append to the memory block"
                              'required #t))
   handler: core-memory-append-handler
   category: :memory
   requires-approval: #f
   metadata: (hash 'version "1.0")))

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
         (hash 'block_name block-name
               'old_value (hash-ref block 'old_value)
               'new_value (hash-ref block 'value)
               'updated_at (hash-ref block 'updated_at))
         metadata: (hash 'tool "core_memory_replace"))))))

(def core-memory-replace-tool
  (make-tool-definition
   name: "core_memory_replace"
   description: "Replace content in a core memory block. Use this to update or correct existing information in persona or human memory blocks."
   parameters: (hash
                'name (hash 'type :string
                           'description "Name of the memory block (e.g., 'persona', 'human')"
                           'required #t)
                'old_content (hash 'type :string
                                  'description "Content to replace (must exist in the block)"
                                  'required #t)
                'new_content (hash 'type :string
                                  'description "New content to replace with"
                                  'required #t))
   handler: core-memory-replace-handler
   category: :memory
   requires-approval: #f
   metadata: (hash 'version "1.0")))

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
         (hash 'id (hash-ref entry 'id)
               'content content
               'importance importance
               'tags tags
               'created_at (hash-ref entry 'created_at))
         metadata: (hash 'tool "archival_memory_insert"))))))

(def archival-memory-insert-tool
  (make-tool-definition
   name: "archival_memory_insert"
   description: "Insert content into archival memory. Use this to store important information for long-term retrieval. Content will be embedded for semantic search."
   parameters: (hash
                'content (hash 'type :string
                              'description "Content to store in archival memory"
                              'required #t)
                'importance (hash 'type :integer
                                 'description "Importance score (1-10, default: 5)"
                                 'required #f
                                 'default 5)
                'tags (hash 'type :array
                           'description "Tags for categorization (optional)"
                           'required #f
                           'default '()))
   handler: archival-memory-insert-handler
   category: :memory
   requires-approval: #f
   metadata: (hash 'version "1.0")))

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
                     (hash 'id (hash-ref entry 'id)
                           'content (hash-ref entry 'content)
                           'importance (hash-ref entry 'importance)
                           'tags (hash-ref entry 'tags)
                           'created_at (hash-ref entry 'created_at)))
                   results)))

          (make-success-result
           (hash 'results formatted-results
                 'count (length formatted-results)
                 'query query
                 'page page)
           metadata: (hash 'tool "archival_memory_search")))))))

(def archival-memory-search-tool
  (make-tool-definition
   name: "archival_memory_search"
   description: "Search archival memory for relevant information. Use this to retrieve previously stored information based on text matching."
   parameters: (hash
                'query (hash 'type :string
                            'description "Search query to find matching entries"
                            'required #t)
                'limit (hash 'type :integer
                            'description "Maximum number of results to return (default: 10)"
                            'required #f
                            'default 10)
                'page (hash 'type :integer
                           'description "Page number for pagination (0-indexed, default: 0)"
                           'required #f
                           'default 0))
   handler: archival-memory-search-handler
   category: :memory
   requires-approval: #f
   metadata: (hash 'version "1.0")))

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
                     (hash 'id (hash-ref entry 'id)
                           'content (hash-ref entry 'content)
                           'importance (hash-ref entry 'importance)
                           'tags (hash-ref entry 'tags)
                           'similarity (hash-ref entry 'similarity)
                           'created_at (hash-ref entry 'created_at)))
                   results)))

          (make-success-result
           (hash 'results formatted-results
                 'count (length formatted-results)
                 'query query
                 'min_similarity min-similarity)
           metadata: (hash 'tool "archival_memory_semantic_search")))))))

(def archival-memory-semantic-search-tool
  (make-tool-definition
   name: "archival_memory_semantic_search"
   description: "Search archival memory using semantic similarity. Use this to find conceptually related information even if the exact words don't match."
   parameters: (hash
                'query (hash 'type :string
                            'description "Search query for semantic matching"
                            'required #t)
                'limit (hash 'type :integer
                            'description "Maximum number of results to return (default: 10)"
                            'required #f
                            'default 10)
                'min_similarity (hash 'type :number
                                     'description "Minimum similarity score (0.0-1.0, default: 0.7)"
                                     'required #f
                                     'default 0.7))
   handler: archival-memory-semantic-search-handler
   category: :memory
   requires-approval: #f
   metadata: (hash 'version "1.0")))

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
                               (hash 'name "persona"
                                     'content "I am helpful and friendly.")
                               agent-id))

;; Replace in core memory
(def call (dispatcher-call-tool dispatcher
                               "core_memory_replace"
                               (hash 'name "persona"
                                     'old_content "helpful"
                                     'new_content "very helpful")
                               agent-id))

;; Insert into archival memory
(def call (dispatcher-call-tool dispatcher
                               "archival_memory_insert"
                               (hash 'content "User prefers Python over JavaScript"
                                     'importance 8
                                     'tags '("preferences" "programming"))
                               agent-id))

;; Search archival memory
(def call (dispatcher-call-tool dispatcher
                               "archival_memory_search"
                               (hash 'query "Python"
                                     'limit 5)
                               agent-id))

;; Semantic search archival memory
(def call (dispatcher-call-tool dispatcher
                               "archival_memory_semantic_search"
                               (hash 'query "programming languages"
                                     'limit 5
                                     'min_similarity 0.7)
                               agent-id))
|#
