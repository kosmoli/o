#lang racket

;;; tools/memory.rkt - Memory Tools Implementation
;;;
;;; Memory tools for agent operations: core memory, archival memory, and recall memory.

(provide core-memory-append-tool
         core-memory-append-handler
         core-memory-replace-tool
         core-memory-replace-handler
         archival-memory-insert-tool
         archival-memory-insert-handler
         archival-memory-search-tool
         archival-memory-search-handler
         archival-memory-semantic-search-tool
         archival-memory-semantic-search-handler
         register-memory-tools!)

(require racket/hash
        racket/format
        "../memory/blocks.rkt"
        "../memory/core.rkt"
        "../memory/archival.rkt"
        "../memory/semantic.rkt"
        "./types.rkt"
        "./core.rkt")

;;; ============================================================================
;;; Core Memory Tools
;;; ============================================================================

(define (core-memory-append-handler arguments context)
  "Handler for core_memory_append tool

   Args:
     arguments: Hash with 'name' and 'content' keys
     context: Execution context

   Returns:
     Tool result with updated memory block"

  (define block-name (hash-ref arguments 'name))
  (define content (hash-ref arguments 'content))
  (define agent-id (tool-execution-context-agent-id context))

  ;; Create memory manager
  (define manager (create-block-manager agent-id))

  ;; Get old value
  (define old-value (block-get-value manager block-name))

  ;; Append to memory block
  (define new-value
    (if (string=? old-value "")
        content
        (string-append old-value "\n" content)))

  ;; Update block
  (block-update! manager block-name new-value)

  (make-success-result
   (hash 'block_name block-name
         'old_value old-value
         'new_value new-value
         'updated_at (current-seconds))
   #:metadata (hash 'tool "core_memory_append")))

(define core-memory-append-tool
  (tool-definition-internal
   "core_memory_append"
   "Append content to a core memory block. Use this to add new information to persona or human memory blocks without replacing existing content."
   (hash 'name (hash 'type 'string
                      'description "Name of the memory block (e.g., 'persona', 'human')"
                      'required #t)
         'content (hash 'type 'string
                        'description "Content to append to the memory block"
                        'required #t))
   core-memory-append-handler
   'memory
   #f
   (hash 'version "1.0")))

(define (core-memory-replace-handler arguments context)
  "Handler for core_memory_replace tool

   Args:
     arguments: Hash with 'name', 'old_content', 'new_content' keys
     context: Execution context

   Returns:
     Tool result with updated memory block"

  (define block-name (hash-ref arguments 'name))
  (define old-content (hash-ref arguments 'old_content))
  (define new-content (hash-ref arguments 'new_content))
  (define agent-id (tool-execution-context-agent-id context))

  ;; Create memory manager
  (define manager (create-block-manager agent-id))

  ;; Get old value
  (define old-value (block-get-value manager block-name))

  ;; Replace in memory block
  (define new-value (string-replace old-value old-content new-content))

  ;; Update block
  (block-update! manager block-name new-value)

  (make-success-result
   (hash 'block_name block-name
         'old_value old-value
         'new_value new-value
         'updated_at (current-seconds))
   #:metadata (hash 'tool "core_memory_replace")))

(define core-memory-replace-tool
  (tool-definition-internal
   "core_memory_replace"
   "Replace content in a core memory block. Use this to update or correct existing information in persona or human memory blocks."
   (hash 'name (hash 'type 'string
                      'description "Name of the memory block (e.g., 'persona', 'human')"
                      'required #t)
         'old_content (hash 'type 'string
                            'description "Content to replace (must exist in the block)"
                            'required #t)
         'new_content (hash 'type 'string
                            'description "New content to replace with"
                            'required #t))
   core-memory-replace-handler
   'memory
   #f
   (hash 'version "1.0")))

;;; ============================================================================
;;; Archival Memory Tools
;;; ============================================================================

(define (archival-memory-insert-handler arguments context)
  "Handler for archival_memory_insert tool

   Args:
     arguments: Hash with 'content', 'importance', 'tags' keys
     context: Execution context

   Returns:
     Tool result with created entry"

  (define content (hash-ref arguments 'content))
  (define importance (hash-ref arguments 'importance 5))
  (define tags (hash-ref arguments 'tags '()))
  (define agent-id (tool-execution-context-agent-id context))

  ;; Convert importance to 0.0-1.0 scale
  (define normalized-importance (/ importance 10.0))

  ;; Create archival manager
  (define manager (make-archival-manager agent-id))

  ;; Create entry (returns a hash)
  (define entry (archival-insert! manager content
                                  #:importance normalized-importance
                                  #:tags tags))

  (make-success-result
   (hash 'id (hash-ref entry 'id)
         'content content
         'importance importance
         'tags tags
         'created_at (hash-ref entry 'created_at))
   #:metadata (hash 'tool "archival_memory_insert")))

(define archival-memory-insert-tool
  (tool-definition-internal
   "archival_memory_insert"
   "Insert content into archival memory. Use this to store important information for long-term retrieval. Content will be embedded for semantic search."
   (hash 'content (hash 'type 'string
                        'description "Content to store in archival memory"
                        'required #t)
         'importance (hash 'type 'integer
                          'description "Importance score (1-10, default: 5)"
                          'required #f
                          'default 5)
         'tags (hash 'type 'array
                    'description "Tags for categorization (optional)"
                    'required #f
                    'default '()))
   archival-memory-insert-handler
   'memory
   #f
   (hash 'version "1.0")))

(define (archival-memory-search-handler arguments context)
  "Handler for archival_memory_search tool

   Args:
     arguments: Hash with 'query', 'limit', 'page' keys
     context: Execution context

   Returns:
     Tool result with matching entries"

  (define query (hash-ref arguments 'query))
  (define limit (hash-ref arguments 'limit 10))
  (define page (hash-ref arguments 'page 0))
  (define agent-id (tool-execution-context-agent-id context))

  ;; Create archival manager
  (define manager (make-archival-manager agent-id))

  ;; Search entries (returns list of hashes)
  (define results (archival-search manager query
                                    #:limit limit
                                    #:offset (* page limit)))

  ;; Format results
  (define formatted-results
    (map (lambda (entry)
           (hash 'id (hash-ref entry 'id)
                 'content (hash-ref entry 'content)
                 'importance (hash-ref entry 'importance)
                 'tags (hash-ref entry 'tags)
                 'created_at (hash-ref entry 'created_at)))
         results))

  (make-success-result
   (hash 'results formatted-results
         'count (length formatted-results)
         'query query
         'page page)
   #:metadata (hash 'tool "archival_memory_search")))

(define archival-memory-search-tool
  (tool-definition-internal
   "archival_memory_search"
   "Search archival memory for relevant information. Use this to retrieve previously stored information based on text matching."
   (hash 'query (hash 'type 'string
                     'description "Search query to find matching entries"
                     'required #t)
         'limit (hash 'type 'integer
                     'description "Maximum number of results to return (default: 10)"
                     'required #f
                     'default 10)
         'page (hash 'type 'integer
                    'description "Page number for pagination (0-indexed, default: 0)"
                    'required #f
                    'default 0))
   archival-memory-search-handler
   'memory
   #f
   (hash 'version "1.0")))

(define (archival-memory-semantic-search-handler arguments context)
  "Handler for archival_memory_semantic_search tool

   Args:
     arguments: Hash with 'query', 'limit', 'min_similarity' keys
     context: Execution context

   Returns:
     Tool result with semantically similar entries"

  (define query (hash-ref arguments 'query))
  (define limit (hash-ref arguments 'limit 10))
  (define min-similarity (hash-ref arguments 'min_similarity 0.7))
  (define agent-id (tool-execution-context-agent-id context))

  ;; Create archival manager
  (define manager (make-archival-manager agent-id))

  ;; Semantic search (returns list of hashes with similarity)
  (define results (semantic-search manager query
                                    #:limit limit
                                    #:min-similarity min-similarity))

  ;; Format results
  (define formatted-results
    (map (lambda (entry)
           (hash 'id (hash-ref entry 'id)
                 'content (hash-ref entry 'content)
                 'importance (hash-ref entry 'importance)
                 'tags (hash-ref entry 'tags)
                 'similarity (hash-ref entry 'similarity)
                 'created_at (hash-ref entry 'created_at)))
         results))

  (make-success-result
   (hash 'results formatted-results
         'count (length formatted-results)
         'query query
         'min_similarity min-similarity)
   #:metadata (hash 'tool "archival_memory_semantic_search")))

(define archival-memory-semantic-search-tool
  (tool-definition-internal
   "archival_memory_semantic_search"
   "Search archival memory using semantic similarity. Use this to find conceptually related information even if the exact words don't match."
   (hash 'query (hash 'type 'string
                     'description "Search query for semantic matching"
                     'required #t)
         'limit (hash 'type 'integer
                     'description "Maximum number of results to return (default: 10)"
                     'required #f
                     'default 10)
         'min_similarity (hash 'type 'number
                              'description "Minimum similarity score (0.0-1.0, default: 0.7)"
                              'required #f
                              'default 0.7))
   archival-memory-semantic-search-handler
   'memory
   #f
   (hash 'version "1.0")))

;;; ============================================================================
;;; Initialize Memory Tools
;;; ============================================================================

(define (register-memory-tools! dispatcher)
  "Register all memory tools with dispatcher

   Args:
     dispatcher: Tool dispatcher (vector: registry call-history approval-queue)

   Returns:
     #t on success"

  (dispatcher-register-tool! dispatcher core-memory-append-tool)
  (dispatcher-register-tool! dispatcher core-memory-replace-tool)
  (dispatcher-register-tool! dispatcher archival-memory-insert-tool)
  (dispatcher-register-tool! dispatcher archival-memory-search-tool)
  (dispatcher-register-tool! dispatcher archival-memory-semantic-search-tool)
  (displayln "Memory tools registered")
  #t)
