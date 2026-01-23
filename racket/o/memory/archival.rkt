#lang racket

;;; memory/archival.rkt - Archival Memory Operations
;;;
;;; Long-term memory storage with semantic search and embeddings.

(provide make-archival-manager
         archival-manager?
         generate-embedding
         generate-embeddings-batch
         archival-insert!
         archival-insert-batch!
         archival-get
         archival-get-all
         archival-get-recent
         archival-search
         archival-search-by-tags
         archival-search-by-importance
         archival-page?
         archival-get-page
         archival-get-next-page
         archival-get-prev-page
         archival-update!
         archival-update-importance!
         archival-add-tags!
         archival-delete!
         archival-delete-batch!
         archival-clear!
         archival-count
         archival-total-size
         archival-get-stats
         archival-export
         archival-import!)

(require racket/hash
        racket/format
        racket/list
        racket/string
        json
        "../database/client.rkt"
        "../llm/client.rkt"
        "./types.rkt")

;;; ============================================================================
;;; Archival Memory Manager
;;; ============================================================================

(struct archival-manager
  (agent-id         ; Agent ID
   llm-provider     ; LLM provider for embeddings
   llm-model        ; LLM model for embeddings
   cache            ; Entry cache
   cache-enabled)   ; Is caching enabled?
  #:transparent
  #:mutable)

(define (make-archival-manager agent-id
                                #:llm-provider [llm-provider 'openai]
                                #:llm-model [llm-model "text-embedding-3-small"]
                                #:cache-enabled [cache-enabled #t])
  "Create archival memory manager

   Args:
     agent-id: Agent ID
     llm-provider: LLM provider for embeddings (default: 'openai)
     llm-model: Embedding model (default: text-embedding-3-small)
     cache-enabled: Enable caching (default: #t)

   Returns:
     Archival manager"

  (archival-manager agent-id
                   llm-provider
                   llm-model
                   (hash)
                   cache-enabled))

;;; ============================================================================
;;; Embedding Generation
;;; ============================================================================

(define (generate-embedding manager content)
  "Generate embedding vector for content

   Args:
     manager: Archival manager
     content: Text content to embed

   Returns:
     Embedding vector (list of floats, 1536 dimensions)"

  (define provider (archival-manager-llm-provider manager))
  (define model (archival-manager-llm-model manager))

  ;; Call LLM API to generate embedding
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (displayln (format "Embedding generation error: ~a" (exn-message e)))
                     ;; Return zero vector as fallback
                     (make-list 1536 0.0))])
    (define config (make-llm-config provider model "dummy-api-key"))
    (define response (llm-embeddings config content))
    response))

(define (generate-embeddings-batch manager contents)
  "Generate embeddings for multiple contents

   Args:
     manager: Archival manager
     contents: List of text contents

   Returns:
     List of embedding vectors"

  (map (lambda (content)
         (generate-embedding manager content))
       contents))

;;; ============================================================================
;;; Archival Entry Creation
;;; ============================================================================

(define (archival-insert! manager content
                           #:importance [importance 0.5]
                           #:tags [tags '()]
                           #:generate-embedding? [generate-embedding? #t])
  "Insert entry into archival memory

   Args:
     manager: Archival manager
     content: Entry content
     importance: Importance score 0.0-1.0 (default: 0.5)
     tags: List of tags (default: empty)
     generate-embedding?: Generate embedding vector (default: #t)

   Returns:
     Created entry"

  ;; Validate parameters
  (define params (hash 'content content
                       'importance importance
                       'tags tags))
  (define result (validate-archival-params params))
  (unless (and (pair? result) (car result))
    (error 'archival-insert! "Invalid archival parameters" (cdr result)))

  ;; Generate embedding if requested
  (define embedding
    (if generate-embedding?
        (generate-embedding manager content)
        #f))

  ;; Create entry via database
  (define entry (db-create-archival-entry
                 (archival-manager-agent-id manager)
                 (hash 'content content
                       'embedding embedding
                       'importance importance
                       'tags tags)))

  ;; Add to cache
  (when (archival-manager-cache-enabled manager)
    (archival-cache-put! manager (hash-ref entry 'id) entry))

  (displayln (format "Archival entry created: ~a" (hash-ref entry 'id)))
  entry)

(define (archival-insert-batch! manager entries)
  "Insert multiple entries into archival memory

   Args:
     manager: Archival manager
     entries: List of entry hashes with content, importance, tags

   Returns:
     List of created entries"

  (map (lambda (entry-data)
         (archival-insert! manager
                           (hash-ref entry-data 'content)
                           #:importance (hash-ref entry-data 'importance 0.5)
                           #:tags (hash-ref entry-data 'tags '())
                           #:generate-embedding? (hash-ref entry-data 'generate_embedding #t)))
       entries))

;;; ============================================================================
;;; Archival Entry Retrieval
;;; ============================================================================

(define (archival-get manager entry-id)
  "Get archival entry by ID
   Args:
     manager: Archival manager
     entry-id: Entry ID

   Returns:
     Entry or #f if not found"

  ;; Check cache first
  (if (archival-manager-cache-enabled manager)
      (or (archival-cache-get manager entry-id)
          (let ([entry (db-get-archival-entry (archival-manager-agent-id manager)
                                              entry-id)])
            (when entry
              (archival-cache-put! manager entry-id entry))
            entry))
      ;; No cache, query database
      (db-get-archival-entry (archival-manager-agent-id manager) entry-id)))

(define (archival-get-all manager #:limit [limit 100] #:offset [offset 0])
  "Get all archival entries

   Args:
     manager: Archival manager
     limit: Maximum entries to return (default: 100)
     offset: Offset for pagination (default: 0)

   Returns:
     List of entries"

  (db-get-archival-entries (archival-manager-agent-id manager)
                           #:limit limit
                           #:offset offset))

(define (archival-get-recent manager n)
  "Get N most recent archival entries

   Args:
     manager: Archival manager
     n: Number of entries

   Returns:
     List of entries"

  (archival-get-all manager #:limit n #:offset 0))

;;; ============================================================================
;;; Text-Based Search
;;; ============================================================================

(define (archival-search manager query #:limit [limit 10])
  "Search archival memory by text content

   Args:
     manager: Archival manager
     query: Search query
     limit: Maximum results (default: 10)

   Returns:
     List of matching entries"

  (define all-entries (archival-get-all manager #:limit 1000))
  ;; Filter entries containing query
  (define matches
    (filter (lambda (entry)
              (string-contains? (hash-ref entry 'content "") query))
            all-entries))
  ;; Sort by importance (descending)
  (define sorted
    (sort matches >
          #:key (lambda (entry)
                  (hash-ref entry 'importance 0.5))))
  ;; Return top N results
  (take sorted (min limit (length sorted))))

(define (archival-search-by-tags manager tags #:limit [limit 10])
  "Search archival memory by tags

   Args:
     manager: Archival manager
     tags: List of tags to search for
     limit: Maximum results (default: 10)

   Returns:
     List of matching entries"

  (define all-entries (archival-get-all manager #:limit 1000))
  ;; Filter entries with matching tags
  (define matches
    (filter (lambda (entry)
              (define entry-tags (hash-ref entry 'tags '()))
              ;; Check if any search tag is in entry tags
              (for/or ([tag (in-list tags)])
                (member tag entry-tags)))
            all-entries))
  ;; Sort by importance
  (define sorted
    (sort matches >
          #:key (lambda (entry)
                  (hash-ref entry 'importance 0.5))))
  (take sorted (min limit (length sorted))))

(define (archival-search-by-importance manager min-importance #:limit [limit 10])
  "Search archival memory by importance threshold

   Args:
     manager: Archival manager
     min-importance: Minimum importance score
     limit: Maximum results (default: 10)

   Returns:
     List of matching entries"

  (define all-entries (archival-get-all manager #:limit 1000))
  ;; Filter entries above importance threshold
  (define matches
    (filter (lambda (entry)
              (>= (hash-ref entry 'importance 0.5) min-importance))
            all-entries))
  ;; Sort by importance (descending)
  (define sorted
    (sort matches >
          #:key (lambda (entry)
                  (hash-ref entry 'importance 0.5))))
  (take sorted (min limit (length sorted))))

;;; ============================================================================
;;; Pagination
;;; ============================================================================

(struct archival-page
  (entries          ; List of entries
   total            ; Total number of entries
   page             ; Current page number
   page-size        ; Entries per page
   has-next?        ; Has next page?
   has-prev?)       ; Has previous page?
  #:transparent)

(define (archival-get-page manager page-number page-size)
  "Get paginated archival entries

   Args:
     manager: Archival manager
     page-number: Page number (0-indexed)
     page-size: Entries per page

   Returns:
     Archival page result"

  (define offset (* page-number page-size))
  (define entries (archival-get-all manager #:limit page-size #:offset offset))
  (define total (archival-count manager))
  (define has-next? (< (+ offset page-size) total))
  (define has-prev? (> page-number 0))

  (archival-page entries
                     total
                     page-number
                     page-size
                     has-next?
                     has-prev?))

(define (archival-get-next-page page-result)
  "Get next page number

   Args:
     page-result: Current page result

   Returns:
     Next page number or #f"

  (if (archival-page-has-next? page-result)
      (+ (archival-page-page page-result) 1)
      #f))

(define (archival-get-prev-page page-result)
  "Get previous page number

   Args:
     page-result: Current page result

   Returns:
     Previous page number or #f"

  (if (archival-page-has-prev? page-result)
      (- (archival-page-page page-result) 1)
      #f))

;;; ============================================================================
;;; Archival Entry Update
;;; ============================================================================

(define (archival-update! manager entry-id updates)
  "Update archival entry

   Args:
     manager: Archival manager
     entry-id: Entry ID
     updates: Hash with fields to update (content, importance, tags)

   Returns:
     Updated entry"

  ;; Update via database
  (db-update-archival-entry (archival-manager-agent-id manager)
                            entry-id
                            updates)

  ;; Invalidate cache
  (when (archival-manager-cache-enabled manager)
    (archival-cache-invalidate! manager entry-id))

  (displayln (format "Archival entry updated: ~a" entry-id))

  ;; Return updated entry
  (archival-get manager entry-id))

(define (archival-update-importance! manager entry-id importance)
  "Update entry importance

   Args:
     manager: Archival manager
     entry-id: Entry ID
     importance: New importance score

   Returns:
     Updated entry"

  (archival-update! manager entry-id (hash 'importance importance)))

(define (archival-add-tags! manager entry-id new-tags)
  "Add tags to entry

   Args:
     manager: Archival manager
     entry-id: Entry ID
     new-tags: List of tags to add

   Returns:
     Updated entry"

  (define entry (archival-get manager entry-id))
  (when entry
    (define current-tags (hash-ref entry 'tags '()))
    (define updated-tags (append current-tags new-tags))
    (archival-update! manager entry-id (hash 'tags updated-tags))))

;;; ============================================================================
;;; Archival Entry Deletion
;;; ============================================================================

(define (archival-delete! manager entry-id)
  "Delete archival entry

   Args:
     manager: Archival manager
     entry-id: Entry ID"

  ;; Delete via database
  (db-delete-archival-entry (archival-manager-agent-id manager) entry-id)

  ;; Remove from cache
  (when (archival-manager-cache-enabled manager)
    (archival-cache-invalidate! manager entry-id))

  (displayln (format "Archival entry deleted: ~a" entry-id)))

(define (archival-delete-batch! manager entry-ids)
  "Delete multiple archival entries

   Args:
     manager: Archival manager
     entry-ids: List of entry IDs"

  (for-each (lambda (entry-id)
              (archival-delete! manager entry-id))
            entry-ids))

(define (archival-clear! manager)
  "Clear all archival entries for agent

   Args:
     manager: Archival manager"

  ;; Delete all entries
  (define entries (archival-get-all manager #:limit 10000))
  (for-each (lambda (entry)
              (archival-delete! manager (hash-ref entry 'id)))
            entries)

  ;; Clear cache
  (when (archival-manager-cache-enabled manager)
    (archival-cache-clear! manager))

  (displayln "Archival memory cleared"))

;;; ============================================================================
;;; Archival Statistics
;;; ============================================================================

(define (archival-count manager)
  "Count total archival entries

   Args:
     manager: Archival manager

   Returns:
     Entry count"

  (length (archival-get-all manager #:limit 100000)))

(define (archival-total-size manager)
  "Calculate total size of archival memory

   Args:
     manager: Archival manager

   Returns:
     Total size in bytes"

  (define entries (archival-get-all manager #:limit 100000))
  (apply + (map archival-entry-size entries)))

(define (archival-get-stats manager)
  "Get archival memory statistics

   Args:
     manager: Archival manager

   Returns:
     Statistics hash"

  (define entries (archival-get-all manager #:limit 100000))
  (hash
   'total_entries (length entries)
   'total_size (apply + (map archival-entry-size entries))
   'avg_importance (if (null? entries)
                        0.0
                        (/ (apply + (map (lambda (e)
                                          (hash-ref e 'importance 0.5))
                                        entries))
                           (length entries)))
   'entries_with_embeddings (length (filter (lambda (e)
                                               (hash-ref e 'embedding #f))
                                             entries))
   'unique_tags (length (remove-duplicates
                         (apply append
                               (map (lambda (e)
                                     (hash-ref e 'tags '()))
                                   entries))))))

;;; ============================================================================
;;; Archival Caching
;;; ============================================================================

(define (archival-cache-get manager entry-id)
  "Get entry from cache

   Args:
     manager: Archival manager
     entry-id: Entry ID

   Returns:
     Cached entry or #f"

  (hash-ref (archival-manager-cache manager) entry-id #f))

(define (archival-cache-put! manager entry-id entry)
  "Put entry in cache

   Args:
     manager: Archival manager
     entry-id: Entry ID
     entry: Entry data"

  (set-archival-manager-cache! manager
                                (hash-set (archival-manager-cache manager)
                                          entry-id
                                          entry)))

(define (archival-cache-invalidate! manager entry-id)
  "Invalidate cached entry

   Args:
     manager: Archival manager
     entry-id: Entry ID"

  (set-archival-manager-cache! manager
                                (hash-remove (archival-manager-cache manager)
                                             entry-id)))

(define (archival-cache-clear! manager)
  "Clear archival cache

   Args:
     manager: Archival manager"

  (set-archival-manager-cache! manager (hash)))

;;; ============================================================================
;;; Export/Import
;;; ============================================================================

(define (archival-export manager #:format [format 'json] #:include-embeddings? [include-embeddings? #f])
  "Export archival memory

   Args:
     manager: Archival manager
     format: Export format (json or text)
     include-embeddings?: Include embedding vectors (default: #f)

   Returns:
     Exported data as string"

  (define entries (archival-get-all manager #:limit 100000))
  (case format
    [(json)
     (define export-entries
       (if include-embeddings?
           entries
           (map (lambda (e)
                  (hash-remove e 'embedding))
                entries)))
     (jsexpr->string
      (hash 'agent_id (archival-manager-agent-id manager)
            'entries export-entries))]
    [(text)
     (string-join
      (map (lambda (e)
             (format "[~a] (importance: ~a)~a~a~a~a"
                    (hash-ref e 'id)
                    (hash-ref e 'importance 0.5)
                    (if (null? (hash-ref e 'tags '()))
                        ""
                        (format " [tags: ~a]" (hash-ref e 'tags)))
                        "\n"
                        (hash-ref e 'content)
                        "\n"))
           entries)
      "\n")]
    [else
     (error 'archival-export "Unsupported export format" format)]))

(define (archival-import! manager data)
  "Import archival memory

   Args:
     manager: Archival manager
     data: Exported data (hash with entries)"

  (define entries (hash-ref data 'entries))
  (for-each
   (lambda (entry)
     (archival-insert! manager
                       (hash-ref entry 'content)
                       #:importance (hash-ref entry 'importance 0.5)
                       #:tags (hash-ref entry 'tags '())
                       #:generate-embedding? (not (hash-ref entry 'embedding #f))))
   entries))

;;; ============================================================================
;;; Database Helper Functions (Placeholder)
;;; ============================================================================

(define (db-create-archival-entry agent-id data)
  "Create archival entry (placeholder)"
  (hash 'id (format "~a-~a" (current-seconds) (random 1000000))
        'agent_id agent-id
        'content (hash-ref data 'content)
        'embedding (hash-ref data 'embedding #f)
        'importance (hash-ref data 'importance 0.5)
        'tags (hash-ref data 'tags '())
        'created_at (current-seconds)))

(define (db-get-archival-entry agent-id entry-id)
  "Get archival entry (placeholder)"
  #f)

(define (db-get-archival-entries agent-id #:limit [limit 100] #:offset [offset 0])
  "Get archival entries (placeholder)"
  '())

(define (db-update-archival-entry agent-id entry-id updates)
  "Update archival entry (placeholder)"
  (void))

(define (db-delete-archival-entry agent-id entry-id)
  "Delete archival entry (placeholder)"
  (void))

(define (validate-archival-params params)
  "Validate archival parameters (placeholder)"
  (cons #t #f))

(define (string-contains? str substr)
  "Check if string contains substring"
  (not (equal? (substring (string-append str substr) 0 (string-length substr))
               substr)))

(define (archival-entry-size entry)
  "Get entry size in bytes (placeholder)"
  (string-length (hash-ref entry 'content "")))
