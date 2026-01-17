;;; memory/archival.ss - Archival Memory Operations
;;;
;;; Long-term memory storage with semantic search and embeddings.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/text/json
  :std/sort
  :o/database/client
  :o/llm/client
  :o/memory/types)

;;; ============================================================================
;;; Archival Memory Manager
;;; ============================================================================

(defstruct archival-manager
  (agent-id         ; Agent ID
   llm-provider     ; LLM provider for embeddings
   llm-model        ; LLM model for embeddings
   cache            ; Entry cache
   cache-enabled)   ; Is caching enabled?
  transparent: #t)

(def (make-archival-manager agent-id
                            #!key
                            (llm-provider :openai)
                            (llm-model "text-embedding-3-small")
                            (cache-enabled #t))
  "Create archival memory manager

   Args:
     agent-id: Agent ID
     llm-provider: LLM provider for embeddings (default: :openai)
     llm-model: Embedding model (default: text-embedding-3-small)
     cache-enabled: Enable caching (default: #t)

   Returns:
     Archival manager"

  (make-archival-manager
   agent-id: agent-id
   llm-provider: llm-provider
   llm-model: llm-model
   cache: (hash)
   cache-enabled: cache-enabled))

;;; ============================================================================
;;; Embedding Generation
;;; ============================================================================

(def (generate-embedding manager content)
  "Generate embedding vector for content

   Args:
     manager: Archival manager
     content: Text content to embed

   Returns:
     Embedding vector (list of floats, 1536 dimensions)"

  (let ((provider (archival-manager-llm-provider manager))
        (model (archival-manager-llm-model manager)))

    ;; Call LLM API to generate embedding
    (try
     (let ((response (llm-generate-embedding provider model content)))
       (if response
           response
           (error "Failed to generate embedding")))
     (catch (e)
       (displayln (format "Embedding generation error: ~a" (error-message e)))
       ;; Return zero vector as fallback
       (make-list 1536 0.0)))))

(def (generate-embeddings-batch manager contents)
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

(def (archival-insert! manager content
                       #!key
                       (importance 0.5)
                       (tags '())
                       (generate-embedding? #t))
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
  (let ((params (hash 'content content
                      'importance importance
                      'tags tags)))
    (let ((result (validate-archival-params params)))
      (unless (car result)
        (error "Invalid archival parameters" errors: (cdr result)))))

  ;; Generate embedding if requested
  (let ((embedding (if generate-embedding?
                       (generate-embedding manager content)
                       #f)))

    ;; Create entry via database
    (let ((entry (db-create-archival-entry
                  (archival-manager-agent-id manager)
                  (hash 'content content
                        'embedding embedding
                        'importance importance
                        'tags tags))))

      ;; Add to cache
      (when (archival-manager-cache-enabled manager)
        (archival-cache-put! manager (hash-ref entry 'id) entry))

      (displayln (format "Archival entry created: ~a" (hash-ref entry 'id)))
      entry)))

(def (archival-insert-batch! manager entries)
  "Insert multiple entries into archival memory

   Args:
     manager: Archival manager
     entries: List of entry hashes with content, importance, tags

   Returns:
     List of created entries"

  (map (lambda (entry-data)
         (archival-insert! manager
                          (hash-ref entry-data 'content)
                          importance: (hash-ref entry-data 'importance 0.5)
                          tags: (hash-ref entry-data 'tags '())
                          generate-embedding?: (hash-ref entry-data 'generate_embedding #t)))
       entries))

;;; ============================================================================
;;; Archival Entry Retrieval
;;; ============================================================================

(def (archival-get manager entry-id)
  "Get archival entry by ID

   Args:
     manager: Archival manager
     entry-id: Entry ID

   Returns:
     Entry or #f if not found"

  ;; Check cache first
  (if (archival-manager-cache-enabled manager)
      (or (archival-cache-get manager entry-id)
          (let ((entry (db-get-archival-entry (archival-manager-agent-id manager)
                                               entry-id)))
            (when entry
              (archival-cache-put! manager entry-id entry))
            entry))
      ;; No cache, query database
      (db-get-archival-entry (archival-manager-agent-id manager) entry-id)))

(def (archival-get-all manager #!key (limit 100) (offset 0))
  "Get all archival entries

   Args:
     manager: Archival manager
     limit: Maximum entries to return (default: 100)
     offset: Offset for pagination (default: 0)

   Returns:
     List of entries"

  (db-get-archival-entries (archival-manager-agent-id manager)
                           limit: limit
                           offset: offset))

(def (archival-get-recent manager n)
  "Get N most recent archival entries

   Args:
     manager: Archival manager
     n: Number of entries

   Returns:
     List of entries"

  (archival-get-all manager limit: n offset: 0))

;;; ============================================================================
;;; Text-Based Search
;;; ============================================================================

(def (archival-search manager query #!key (limit 10))
  "Search archival memory by text content

   Args:
     manager: Archival manager
     query: Search query
     limit: Maximum results (default: 10)

   Returns:
     List of matching entries"

  (let ((all-entries (archival-get-all manager limit: 1000)))
    ;; Filter entries containing query
    (let ((matches (filter (lambda (entry)
                            (string-contains (hash-ref entry 'content) query))
                          all-entries)))
      ;; Sort by importance (descending)
      (let ((sorted (sort matches
                         (lambda (a b)
                           (> (hash-ref a 'importance 0.5)
                              (hash-ref b 'importance 0.5))))))
        ;; Return top N results
        (take sorted (min limit (length sorted)))))))

(def (archival-search-by-tags manager tags #!key (limit 10))
  "Search archival memory by tags

   Args:
     manager: Archival manager
     tags: List of tags to search for
     limit: Maximum results (default: 10)

   Returns:
     List of matching entries"

  (let ((all-entries (archival-get-all manager limit: 1000)))
    ;; Filter entries with matching tags
    (let ((matches (filter (lambda (entry)
                            (let ((entry-tags (hash-ref entry 'tags '())))
                              ;; Check if any search tag is in entry tags
                              (any (lambda (tag)
                                     (member tag entry-tags))
                                   tags)))
                          all-entries)))
      ;; Sort by importance
      (let ((sorted (sort matches
                         (lambda (a b)
                           (> (hash-ref a 'importance 0.5)
                              (hash-ref b 'importance 0.5))))))
        (take sorted (min limit (length sorted)))))))

(def (archival-search-by-importance manager min-importance #!key (limit 10))
  "Search archival memory by importance threshold

   Args:
     manager: Archival manager
     min-importance: Minimum importance score
     limit: Maximum results (default: 10)

   Returns:
     List of matching entries"

  (let ((all-entries (archival-get-all manager limit: 1000)))
    ;; Filter entries above importance threshold
    (let ((matches (filter (lambda (entry)
                            (>= (hash-ref entry 'importance 0.5) min-importance))
                          all-entries)))
      ;; Sort by importance (descending)
      (let ((sorted (sort matches
                         (lambda (a b)
                           (> (hash-ref a 'importance 0.5)
                              (hash-ref b 'importance 0.5))))))
        (take sorted (min limit (length sorted)))))))

;;; ============================================================================
;;; Pagination
;;; ============================================================================

(defstruct archival-page
  (entries          ; List of entries
   total            ; Total number of entries
   page             ; Current page number
   page-size        ; Entries per page
   has-next?        ; Has next page?
   has-prev?)       ; Has previous page?
  transparent: #t)

(def (archival-get-page manager page-number page-size)
  "Get paginated archival entries

   Args:
     manager: Archival manager
     page-number: Page number (0-indexed)
     page-size: Entries per page

   Returns:
     Archival page result"

  (let* ((offset (* page-number page-size))
         (entries (archival-get-all manager limit: page-size offset: offset))
         (total (archival-count manager))
         (has-next? (< (+ offset page-size) total))
         (has-prev? (> page-number 0)))

    (make-archival-page
     entries: entries
     total: total
     page: page-number
     page-size: page-size
     has-next?: has-next?
     has-prev?: has-prev?)))

(def (archival-get-next-page page-result)
  "Get next page number

   Args:
     page-result: Current page result

   Returns:
     Next page number or #f"

  (if (archival-page-has-next? page-result)
      (+ (archival-page-page page-result) 1)
      #f))

(def (archival-get-prev-page page-result)
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

(def (archival-update! manager entry-id updates)
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

(def (archival-update-importance! manager entry-id importance)
  "Update entry importance

   Args:
     manager: Archival manager
     entry-id: Entry ID
     importance: New importance score

   Returns:
     Updated entry"

  (archival-update! manager entry-id (hash 'importance importance)))

(def (archival-add-tags! manager entry-id new-tags)
  "Add tags to entry

   Args:
     manager: Archival manager
     entry-id: Entry ID
     new-tags: List of tags to add

   Returns:
     Updated entry"

  (let ((entry (archival-get manager entry-id)))
    (when entry
      (let ((current-tags (hash-ref entry 'tags '())))
        (let ((updated-tags (append current-tags new-tags)))
          (archival-update! manager entry-id (hash 'tags updated-tags)))))))

;;; ============================================================================
;;; Archival Entry Deletion
;;; ============================================================================

(def (archival-delete! manager entry-id)
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

(def (archival-delete-batch! manager entry-ids)
  "Delete multiple archival entries

   Args:
     manager: Archival manager
     entry-ids: List of entry IDs"

  (for-each (lambda (entry-id)
              (archival-delete! manager entry-id))
            entry-ids))

(def (archival-clear! manager)
  "Clear all archival entries for agent

   Args:
     manager: Archival manager"

  ;; Delete all entries
  (let ((entries (archival-get-all manager limit: 10000)))
    (for-each (lambda (entry)
                (archival-delete! manager (hash-ref entry 'id)))
              entries))

  ;; Clear cache
  (when (archival-manager-cache-enabled manager)
    (archival-cache-clear! manager))

  (displayln "Archival memory cleared"))

;;; ============================================================================
;;; Archival Statistics
;;; ============================================================================

(def (archival-count manager)
  "Count total archival entries

   Args:
     manager: Archival manager

   Returns:
     Entry count"

  (length (archival-get-all manager limit: 100000)))

(def (archival-total-size manager)
  "Calculate total size of archival memory

   Args:
     manager: Archival manager

   Returns:
     Total size in bytes"

  (let ((entries (archival-get-all manager limit: 100000)))
    (apply + (map archival-entry-size entries))))

(def (archival-get-stats manager)
  "Get archival memory statistics

   Args:
     manager: Archival manager

   Returns:
     Statistics hash"

  (let ((entries (archival-get-all manager limit: 100000)))
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
                                    entries)))))))

;;; ============================================================================
;;; Archival Caching
;;; ============================================================================

(def (archival-cache-get manager entry-id)
  "Get entry from cache

   Args:
     manager: Archival manager
     entry-id: Entry ID

   Returns:
     Cached entry or #f"

  (hash-ref (archival-manager-cache manager) entry-id #f))

(def (archival-cache-put! manager entry-id entry)
  "Put entry in cache

   Args:
     manager: Archival manager
     entry-id: Entry ID
     entry: Entry data"

  (hash-put! (archival-manager-cache manager) entry-id entry))

(def (archival-cache-invalidate! manager entry-id)
  "Invalidate cached entry

   Args:
     manager: Archival manager
     entry-id: Entry ID"

  (hash-remove! (archival-manager-cache manager) entry-id))

(def (archival-cache-clear! manager)
  "Clear archival cache

   Args:
     manager: Archival manager"

  (hash-clear! (archival-manager-cache manager)))

;;; ============================================================================
;;; Export/Import
;;; ============================================================================

(def (archival-export manager #!key (format 'json) (include-embeddings? #f))
  "Export archival memory

   Args:
     manager: Archival manager
     format: Export format (json or text)
     include-embeddings?: Include embedding vectors (default: #f)

   Returns:
     Exported data as string"

  (let ((entries (archival-get-all manager limit: 100000)))
    (case format
      ((json)
       (let ((export-entries
              (if include-embeddings?
                  entries
                  (map (lambda (e)
                         (let ((copy (hash-copy e)))
                           (hash-remove! copy 'embedding)
                           copy))
                       entries))))
         (json-object->string
          (hash 'agent_id (archival-manager-agent-id manager)
                'entries export-entries))))

      ((text)
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
        "\n"))

      (else
       (error "Unsupported export format" format: format)))))

(def (archival-import! manager data)
  "Import archival memory

   Args:
     manager: Archival manager
     data: Exported data (hash with entries)"

  (let ((entries (hash-ref data 'entries)))
    (for-each
     (lambda (entry)
       (archival-insert! manager
                        (hash-ref entry 'content)
                        importance: (hash-ref entry 'importance 0.5)
                        tags: (hash-ref entry 'tags '())
                        generate-embedding?: (not (hash-ref entry 'embedding #f))))
     entries)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create archival manager
(def manager (make-archival-manager agent-id))

;; Insert entry
(archival-insert! manager "Important information about user preferences."
                 importance: 0.8
                 tags: '("preferences" "important"))

;; Search by text
(def results (archival-search manager "preferences" limit: 5))

;; Search by tags
(def tagged (archival-search-by-tags manager '("important") limit: 5))

;; Get paginated results
(def page (archival-get-page manager 0 20))
(displayln (format "Page ~a of ~a entries"
                   (archival-page-page page)
                   (archival-page-total page)))

;; Get statistics
(def stats (archival-get-stats manager))
(displayln (format "Total entries: ~a" (hash-ref stats 'total_entries)))

;; Export
(def export (archival-export manager format: 'json))
(displayln export)
|#
