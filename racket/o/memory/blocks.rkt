#lang racket

;;; memory/blocks.rkt - Memory Block Operations
;;;
;;; High-level memory block operations for core memory management.

(provide create-block-manager
         block-create!
         block-create-from-template!
         block-ensure-exists!
         block-get
         block-get-all
         block-get-value
         block-get-standard
         block-get-custom
         block-update!
         block-append!
         block-replace!
         block-set-readonly!
         block-delete!
         block-exists?
         block-validate
         block-validate-all
         block-cache-get
         block-cache-put!
         block-cache-invalidate!
         block-cache-clear!
         core-memory-get
         core-memory-set!
         core-memory-initialize!
         block-search
         block-search-by-label
         block-count
         block-count-standard
         block-count-custom
         block-total-size
         block-get-stats
         block-export
         block-import!
         block-list-labels
         block-get-by-labels
         block-copy!
         (struct-out block-manager))

(require racket/hash
        racket/format
        racket/list
        racket/bool
        json
        "types.rkt")

;;; ============================================================================
;;; Database Stub Functions (placeholder until socket layer is implemented)
;;; ============================================================================

(define (db-create-memory-block agent-id params)
  "Create memory block (stub - returns mock data)"
  (hash 'id (uuid-generate)
        'agent_id agent-id
        'label (hash-ref params 'label)
        'value (hash-ref params 'value)
        'is_template (hash-ref params 'is_template #f)
        'is_readonly (hash-ref params 'is_readonly #f)
        'created_at (current-seconds)
        'updated_at (current-seconds)))

(define (db-get-memory-blocks agent-id)
  "Get all memory blocks for agent (stub - returns empty list)"
  '())

(define (db-update-memory-block agent-id label value)
  "Update memory block (stub - returns success)"
  (hash 'success #t))

(define (db-delete-memory-block agent-id label)
  "Delete memory block (stub - returns success)"
  (hash 'success #t))

(define (db-connect!)
  "Connect to database (stub - no-op)"
  (void))

(define (block-template-labels)
  "Get standard block template labels"
  '("persona" "human"))

(define (uuid-generate)
  "Generate UUID (simple implementation)"
  (format "~a-~a" (current-seconds) (random 1000000)))

;;; ============================================================================
;;; Memory Block Manager
;;; ============================================================================

(struct block-manager
  (agent-id      ; Agent ID
   cache         ; Block cache
   cache-enabled) ; Is caching enabled?
  #:transparent)

(define (create-block-manager agent-id [cache-enabled #t])
  "Create a new block manager for an agent"
  (block-manager agent-id (make-hash) cache-enabled))

;;; ============================================================================
;;; Block Creation
;;; ============================================================================

(define (block-create! manager label value
                        #:is-template [is-template #f]
                        #:is-readonly [is-readonly #f]
                        #:validate? [validate? #t])
  "Create a new memory block"
  ;; Validate block parameters if requested
  (when validate?
    (define params (hash 'label label 'value value))
    (define result (validate-block-params params))
    (unless (car result)
      (error 'block-create! "Invalid block parameters: ~a" (cdr result))))

  ;; Create block via database
  (define block
    (db-create-memory-block
     (block-manager-agent-id manager)
     (hash 'label label
           'value value
           'is_template is-template
           'is_readonly is-readonly)))

  ;; Add to cache
  (when (block-manager-cache-enabled manager)
    (block-cache-put! manager label block))

  (displayln (format "Block created: ~a" label))
  block)

(define (block-create-from-template! manager label)
  "Create block from template"
  (define template (get-block-template label))
  (block-create! manager label template
                 #:is-template #t
                 #:is-readonly #f))

(define (block-ensure-exists! manager label)
  "Ensure block exists, create from template if not"
  (if (block-exists? manager label)
      (block-get manager label)
      (block-create-from-template! manager label)))

;;; ============================================================================
;;; Block Retrieval
;;; ============================================================================

(define (block-get manager label)
  "Get memory block by label"
  ;; Check cache first
  (if (block-manager-cache-enabled manager)
      (or (block-cache-get manager label)
          (let ([blocks (db-get-memory-blocks (block-manager-agent-id manager))])
            ;; Find block with matching label
            (define found (findf (lambda (b) (equal? (hash-ref b 'label) label))
                               blocks))
            (when found
              (block-cache-put! manager label found))
            found))
      ;; No cache, query database
      (let ([blocks (db-get-memory-blocks (block-manager-agent-id manager))])
        (findf (lambda (b) (equal? (hash-ref b 'label) label))
              blocks))))

(define (block-get-all manager)
  "Get all memory blocks"
  (db-get-memory-blocks (block-manager-agent-id manager)))

(define (block-get-value manager label)
  "Get block value by label"
  (define block (block-get manager label))
  (if block
      (hash-ref block 'value)
      #f))

(define (block-get-standard manager)
  "Get standard blocks (persona, human)"
  (define blocks (block-get-all manager))
  (filter (lambda (b)
            (standard-block? (hash-ref b 'label)))
          blocks))

(define (block-get-custom manager)
  "Get custom blocks"
  (define blocks (block-get-all manager))
  (filter (lambda (b)
            (not (standard-block? (hash-ref b 'label))))
          blocks))

;;; ============================================================================
;;; Block Update
;;; ============================================================================

(define (block-update! manager label value #:validate? [validate? #t])
  "Update memory block value"
  ;; Check if block is read-only
  (define block (block-get manager label))
  (when (and block (is-readonly-block? block))
    (error 'block-update! "Cannot update read-only block: ~a" label))

  ;; Validate value if requested
  (when validate?
    (unless (string? value)
      (error 'block-update! "Value must be a string")))

  ;; Update block via database
  (db-update-memory-block (block-manager-agent-id manager) label value)

  ;; Update cache
  (when (block-manager-cache-enabled manager)
    (block-cache-invalidate! manager label))

  (displayln (format "Block updated: ~a" label)))

(define (block-append! manager label text #:separator [separator "\n"])
  "Append text to block value"
  (define current (block-get-value manager label))
  (if current
      (let ([new-value (string-append current separator text)])
        (block-update! manager label new-value))
      (error 'block-append! "Block not found: ~a" label)))

(define (block-replace! manager label old-text new-text)
  "Replace text in block value"
  (define current (block-get-value manager label))
  (if current
      (let ([new-value (string-replace current old-text new-text)])
        (block-update! manager label new-value))
      (error 'block-replace! "Block not found: ~a" label)))

(define (block-set-readonly! manager label readonly?)
  "Set block read-only status"
  ;; Note: This requires database support for updating is_readonly
  ;; For now, we'll just update the cache
  (define block (block-get manager label))
  (when block
    (hash-set*! block 'is_readonly readonly?)
    (when (block-manager-cache-enabled manager)
      (block-cache-put! manager label block))
    (displayln (format "Block ~a read-only: ~a" label readonly?))))

;;; ============================================================================
;;; Block Deletion
;;; ============================================================================

(define (block-delete! manager label)
  "Delete memory block"
  ;; Check if block is standard block
  (when (standard-block? label)
    (error 'block-delete! "Cannot delete standard block: ~a" label))

  ;; Check if block is read-only
  (define block (block-get manager label))
  (when (and block (is-readonly-block? block))
    (error 'block-delete! "Cannot delete read-only block: ~a" label))

  ;; Delete block via database
  (db-delete-memory-block (block-manager-agent-id manager) label)

  ;; Remove from cache
  (when (block-manager-cache-enabled manager)
    (block-cache-invalidate! manager label))

  (displayln (format "Block deleted: ~a" label)))

;;; ============================================================================
;;; Block Validation
;;; ============================================================================

(define (block-exists? manager label)
  "Check if block exists"
  (define block (block-get manager label))
  (and block #t))

(define (block-validate manager label)
  "Validate block"
  (define block (block-get manager label))
  (if block
      (validate-block-params (hash 'label label
                                   'value (hash-ref block 'value)))
      (cons #f (list "Block not found"))))

(define (block-validate-all manager)
  "Validate all blocks"
  (define blocks (block-get-all manager))
  (define errors '())

  (for ([block blocks])
    (define result (validate-block-params
                    (hash 'label (hash-ref block 'label)
                          'value (hash-ref block 'value))))
    (unless (car result)
      (set! errors (cons (cons (hash-ref block 'label) (cdr result))
                        errors))))

  (if (null? errors)
      (cons #t #f)
      (cons #f (reverse errors))))

;;; ============================================================================
;;; Block Caching
;;; ============================================================================

(define (block-cache-get manager label)
  "Get block from cache"
  (hash-ref (block-manager-cache manager) label #f))

(define (block-cache-put! manager label block)
  "Put block in cache"
  (hash-set! (block-manager-cache manager) label block))

(define (block-cache-invalidate! manager label)
  "Invalidate cached block"
  (hash-remove! (block-manager-cache manager) label))

(define (block-cache-clear! manager)
  "Clear block cache"
  (define cache (block-manager-cache manager))
  ;; Clear the hash
  (for ([key (hash-keys cache)])
    (hash-remove! cache key)))

;;; ============================================================================
;;; Core Memory Operations
;;; ============================================================================

(define (core-memory-get manager)
  "Get core memory structure"
  (define blocks (block-get-all manager))
  (define persona-block (findf (lambda (b) (equal? (hash-ref b 'label) "persona"))
                              blocks))
  (define human-block (findf (lambda (b) (equal? (hash-ref b 'label) "human"))
                            blocks))
  (define custom-blocks (filter (lambda (b)
                                  (not (standard-block? (hash-ref b 'label))))
                                blocks))

  (core-memory
   (if persona-block (hash-ref persona-block 'value) "")
   (if human-block (hash-ref human-block 'value) "")
   (make-hash (map (lambda (b)
                     (cons (hash-ref b 'label) (hash-ref b 'value)))
                   custom-blocks))))

(define (core-memory-set! manager memory)
  "Set core memory structure"
  ;; Update persona
  (block-update! manager "persona" (core-memory-persona memory))

  ;; Update human
  (block-update! manager "human" (core-memory-human memory))

  ;; Update custom blocks
  (for ([(label value) (in-hash (core-memory-custom memory))])
    (if (block-exists? manager label)
        (block-update! manager label value)
        (block-create! manager label value))))

(define (core-memory-initialize! manager
                                 #:persona [persona #f]
                                 #:human [human #f])
  "Initialize core memory with default or provided values"
  ;; Create persona block
  (block-create! manager "persona"
                (or persona default-persona-template)
                #:is-template #t)

  ;; Create human block
  (block-create! manager "human"
                (or human default-human-template)
                #:is-template #t))

;;; ============================================================================
;;; Block Search
;;; ============================================================================

(define (block-search manager query)
  "Search blocks by content"
  (define blocks (block-get-all manager))
  (filter (lambda (b)
            (string-contains? (hash-ref b 'value) query))
          blocks))

(define (block-search-by-label manager pattern)
  "Search blocks by label pattern"
  (define blocks (block-get-all manager))
  (filter (lambda (b)
            (string-contains? (hash-ref b 'label) pattern))
          blocks))

;;; ============================================================================
;;; Block Statistics
;;; ============================================================================

(define (block-count manager)
  "Count total blocks"
  (length (block-get-all manager)))

(define (block-count-standard manager)
  "Count standard blocks"
  (length (block-get-standard manager)))

(define (block-count-custom manager)
  "Count custom blocks"
  (length (block-get-custom manager)))

(define (block-total-size manager)
  "Calculate total size of all blocks"
  (define blocks (block-get-all manager))
  (apply + (map memory-block-size blocks)))

(define (block-get-stats manager)
  "Get block statistics"
  (hash 'total_blocks (block-count manager)
        'standard_blocks (block-count-standard manager)
        'custom_blocks (block-count-custom manager)
        'total_size (block-total-size manager)))

;;; ============================================================================
;;; Block Export/Import
;;; ============================================================================

(define (block-export manager #:format [fmt 'json])
  "Export all blocks"
  (define blocks (block-get-all manager))
  (match fmt
    ['json
     (jsexpr->string
      (hash 'agent_id (block-manager-agent-id manager)
            'blocks blocks))]
    ['text
     (string-join
      (map (lambda (b)
             (format "[~a]\n~a\n"
                    (hash-ref b 'label)
                    (hash-ref b 'value)))
           blocks)
      "\n")]
    [else
     (error 'block-export "Unsupported export format: ~a" fmt)]))

(define (block-import! manager blocks-data)
  "Import blocks from exported data"
  (define blocks (hash-ref blocks-data 'blocks))
  (for ([block blocks])
    (define label (hash-ref block 'label))
    (define value (hash-ref block 'value))
    (if (block-exists? manager label)
        (block-update! manager label value)
        (block-create! manager label value))))

;;; ============================================================================
;;; Block Utilities
;;; ============================================================================

(define (block-list-labels manager)
  "List all block labels"
  (map (lambda (b) (hash-ref b 'label))
       (block-get-all manager)))

(define (block-get-by-labels manager labels)
  "Get blocks by list of labels"
  (filter-map (lambda (label)
                (block-get manager label))
              labels))

(define (block-copy! manager from-label to-label)
  "Copy block to new label"
  (define value (block-get-value manager from-label))
  (if value
      (block-create! manager to-label value)
      (error 'block-copy! "Source block not found: ~a" from-label)))
