;;; memory/blocks.ss - Memory Block Operations
;;;
;;; High-level memory block operations for core memory management.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  ../database/client
  ./types)

;;; ============================================================================
;;; Memory Block Manager
;;; ============================================================================

(defstruct block-manager
  (agent-id      ; Agent ID
   cache         ; Block cache
   cache-enabled) ; Is caching enabled?
  transparent: #t)

(def (make-block-manager agent-id #!key (cache-enabled #t))
  "Create a new block manager for an agent"
  (make-block-manager
   agent-id: agent-id
   cache: (hash)
   cache-enabled: cache-enabled))

;;; ============================================================================
;;; Block Creation
;;; ============================================================================

(def (block-create! manager label value
                    #!key
                    (is-template #f)
                    (is-readonly #f)
                    (validate? #t))
  "Create a new memory block"

  ;; Validate block parameters if requested
  (when validate?
    (let ((params (hash 'label label 'value value)))
      (let ((result (validate-block-params params)))
        (unless (car result)
          (error "Invalid block parameters" errors: (cdr result))))))

  ;; Create block via database
  (let ((block (db-create-memory-block (block-manager-agent-id manager)
                                       (hash 'label label
                                             'value value
                                             'is_template is-template
                                             'is_readonly is-readonly))))

    ;; Add to cache
    (when (block-manager-cache-enabled manager)
      (block-cache-put! manager label block))

    (displayln (format "Block created: ~a" label))
    block))

(def (block-create-from-template! manager label)
  "Create block from template"
  (let ((template (get-block-template label)))
    (block-create! manager label template
                  is-template: #t
                  is-readonly: #f)))

(def (block-ensure-exists! manager label)
  "Ensure block exists, create from template if not"
  (if (block-exists? manager label)
      (block-get manager label)
      (block-create-from-template! manager label)))

;;; ============================================================================
;;; Block Retrieval
;;; ============================================================================

(def (block-get manager label)
  "Get memory block by label"
  ;; Check cache first
  (if (block-manager-cache-enabled manager)
      (or (block-cache-get manager label)
          (let ((block (db-get-memory-blocks (block-manager-agent-id manager))))
            ;; Find block with matching label
            (let ((found (find (lambda (b) (equal? (hash-ref b 'label) label))
                              block)))
              (when found
                (block-cache-put! manager label found))
              found)))
      ;; No cache, query database
      (let ((blocks (db-get-memory-blocks (block-manager-agent-id manager))))
        (find (lambda (b) (equal? (hash-ref b 'label) label))
              blocks))))

(def (block-get-all manager)
  "Get all memory blocks"
  (db-get-memory-blocks (block-manager-agent-id manager)))

(def (block-get-value manager label)
  "Get block value by label"
  (let ((block (block-get manager label)))
    (if block
        (hash-ref block 'value)
        #f)))

(def (block-get-standard manager)
  "Get standard blocks (persona, human)"
  (let ((blocks (block-get-all manager)))
    (filter (lambda (b)
              (standard-block? (hash-ref b 'label)))
            blocks)))

(def (block-get-custom manager)
  "Get custom blocks"
  (let ((blocks (block-get-all manager)))
    (filter (lambda (b)
              (not (standard-block? (hash-ref b 'label))))
            blocks)))

;;; ============================================================================
;;; Block Update
;;; ============================================================================

(def (block-update! manager label value #!key (validate? #t))
  "Update memory block value"

  ;; Check if block is read-only
  (let ((block (block-get manager label)))
    (when (and block (is-readonly-block? block))
      (error "Cannot update read-only block" label: label)))

  ;; Validate value if requested
  (when validate?
    (unless (string? value)
      (error "Value must be a string")))

  ;; Update block via database
  (db-update-memory-block (block-manager-agent-id manager) label value)

  ;; Update cache
  (when (block-manager-cache-enabled manager)
    (block-cache-invalidate! manager label))

  (displayln (format "Block updated: ~a" label)))

(def (block-append! manager label text #!key (separator "\n"))
  "Append text to block value"
  (let ((current (block-get-value manager label)))
    (if current
        (let ((new-value (string-append current separator text)))
          (block-update! manager label new-value))
        (error "Block not found" label: label))))

(def (block-replace! manager label old-text new-text)
  "Replace text in block value"
  (let ((current (block-get-value manager label)))
    (if current
        (let ((new-value (string-replace current old-text new-text)))
          (block-update! manager label new-value))
        (error "Block not found" label: label))))

(def (block-set-readonly! manager label readonly?)
  "Set block read-only status"
  ;; Note: This requires database support for updating is_readonly
  ;; For now, we'll just update the cache
  (let ((block (block-get manager label)))
    (when block
      (hash-put! block 'is_readonly readonly?)
      (when (block-manager-cache-enabled manager)
        (block-cache-put! manager label block))
      (displayln (format "Block ~a read-only: ~a" label readonly?)))))

;;; ============================================================================
;;; Block Deletion
;;; ============================================================================

(def (block-delete! manager label)
  "Delete memory block"

  ;; Check if block is standard block
  (when (standard-block? label)
    (error "Cannot delete standard block" label: label))

  ;; Check if block is read-only
  (let ((block (block-get manager label)))
    (when (and block (is-readonly-block? block))
      (error "Cannot delete read-only block" label: label)))

  ;; Delete block via database
  ;; Note: db-delete-memory-block not implemented yet
  ;; For now, we'll just remove from cache
  (when (block-manager-cache-enabled manager)
    (block-cache-invalidate! manager label))

  (displayln (format "Block deleted: ~a" label)))

;;; ============================================================================
;;; Block Validation
;;; ============================================================================

(def (block-exists? manager label)
  "Check if block exists"
  (let ((block (block-get manager label)))
    (and block #t)))

(def (block-validate manager label)
  "Validate block"
  (let ((block (block-get manager label)))
    (if block
        (validate-block-params (hash 'label label
                                     'value (hash-ref block 'value)))
        (cons #f (list "Block not found")))))

(def (block-validate-all manager)
  "Validate all blocks"
  (let ((blocks (block-get-all manager))
        (errors '()))

    (for-each
     (lambda (block)
       (let ((result (validate-block-params
                      (hash 'label (hash-ref block 'label)
                            'value (hash-ref block 'value)))))
         (unless (car result)
           (set! errors (cons (cons (hash-ref block 'label) (cdr result))
                             errors)))))
     blocks)

    (if (null? errors)
        (cons #t #f)
        (cons #f errors))))

;;; ============================================================================
;;; Block Caching
;;; ============================================================================

(def (block-cache-get manager label)
  "Get block from cache"
  (hash-ref (block-manager-cache manager) label #f))

(def (block-cache-put! manager label block)
  "Put block in cache"
  (hash-put! (block-manager-cache manager) label block))

(def (block-cache-invalidate! manager label)
  "Invalidate cached block"
  (hash-remove! (block-manager-cache manager) label))

(def (block-cache-clear! manager)
  "Clear block cache"
  (hash-clear! (block-manager-cache manager)))

;;; ============================================================================
;;; Core Memory Operations
;;; ============================================================================

(def (core-memory-get manager)
  "Get core memory structure"
  (let ((blocks (block-get-all manager)))
    (let ((persona-block (find (lambda (b) (equal? (hash-ref b 'label) "persona"))
                               blocks))
          (human-block (find (lambda (b) (equal? (hash-ref b 'label) "human"))
                            blocks))
          (custom-blocks (filter (lambda (b)
                                   (not (standard-block? (hash-ref b 'label))))
                                blocks)))

      (make-core-memory
       persona: (if persona-block (hash-ref persona-block 'value) "")
       human: (if human-block (hash-ref human-block 'value) "")
       custom: (list->hash
                (map (lambda (b)
                       (cons (hash-ref b 'label) (hash-ref b 'value)))
                     custom-blocks))))))

(def (core-memory-set! manager memory)
  "Set core memory structure"
  ;; Update persona
  (block-update! manager "persona" (core-memory-persona memory))

  ;; Update human
  (block-update! manager "human" (core-memory-human memory))

  ;; Update custom blocks
  (hash-for-each
   (lambda (label value)
     (if (block-exists? manager label)
         (block-update! manager label value)
         (block-create! manager label value)))
   (core-memory-custom memory)))

(def (core-memory-initialize! manager
                              #!key
                              (persona #f)
                              (human #f))
  "Initialize core memory with default or provided values"
  ;; Create persona block
  (block-create! manager "persona"
                (or persona (get-block-template "persona"))
                is-template: #t)

  ;; Create human block
  (block-create! manager "human"
                (or human (get-block-template "human"))
                is-template: #t))

;;; ============================================================================
;;; Block Search
;;; ============================================================================

(def (block-search manager query)
  "Search blocks by content"
  (let ((blocks (block-get-all manager)))
    (filter (lambda (b)
              (string-contains (hash-ref b 'value) query))
            blocks)))

(def (block-search-by-label manager pattern)
  "Search blocks by label pattern"
  (let ((blocks (block-get-all manager)))
    (filter (lambda (b)
              (string-contains (hash-ref b 'label) pattern))
            blocks)))

;;; ============================================================================
;;; Block Statistics
;;; ============================================================================

(def (block-count manager)
  "Count total blocks"
  (length (block-get-all manager)))

(def (block-count-standard manager)
  "Count standard blocks"
  (length (block-get-standard manager)))

(def (block-count-custom manager)
  "Count custom blocks"
  (length (block-get-custom manager)))

(def (block-total-size manager)
  "Calculate total size of all blocks"
  (let ((blocks (block-get-all manager)))
    (apply + (map memory-block-size blocks))))

(def (block-get-stats manager)
  "Get block statistics"
  (hash
   'total_blocks (block-count manager)
   'standard_blocks (block-count-standard manager)
   'custom_blocks (block-count-custom manager)
   'total_size (block-total-size manager)))

;;; ============================================================================
;;; Block Export/Import
;;; ============================================================================

(def (block-export manager #!key (format 'json))
  "Export all blocks"
  (let ((blocks (block-get-all manager)))
    (case format
      ((json)
       (json-object->string
        (hash 'agent_id (block-manager-agent-id manager)
              'blocks blocks)))

      ((text)
       (string-join
        (map (lambda (b)
               (format "[~a]\n~a\n"
                      (hash-ref b 'label)
                      (hash-ref b 'value)))
             blocks)
        "\n"))

      (else
       (error "Unsupported export format" format: format)))))

(def (block-import! manager blocks-data)
  "Import blocks from exported data"
  (let ((blocks (hash-ref blocks-data 'blocks)))
    (for-each
     (lambda (block)
       (let ((label (hash-ref block 'label))
             (value (hash-ref block 'value)))
         (if (block-exists? manager label)
             (block-update! manager label value)
             (block-create! manager label value))))
     blocks)))

;;; ============================================================================
;;; Block Utilities
;;; ============================================================================

(def (block-list-labels manager)
  "List all block labels"
  (map (lambda (b) (hash-ref b 'label))
       (block-get-all manager)))

(def (block-get-by-labels manager labels)
  "Get blocks by list of labels"
  (filter-map (lambda (label)
                (block-get manager label))
              labels))

(def (block-copy! manager from-label to-label)
  "Copy block to new label"
  (let ((value (block-get-value manager from-label)))
    (if value
        (block-create! manager to-label value)
        (error "Source block not found" label: from-label))))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Connect to database
(db-connect!)

;; Create block manager
(def manager (make-block-manager agent-id))

;; Initialize core memory
(core-memory-initialize! manager
                        persona: "You are a helpful AI assistant."
                        human: "User prefers concise responses.")

;; Create custom block
(block-create! manager "preferences"
              "User likes technical details."
              is-readonly: #f)

;; Get block
(def block (block-get manager "persona"))
(displayln (format "Persona: ~a" (hash-ref block 'value)))

;; Update block
(block-update! manager "persona" "You are a very helpful AI assistant.")

;; Append to block
(block-append! manager "human" "User is a software engineer.")

;; Get core memory
(def memory (core-memory-get manager))
(displayln (format "Persona: ~a" (core-memory-persona memory)))

;; Search blocks
(def results (block-search manager "helpful"))
(displayln (format "Found ~a blocks" (length results)))

;; Get statistics
(def stats (block-get-stats manager))
(displayln (format "Total blocks: ~a" (hash-ref stats 'total_blocks)))

;; Export blocks
(def export (block-export manager format: 'json))
(displayln export)
|#
