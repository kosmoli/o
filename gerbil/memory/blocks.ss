;;; memory/blocks.ss - Memory Block Operations
;;;
;;; High-level memory block operations for core memory management.

(package: o)

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/misc/uuid
  :std/srfi/19
  :o/memory/types)

;;; ============================================================================
;;; Database Stub Functions (placeholder until socket layer is implemented)
;;; ============================================================================

(def (db-create-memory-block agent-id params)
  "Create memory block (stub - returns mock data)"
  (let ((ht (make-hash-table)))
    (hash-put! ht 'id (uuid-generate))
    (hash-put! ht 'agent_id agent-id)
    (hash-put! ht 'label (hash-ref params 'label))
    (hash-put! ht 'value (hash-ref params 'value))
    (hash-put! ht 'is_template (hash-ref params 'is_template #f))
    (hash-put! ht 'is_readonly (hash-ref params 'is_readonly #f))
    (hash-put! ht 'created_at (time->seconds (current-time)))
    (hash-put! ht 'updated_at (time->seconds (current-time)))
    ht))

(def (db-get-memory-blocks agent-id)
  "Get all memory blocks for agent (stub - returns empty list)"
  '())

(def (db-update-memory-block agent-id label value)
  "Update memory block (stub - returns success)"
  (let ((ht (make-hash-table)))
    (hash-put! ht 'success #t)
    ht))

(def (db-delete-memory-block agent-id label)
  "Delete memory block (stub - returns success)"
  (let ((ht (make-hash-table)))
    (hash-put! ht 'success #t)
    ht))

(def (db-connect!)
  "Connect to database (stub - no-op)"
  (displayln "Database connect (stub)"))

(def (block-template-labels)
  "Get standard block template labels"
  '("persona" "human"))

(def (get-block-template label)
  "Get block template content"
  (case label
    ((persona) "You are a helpful AI assistant.")
    ((human) "The user I am assisting.")
    (else "")))

;;; ============================================================================
;;; Memory Block Manager
;;; ============================================================================

(defstruct block-manager
  (agent-id      ; Agent ID
   cache         ; Block cache
   cache-enabled) ; Is caching enabled?
  transparent: #t)

(def (create-block-manager agent-id . rest)
  "Create a new block manager for an agent"
  (let ((cache-enabled (if (null? rest) #t (car rest))))
    (make-block-manager
     agent-id
     (make-hash-table)
     cache-enabled)))

;;; ============================================================================
;;; Block Creation
;;; ============================================================================

(def (block-create! manager label value . rest)
  "Create a new memory block"
  (let ((is-template (if (null? rest) #f (car rest)))
        (is-readonly (if (or (null? rest) (null? (cdr rest))) #f (cadr rest)))
        (validate? (if (or (null? rest) (null? rest) (null? (caddr rest))) #t (car (caddr rest))))
    ;; Validate block parameters if requested
    (when validate?
      (let ((params (let ((ht (make-hash-table)))
        (hash-put! ht 'label label)
        (hash-put! ht 'value value)
        ht)))
        (let ((result (validate-block-params params)))
          (unless (car result)
            (error "Invalid block parameters" (cdr result))))))

    ;; Create block via database
    (let ((block (db-create-memory-block (block-manager-agent-id manager)
                                           (let ((ht (make-hash-table)))
                                             (hash-put! ht 'label label)
                                             (hash-put! ht 'value value)
                                             (hash-put! ht 'is_template is-template)
                                             (hash-put! ht 'is_readonly is-readonly)
                                             ht))))

      ;; Add to cache
      (when (block-manager-cache-enabled manager)
        (block-cache-put! manager label block))

      (displayln (format "Block created: ~a" label))
      block)))

(def (block-create-from-template! manager label)
  "Create block from template"
  (let ((template (get-block-template label)))
    (block-create! manager label template #t #f)))

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
              blocks)))))

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

(def (block-update! manager label value . rest)
  "Update memory block value"
  (let ((validate? (if (null? rest) #t (car rest))))
    ;; Check if block is read-only
    (let ((block (block-get manager label)))
      (when (and block (is-readonly-block? block))
        (error "Cannot update read-only block")))

    ;; Validate value if requested
    (when validate?
      (unless (string? value)
        (error "Value must be a string")))

    ;; Update block via database
    (db-update-memory-block (block-manager-agent-id manager) label value)

    ;; Update cache
    (when (block-manager-cache-enabled manager)
      (block-cache-invalidate! manager label))

    (displayln (format "Block updated: ~a" label))))

(def (block-append! manager label text . rest)
  "Append text to block value"
  (let ((separator (if (null? rest) "\n" (car rest))))
    (let ((current (block-get-value manager label)))
      (if current
          (let ((new-value (string-append current separator text)))
            (block-update! manager label new-value))
          (error "Block not found" label)))))

(def (block-replace! manager label old-text new-text)
  "Replace text in block value"
  (let ((current (block-get-value manager label)))
    (if current
        (let ((new-value (string-replace current old-text new-text)))
          (block-update! manager label new-value))
        (error "Block not found" label))))

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
    (error "Cannot delete standard block" label))

  ;; Check if block is read-only
  (let ((block (block-get manager label)))
    (when (and block (is-readonly-block? block))
      (error "Cannot delete read-only block" label)))

  ;; Delete block via database
  (db-delete-memory-block (block-manager-agent-id manager) label)

  ;; Remove from cache
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
        (validate-block-params (let ((ht (make-hash-table)))
          (hash-put! ht 'label label)
          (hash-put! ht 'value (hash-ref block 'value))
          ht))
        (cons #f (list "Block not found")))))

(def (block-validate-all manager)
  "Validate all blocks"
  (let ((blocks (block-get-all manager))
        (errors '()))

    (for-each
     (lambda (block)
       (let ((result (validate-block-params
                      (let ((ht (make-hash-table)))
                        (hash-put! ht 'label (hash-ref block 'label))
                        (hash-put! ht 'value (hash-ref block 'value))
                        ht))))
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
       (if persona-block (hash-ref persona-block 'value) "")
       (if human-block (hash-ref human-block 'value) "")
       (list->hash
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

(def (core-memory-initialize! manager . rest)
  "Initialize core memory with default or provided values"
  (let ((persona (if (null? rest) #f (car rest)))
        (human (if (or (null? rest) (null? (cdr rest))) #f (cadr rest))))
    ;; Create persona block
    (block-create! manager "persona"
                  (or persona (get-block-template "persona"))
                  #t #f)

    ;; Create human block
    (block-create! manager "human"
                  (or human (get-block-template "human"))
                  #t #f)))

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
  (let ((ht (make-hash-table)))
    (hash-put! ht 'total_blocks (block-count manager))
    (hash-put! ht 'standard_blocks (block-count-standard manager))
    (hash-put! ht 'custom_blocks (block-count-custom manager))
    (hash-put! ht 'total_size (block-total-size manager))
    ht))

;;; ============================================================================
;;; Block Export/Import
;;; ============================================================================

(def (block-export manager . rest)
  "Export all blocks"
  (let ((format (if (null? rest) 'json (car rest)))
        (blocks (block-get-all manager)))
    (case format
      ((json)
       (json-object->string
        (let ((ht (make-hash-table)))
          (hash-put! ht 'agent_id (block-manager-agent-id manager))
          (hash-put! ht 'blocks blocks)
          ht)))

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
        (error "Source block not found" from-label))))

#|
;;; ============================================================================
;;; Example Usage
;;; ============================================================================

;; Connect to database
(db-connect!)

;; Create block manager
(def manager (create-block-manager agent-id))

;; Initialize core memory
(core-memory-initialize! manager
                        persona: "You are a helpful AI assistant."
                        human: "User prefers concise responses.")

;; Create custom block
(block-create! manager "preferences"
              "User likes technical details."
              is-readonly: #f)

;; Get block
(def block (block-get manager "persona")
(displayln (format "Persona: ~a" (hash-ref block 'value)))

;; Update block
(block-update! manager "persona" "You are a very helpful AI assistant.")

;; Append to block
(block-append! manager "human" "User is a software engineer.")

;; Get core memory
(def memory (core-memory-get manager))
(displayln (format "Persona: ~a" (core-memory-persona memory)))

;; Search blocks
(def results (block-search manager "helpful")
(displayln (format "Found ~a blocks" (length results)))

;; Get statistics
(def stats (block-get-stats manager)
(displayln (format "Total blocks: ~a" (hash-ref stats 'total_blocks)))

;; Export blocks
(def export (block-export manager format: 'json)
(displayln export)
|#
