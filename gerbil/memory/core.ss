;;; memory/core.ss - Core Memory Operations
;;;
;;; High-level core memory manipulation tools for agent memory management.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/text/json
  :o/memory/types
  :o/memory/blocks)

;;; ============================================================================
;;; Memory Change Tracking
;;; ============================================================================

(defstruct memory-change
  (timestamp        ; When the change occurred
   operation        ; Operation type (append, replace, patch)
   block-label      ; Which block was changed
   old-value        ; Previous value
   new-value        ; New value
   metadata)        ; Additional metadata
  transparent: #t)

(defstruct memory-history
  (agent-id         ; Agent ID
   changes          ; List of memory-change records
   max-history)     ; Maximum history size
  transparent: #t)

(def (make-memory-history agent-id . rest (max-history 100))
  "Create memory history tracker"
  (make-memory-history
   agent-id: agent-id
   changes: '()
   max-history: max-history))

(def (history-add-change! history change)
  "Add change to history"
  (let ((changes (memory-history-changes history)))
    (set! (memory-history-changes history)
          (cons change changes))
    ;; Trim history if too long
    (when (> (length (memory-history-changes history))
             (memory-history-max-history history))
      (set! (memory-history-changes history)
            (take (memory-history-changes history)
                  (memory-history-max-history history))))))

(def (history-get-changes history . rest (limit 10))
  "Get recent changes"
  (take (memory-history-changes history)
        (min limit (length (memory-history-changes history)))))

(def (history-get-changes-for-block history block-label)
  "Get changes for specific block"
  (filter (lambda (change)
            (equal? (memory-change-block-label change) block-label))
          (memory-history-changes history)))

;;; ============================================================================
;;; Core Memory Manager with History
;;; ============================================================================

(defstruct core-memory-manager
  (block-manager    ; Block manager
   history          ; Memory history
   constraints)     ; Memory constraints
  transparent: #t)

(def (make-core-memory-manager agent-id . rest
                               (cache-enabled #t)
                               (max-history 100)
                               (constraints #f))
  "Create core memory manager with history tracking"
  (make-core-memory-manager
   block-manager: (make-block-manager agent-id cache-enabled: cache-enabled)
   history: (make-memory-history agent-id max-history: max-history)
   constraints: (or constraints (make-default-constraints))))

;;; ============================================================================
;;; Memory Constraints
;;; ============================================================================

(defstruct memory-constraints
  (max-block-size       ; Maximum block size in characters
   min-block-size       ; Minimum block size
   readonly-blocks      ; List of read-only block labels
   required-blocks      ; List of required block labels
   custom-validators)   ; Custom validation functions
  transparent: #t)

(def (make-default-constraints)
  "Create default memory constraints"
  (make-memory-constraints
   max-block-size: 10000
   min-block-size: 0
   readonly-blocks: '()
   required-blocks: '("persona" "human")
   custom-validators: '()))

(def (validate-constraints manager block-label new-value)
  "Validate memory constraints"
  (let ((constraints (core-memory-manager-constraints manager))
        (errors '()))

    ;; Check block size
    (let ((size (string-length new-value)))
      (when (> size (memory-constraints-max-block-size constraints))
        (set! errors (cons (format "Block size ~a exceeds maximum ~a"
                                  size
                                  (memory-constraints-max-block-size constraints))
                          errors)))
      (when (< size (memory-constraints-min-block-size constraints))
        (set! errors (cons (format "Block size ~a below minimum ~a"
                                  size
                                  (memory-constraints-min-block-size constraints))
                          errors))))

    ;; Check read-only
    (when (member block-label (memory-constraints-readonly-blocks constraints))
      (set! errors (cons (format "Block ~a is read-only" block-label) errors)))

    ;; Run custom validators
    (for-each
     (lambda (validator)
       (let ((result (validator block-label new-value)))
         (unless (car result)
           (set! errors (append errors (cdr result))))))
     (memory-constraints-custom-validators constraints))

    ;; Return validation result
    (if (null? errors)
        (cons #t #f)
        (cons #f errors))))

;;; ============================================================================
;;; Core Memory Append
;;; ============================================================================

(def (core-memory-append! manager block-label text . rest
                          (separator "\n")
                          (validate? #t))
  "Append text to memory block

   Args:
     manager: Core memory manager
     block-label: Block label (e.g., 'persona', 'human')
     text: Text to append
     separator: Separator between old and new text (default: newline)
     validate?: Validate constraints (default: #t)

   Returns:
     Updated block value

   Example:
     (core-memory-append! manager \"human\" \"User is a software engineer.\")"

  (let* ((block-mgr (core-memory-manager-block-manager manager))
         (old-value (block-get-value block-mgr block-label)))

    (unless old-value
      (error "Block not found" label: block-label))

    ;; Create new value
    (let ((new-value (if (string=? old-value "")
                         text
                         (string-append old-value separator text))))

      ;; Validate constraints
      (when validate?
        (let ((result (validate-constraints manager block-label new-value)))
          (unless (car result)
            (error "Constraint validation failed" errors: (cdr result)))))

      ;; Record change
      (let ((change (make-memory-change
                     timestamp: (current-seconds)
                     operation: 'append
                     block-label: block-label
                     old-value: old-value
                     new-value: new-value
                     metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'text text)
  (hash-put! ht 'separator separator)
  ht))))
        (history-add-change! (core-memory-manager-history manager) change))

      ;; Update block
      (block-update! block-mgr block-label new-value validate?: #f)

      (displayln (format "Appended to block ~a: ~a" block-label text))
      new-value)))

;;; ============================================================================
;;; Core Memory Replace
;;; ============================================================================

(def (core-memory-replace! manager block-label old-text new-text . rest
                           (validate? #t)
                           (case-sensitive? #t))
  "Replace text in memory block

   Args:
     manager: Core memory manager
     block-label: Block label
     old-text: Text to find and replace
     new-text: Replacement text
     validate?: Validate constraints (default: #t)
     case-sensitive?: Case-sensitive search (default: #t)

   Returns:
     Updated block value

   Example:
     (core-memory-replace! manager \"persona\" \"helpful\" \"very helpful\")"

  (let* ((block-mgr (core-memory-manager-block-manager manager))
         (old-value (block-get-value block-mgr block-label)))

    (unless old-value
      (error "Block not found" label: block-label))

    ;; Check if old-text exists
    (let ((search-value (if case-sensitive? old-value (string-downcase old-value)))
          (search-text (if case-sensitive? old-text (string-downcase old-text))))
      (unless (string-contains search-value search-text)
        (error "Text not found in block" text: old-text label: block-label)))

    ;; Create new value
    (let ((new-value (string-replace old-value old-text new-text)))

      ;; Validate constraints
      (when validate?
        (let ((result (validate-constraints manager block-label new-value)))
          (unless (car result)
            (error "Constraint validation failed" errors: (cdr result)))))

      ;; Record change
      (let ((change (make-memory-change
                     timestamp: (current-seconds)
                     operation: 'replace
                     block-label: block-label
                     old-value: old-value
                     new-value: new-value
                     metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'old_text old-text)
  (hash-put! ht 'new_text new-text)
  (hash-put! ht 'case_sensitive case-sensitive?)
  ht))))
        (history-add-change! (core-memory-manager-history manager) change))

      ;; Update block
      (block-update! block-mgr block-label new-value validate?: #f)

      (displayln (format "Replaced in block ~a: '~a' -> '~a'"
                        block-label old-text new-text))
      new-value)))

;;; ============================================================================
;;; Memory Patch Operations
;;; ============================================================================

(def (apply-patch-operation block-value operation)
  "Apply single JSON patch operation

   Supported operations:
   - {op: 'append', value: 'text', separator: '\\n'}
   - {op: 'replace', old: 'text', new: 'text'}
   - {op: 'set', value: 'text'}
   - {op: 'clear'}
   - {op: 'prepend', value: 'text', separator: '\\n'}"

  (let ((op (hash-ref operation 'op)))
    (case op
      (("append")
       (let ((value (hash-ref operation 'value))
             (separator (hash-ref operation 'separator "\n")))
         (if (string=? block-value "")
             value
             (string-append block-value separator value))))

      (("prepend")
       (let ((value (hash-ref operation 'value))
             (separator (hash-ref operation 'separator "\n")))
         (if (string=? block-value "")
             value
             (string-append value separator block-value))))

      (("replace")
       (let ((old-text (hash-ref operation 'old))
             (new-text (hash-ref operation 'new)))
         (string-replace block-value old-text new-text)))

      (("set")
       (hash-ref operation 'value))

      (("clear")
       "")

      (else
       (error "Unknown patch operation" op: op)))))

(def (memory-apply-patch! manager block-label patch . rest
                          (validate? #t))
  "Apply JSON patch to memory block

   Args:
     manager: Core memory manager
     block-label: Block label
     patch: Patch specification (make-hash-table)
     validate?: Validate constraints (default: #t)

   Patch format:
     Single operation: {op: 'append', value: 'text'}
     Multiple operations: [{op: 'append', ...}, {op: 'replace', ...}]

   Returns:
     Updated block value

   Example:
     (memory-apply-patch! manager \"persona\"
                         (let ((ht (make-hash-table)))
  (hash-put! ht 'op \"append\")
  (hash-put! ht 'value \"I)
  ht))"

  (let* ((block-mgr (core-memory-manager-block-manager manager))
         (old-value (block-get-value block-mgr block-label)))

    (unless old-value
      (error "Block not found" label: block-label))

    ;; Normalize patch to list of operations
    (let ((operations (if (list? patch) patch (list patch))))

      ;; Apply operations sequentially
      (let ((new-value old-value))
        (for-each
         (lambda (operation)
           (set! new-value (apply-patch-operation new-value operation)))
         operations)

        ;; Validate constraints
        (when validate?
          (let ((result (validate-constraints manager block-label new-value)))
            (unless (car result)
              (error "Constraint validation failed" errors: (cdr result)))))

        ;; Record change
        (let ((change (make-memory-change
                       timestamp: (current-seconds)
                       operation: 'patch
                       block-label: block-label
                       old-value: old-value
                       new-value: new-value
                       metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'patch patch)
  ht))))
          (history-add-change! (core-memory-manager-history manager) change))

        ;; Update block
        (block-update! block-mgr block-label new-value validate?: #f)

        (displayln (format "Applied patch to block ~a" block-label))
        new-value))))

;;; ============================================================================
;;; Memory Rollback
;;; ============================================================================

(def (memory-rollback! manager block-label . rest (steps 1))
  "Rollback memory block to previous state

   Args:
     manager: Core memory manager
     block-label: Block label
     steps: Number of changes to rollback (default: 1)

   Returns:
     Restored block value"

  (let* ((history (core-memory-manager-history manager))
         (changes (history-get-changes-for-block history block-label)))

    (when (null? changes)
      (error "No history available for block" label: block-label))

    (when (< (length changes) steps)
      (error "Not enough history" available: (length changes) requested: steps))

    ;; Get the change to rollback to
    (let* ((target-change (list-ref changes (- steps 1)))
           (old-value (memory-change-old-value target-change))
           (block-mgr (core-memory-manager-block-manager manager)))

      ;; Update block
      (block-update! block-mgr block-label old-value validate?: #f)

      ;; Record rollback
      (let ((change (make-memory-change
                     timestamp: (current-seconds)
                     operation: 'rollback
                     block-label: block-label
                     old-value: (block-get-value block-mgr block-label)
                     new-value: old-value
                     metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'steps steps)
  ht))))
        (history-add-change! history change))

      (displayln (format "Rolled back block ~a by ~a steps" block-label steps))
      old-value)))

(def (memory-rollback-to-timestamp! manager block-label timestamp)
  "Rollback memory block to specific timestamp

   Args:
     manager: Core memory manager
     block-label: Block label
     timestamp: Target timestamp

   Returns:
     Restored block value"

  (let* ((history (core-memory-manager-history manager))
         (changes (history-get-changes-for-block history block-label)))

    (when (null? changes)
      (error "No history available for block" label: block-label))

    ;; Find change at or before timestamp
    (let ((target-change
           (find (lambda (change)
                   (<= (memory-change-timestamp change) timestamp))
                 changes)))

      (unless target-change
        (error "No change found at or before timestamp" timestamp: timestamp))

      (let* ((old-value (memory-change-old-value target-change))
             (block-mgr (core-memory-manager-block-manager manager)))

        ;; Update block
        (block-update! block-mgr block-label old-value validate?: #f)

        ;; Record rollback
        (let ((change (make-memory-change
                       timestamp: (current-seconds)
                       operation: 'rollback
                       block-label: block-label
                       old-value: (block-get-value block-mgr block-label)
                       new-value: old-value
                       metadata: (let ((ht (make-hash-table)))
  (hash-put! ht 'target_timestamp timestamp)
  ht))))
          (history-add-change! history change))

        (displayln (format "Rolled back block ~a to timestamp ~a"
                          block-label timestamp))
        old-value))))

;;; ============================================================================
;;; Memory Validation
;;; ============================================================================

(def (validate-core-memory manager)
  "Validate entire core memory

   Returns:
     (cons #t #f) if valid, (cons #f errors) if invalid"

  (let ((block-mgr (core-memory-manager-block-manager manager))
        (constraints (core-memory-manager-constraints manager))
        (errors '()))

    ;; Check required blocks exist
    (for-each
     (lambda (label)
       (unless (block-exists? block-mgr label)
         (set! errors (cons (format "Required block missing: ~a" label) errors))))
     (memory-constraints-required-blocks constraints))

    ;; Validate all blocks
    (let ((blocks (block-get-all block-mgr)))
      (for-each
       (lambda (block)
         (let* ((label (hash-ref block 'label))
                (value (hash-ref block 'value))
                (result (validate-constraints manager label value)))
           (unless (car result)
             (set! errors (append errors (cdr result))))))
       blocks))

    ;; Return validation result
    (if (null? errors)
        (cons #t #f)
        (cons #f errors))))

;;; ============================================================================
;;; Memory Statistics
;;; ============================================================================

(def (get-memory-stats manager)
  "Get memory statistics including history

   Returns:
     Hash with statistics"

  (let* ((block-mgr (core-memory-manager-block-manager manager))
         (history (core-memory-manager-history manager))
         (block-stats (block-get-stats block-mgr)))

    (hash-put! block-stats 'total_changes (length (memory-history-changes history)))
    (hash-put! block-stats 'max_history (memory-history-max-history history))

    block-stats))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(def (core-memory-set-block! manager block-label value . rest (validate? #t))
  "Set block value directly (replaces entire content)"
  (memory-apply-patch! manager block-label
                      (let ((ht (make-hash-table)))
  (hash-put! ht 'op "set")
  (hash-put! ht 'value value)
  ht)
                      validate?: validate?))

(def (core-memory-clear-block! manager block-label)
  "Clear block content"
  (memory-apply-patch! manager block-label
                      (let ((ht (make-hash-table)))
  (hash-put! ht 'op "clear")
  ht)))

(def (core-memory-prepend! manager block-label text . rest (separator "\n"))
  "Prepend text to memory block"
  (memory-apply-patch! manager block-label
                      (let ((ht (make-hash-table)))
  (hash-put! ht 'op "prepend")
  (hash-put! ht 'value text)
  (hash-put! ht 'separator separator)
  ht)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create core memory manager
(def manager (make-core-memory-manager agent-id))

;; Initialize core memory
(core-memory-initialize! (core-memory-manager-block-manager manager))

;; Append to persona
(core-memory-append! manager "persona" "I am knowledgeable about AI.")

;; Replace text
(core-memory-replace! manager "persona" "helpful" "very helpful")

;; Apply patch
(memory-apply-patch! manager "human"
                    (list (let ((ht (make-hash-table)))
  (hash-put! ht 'op "append")
  (hash-put! ht 'value "User is a developer.")
  ht)
                          (hash ('op "append") ('value "User prefers concise answers."))))

;; Rollback
(memory-rollback! manager "persona" steps: 1)

;; Validate
(def result (validate-core-memory manager))
(if (car result)
    (displayln "Memory is valid")
    (displayln (format "Validation errors: ~a" (cdr result))))

;; Get statistics
(def stats (get-memory-stats manager))
(displayln (format "Total changes: ~a" (hash-ref stats 'total_changes)))
|#
