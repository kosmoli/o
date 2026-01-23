#lang racket

;;; memory/core.rkt - Core Memory Operations
;;;
;;; High-level core memory manipulation tools for agent memory management.

(provide (struct-out memory-change)
         (struct-out memory-history)
         (struct-out core-memory-manager)
         (struct-out memory-constraints)
         make-memory-history
         history-add-change!
         history-get-changes
         history-get-changes-for-block
         make-core-memory-manager
         make-default-constraints
         validate-constraints
         core-memory-append!
         core-memory-replace!
         apply-patch-operation
         memory-apply-patch!
         memory-rollback!
         memory-rollback-to-timestamp!
         validate-core-memory
         get-memory-stats
         core-memory-set-block!
         core-memory-clear-block!
         core-memory-prepend!)

(require racket/hash
        racket/format
        racket/list
        racket/string
        json
        "./types.rkt"
        "./blocks.rkt")

;;; ============================================================================
;;; Memory Change Tracking
;;; ============================================================================

(struct memory-change
  (timestamp        ; When the change occurred
   operation        ; Operation type (append, replace, patch)
   block-label      ; Which block was changed
   old-value        ; Previous value
   new-value        ; New value
   metadata)        ; Additional metadata
  #:transparent)

(struct memory-history
  (agent-id         ; Agent ID
   changes          ; List of memory-change records
   max-history)     ; Maximum history size
  #:transparent
  #:mutable)

(define (make-memory-history agent-id #:max-history [max-history 100])
  "Create memory history tracker"
  (memory-history agent-id '() max-history))

(define (history-add-change! history change)
  "Add change to history"
  (define changes (memory-history-changes history))
  (define new-changes (cons change changes))
  ;; Trim history if too long
  (define trimmed
    (if (> (length new-changes) (memory-history-max-history history))
        (take new-changes (memory-history-max-history history))
        new-changes))
  (set-memory-history-changes! history trimmed))

(define (history-get-changes history #:limit [limit 10])
  "Get recent changes"
  (take (memory-history-changes history)
        (min limit (length (memory-history-changes history)))))

(define (history-get-changes-for-block history block-label)
  "Get changes for specific block"
  (filter (lambda (change)
            (equal? (memory-change-block-label change) block-label))
          (memory-history-changes history)))

;;; ============================================================================
;;; Core Memory Manager with History
;;; ============================================================================

(struct core-memory-manager
  (block-manager    ; Block manager
   history          ; Memory history
   constraints)     ; Memory constraints
  #:transparent)

(define (make-core-memory-manager agent-id
                                   #:cache-enabled [cache-enabled #t]
                                   #:max-history [max-history 100]
                                   #:constraints [constraints #f])
  "Create core memory manager with history tracking"
  (core-memory-manager
   (create-block-manager agent-id #:cache-enabled cache-enabled)
   (make-memory-history agent-id #:max-history max-history)
   (or constraints (make-default-constraints))))

;;; ============================================================================
;;; Memory Constraints
;;; ============================================================================

(struct memory-constraints
  (max-block-size       ; Maximum block size in characters
   min-block-size       ; Minimum block size
   readonly-blocks      ; List of read-only block labels
   required-blocks      ; List of required block labels
   custom-validators)   ; Custom validation functions
  #:transparent)

(define (make-default-constraints)
  "Create default memory constraints"
  (memory-constraints
   10000
   0
   '()
   '("persona" "human")
   '()))

(define (validate-constraints manager block-label new-value)
  "Validate memory constraints"
  (define constraints (core-memory-manager-constraints manager))
  (define errors '())

  ;; Check block size
  (define size (string-length new-value))
  (when (> size (memory-constraints-max-block-size constraints))
    (set! errors (cons (format "Block size ~a exceeds maximum ~a"
                               size
                               (memory-constraints-max-block-size constraints))
                       errors)))
  (when (< size (memory-constraints-min-block-size constraints))
    (set! errors (cons (format "Block size ~a below minimum ~a"
                               size
                               (memory-constraints-min-block-size constraints))
                       errors)))

  ;; Check read-only
  (when (member block-label (memory-constraints-readonly-blocks constraints))
    (set! errors (cons (format "Block ~a is read-only" block-label) errors)))

  ;; Return validation result
  (if (null? errors)
      (cons #t #f)
      (cons #f errors)))

;;; ============================================================================
;;; Core Memory Append
;;; ============================================================================

(define (core-memory-append! manager block-label text
                               #:separator [separator "\n"]
                               #:validate? [validate? #t])
  "Append text to memory block"
  (define block-mgr (core-memory-manager-block-manager manager))
  (define old-value (block-get-value block-mgr block-label))

  (unless old-value
    (error 'core-memory-append! "Block not found" block-label))

  ;; Create new value
  (define new-value
    (if (string=? old-value "")
        text
        (string-append old-value separator text)))

  ;; Validate constraints
  (when validate?
    (define result (validate-constraints manager block-label new-value))
    (unless (car result)
      (error 'core-memory-append! "Constraint validation failed" (cdr result))))

  ;; Record change
  (define change
    (memory-change
     (current-seconds)
     'append
     block-label
     old-value
     new-value
     (hash 'text text 'separator separator)))
  (history-add-change! (core-memory-manager-history manager) change)

  ;; Update block
  (block-update! block-mgr block-label new-value #:validate? #f)

  (displayln (format "Appended to block ~a: ~a" block-label text))
  new-value)

;;; ============================================================================
;;; Core Memory Replace
;;; ============================================================================

(define (core-memory-replace! manager block-label old-text new-text
                               #:validate? [validate? #t]
                               #:case-sensitive? [case-sensitive? #t])
  "Replace text in memory block"
  (define block-mgr (core-memory-manager-block-manager manager))
  (define old-value (block-get-value block-mgr block-label))

  (unless old-value
    (error 'core-memory-replace! "Block not found" block-label))

  ;; Check if old-text exists
  (define search-value (if case-sensitive? old-value (string-downcase old-value)))
  (define search-text (if case-sensitive? old-text (string-downcase old-text)))
  (unless (string-contains? search-value search-text)
    (error 'core-memory-replace! "Text not found in block" old-text block-label))

  ;; Create new value
  (define new-value (string-replace old-value old-text new-text))

  ;; Validate constraints
  (when validate?
    (define result (validate-constraints manager block-label new-value))
    (unless (car result)
      (error 'core-memory-replace! "Constraint validation failed" (cdr result))))

  ;; Record change
  (define change
    (memory-change
     (current-seconds)
     'replace
     block-label
     old-value
     new-value
     (hash 'old_text old-text
           'new_text new-text
           'case_sensitive case-sensitive?)))
  (history-add-change! (core-memory-manager-history manager) change)

  ;; Update block
  (block-update! block-mgr block-label new-value #:validate? #f)

  (displayln (format "Replaced in block ~a: '~a' -> '~a'"
                     block-label old-text new-text))
  new-value)

;;; ============================================================================
;;; Memory Patch Operations
;;; ============================================================================

(define (apply-patch-operation block-value operation)
  "Apply single JSON patch operation"
  (define op (hash-ref operation 'op))
  (case op
    [("append")
     (define value (hash-ref operation 'value))
     (define separator (hash-ref operation 'separator "\n"))
     (if (string=? block-value "")
         value
         (string-append block-value separator value))]
    [("prepend")
     (define value (hash-ref operation 'value))
     (define separator (hash-ref operation 'separator "\n"))
     (if (string=? block-value "")
         value
         (string-append value separator block-value))]
    [("replace")
     (define old-text (hash-ref operation 'old))
     (define new-text (hash-ref operation 'new))
     (string-replace block-value old-text new-text)]
    [("set")
     (hash-ref operation 'value)]
    [("clear")
     ""]
    [else
     (error 'apply-patch-operation "Unknown patch operation" op)]))

(define (memory-apply-patch! manager block-label patch
                             #:validate? [validate? #t])
  "Apply JSON patch to memory block"
  (define block-mgr (core-memory-manager-block-manager manager))
  (define old-value (block-get-value block-mgr block-label))

  (unless old-value
    (error 'memory-apply-patch! "Block not found" block-label))

  ;; Normalize patch to list of operations
  (define operations (if (list? patch) patch (list patch)))

  ;; Apply operations sequentially
  (define new-value
    (for/fold ([value old-value])
              ([operation (in-list operations)])
      (apply-patch-operation value operation)))

  ;; Validate constraints
  (when validate?
    (define result (validate-constraints manager block-label new-value))
    (unless (car result)
      (error 'memory-apply-patch! "Constraint validation failed" (cdr result))))

  ;; Record change
  (define change
    (memory-change
     (current-seconds)
     'patch
     block-label
     old-value
     new-value
     (hash 'patch patch)))
  (history-add-change! (core-memory-manager-history manager) change)

  ;; Update block
  (block-update! block-mgr block-label new-value #:validate? #f)

  (displayln (format "Applied patch to block ~a" block-label))
  new-value)

;;; ============================================================================
;;; Memory Rollback
;;; ============================================================================

(define (memory-rollback! manager block-label #:steps [steps 1])
  "Rollback memory block to previous state"
  (define history (core-memory-manager-history manager))
  (define changes (history-get-changes-for-block history block-label))

  (when (null? changes)
    (error 'memory-rollback! "No history available for block" block-label))

  (when (< (length changes) steps)
    (error 'memory-rollback! "Not enough history" (length changes) steps))

  ;; Get the change to rollback to
  (define target-change (list-ref changes (- steps 1)))
  (define old-value (memory-change-old-value target-change))
  (define block-mgr (core-memory-manager-block-manager manager))

  ;; Update block
  (block-update! block-mgr block-label old-value #:validate? #f)

  ;; Record rollback
  (define change
    (memory-change
     (current-seconds)
     'rollback
     block-label
     (block-get-value block-mgr block-label)
     old-value
     (hash 'steps steps)))
  (history-add-change! history change)

  (displayln (format "Rolled back block ~a by ~a steps" block-label steps))
  old-value)

(define (memory-rollback-to-timestamp! manager block-label timestamp)
  "Rollback memory block to specific timestamp"
  (define history (core-memory-manager-history manager))
  (define changes (history-get-changes-for-block history block-label))

  (when (null? changes)
    (error 'memory-rollback-to-timestamp! "No history available for block" block-label))

  ;; Find change at or before timestamp
  (define target-change
    (for/first ([change (in-list changes)]
                #:when (<= (memory-change-timestamp change) timestamp))
      change))

  (unless target-change
    (error 'memory-rollback-to-timestamp! "No change found at or before timestamp" timestamp))

  (define old-value (memory-change-old-value target-change))
  (define block-mgr (core-memory-manager-block-manager manager))

  ;; Update block
  (block-update! block-mgr block-label old-value #:validate? #f)

  ;; Record rollback
  (define change
    (memory-change
     (current-seconds)
     'rollback
     block-label
     (block-get-value block-mgr block-label)
     old-value
     (hash 'target_timestamp timestamp)))
  (history-add-change! history change)

  (displayln (format "Rolled back block ~a to timestamp ~a" block-label timestamp))
  old-value)

;;; ============================================================================
;;; Memory Validation
;;; ============================================================================

(define (validate-core-memory manager)
  "Validate entire core memory"
  (define block-mgr (core-memory-manager-block-manager manager))
  (define constraints (core-memory-manager-constraints manager))
  (define errors '())

  ;; Check required blocks exist
  (for-each
   (lambda (label)
     (unless (block-exists? block-mgr label)
       (set! errors (cons (format "Required block missing: ~a" label) errors))))
   (memory-constraints-required-blocks constraints))

  ;; Validate all blocks
  (define blocks (block-get-all block-mgr))
  (for-each
   (lambda (block)
     (define label (hash-ref block 'label))
     (define value (hash-ref block 'value))
     (define result (validate-constraints manager label value))
     (unless (car result)
       (set! errors (append errors (cdr result)))))
   blocks)

  ;; Return validation result
  (if (null? errors)
      (cons #t #f)
      (cons #f errors)))

;;; ============================================================================
;;; Memory Statistics
;;; ============================================================================

(define (get-memory-stats manager)
  "Get memory statistics including history"
  (define block-mgr (core-memory-manager-block-manager manager))
  (define history (core-memory-manager-history manager))
  (define block-stats (block-get-stats block-mgr))

  (hash-set block-stats 'total_changes (length (memory-history-changes history))))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(define (core-memory-set-block! manager block-label value #:validate? [validate? #t])
  "Set block value directly (replaces entire content)"
  (memory-apply-patch! manager block-label
                       (hash 'op "set" 'value value)
                       #:validate? validate?))

(define (core-memory-clear-block! manager block-label)
  "Clear block content"
  (memory-apply-patch! manager block-label
                       (hash 'op "clear")))

(define (core-memory-prepend! manager block-label text #:separator [separator "\n"])
  "Prepend text to memory block"
  (memory-apply-patch! manager block-label
                       (hash 'op "prepend" 'value text 'separator separator)))
