#lang racket

;;; memory/types.rkt - Memory Type Definitions
;;;
;;; Type definitions for memory system (memos-compatible).

(provide (struct-out memory-block)
         (struct-out core-memory)
         (struct-out archival-entry)
         (struct-out recall-entry)
         (struct-out memory-stats)
         standard-block-labels
         valid-block-label?
         standard-block?
         validate-block-params
         default-persona-template
         default-human-template
         get-block-template
         is-template-block?
         is-readonly-block?
         make-default-core-memory
         core-memory-get-block
         core-memory-set-block!
         core-memory-has-block?
         core-memory-list-blocks
         validate-archival-params
         make-empty-memory-stats
         hash->memory-block
         memory-block->hash
         hash->archival-entry
         archival-entry->hash
         memory-block-size
         archival-entry-size
         calculate-total-memory-size)

(require racket/hash
         racket/match
         racket/format)

;;; ============================================================================
;;; Memory Block Types
;;; ============================================================================

(struct memory-block
  (id           ; UUID
   agent-id     ; UUID
   label        ; Block label (e.g., "persona", "human")
   value        ; Block content
   is-template  ; Is this a template block?
   is-readonly  ; Is this block read-only?
   created-at   ; Timestamp
   updated-at)  ; Timestamp
  #:transparent)

;;; Standard block labels
(define standard-block-labels '("persona" "human"))

;;; ============================================================================
;;; Memory Block Validation
;;; ============================================================================

(define (valid-block-label? label)
  "Check if block label is valid"
  (and (string? label)
       (> (string-length label) 0)
       (<= (string-length label) 100)))

(define (standard-block? label)
  "Check if block is a standard block"
  (if (member label standard-block-labels) #t #f))

(define (validate-block-params params)
  "Validate memory block parameters
   Returns: (cons #t #f) if valid, (cons #f errors) if invalid"
  (define errors '())

  ;; Validate label
  (unless (hash-has-key? params 'label)
    (set! errors (cons "Label is required" errors)))

  (when (hash-has-key? params 'label)
    (let ([label (hash-ref params 'label)])
      (unless (valid-block-label? label)
        (set! errors (cons "Invalid label" errors)))))

  ;; Validate value
  (unless (hash-has-key? params 'value)
    (set! errors (cons "Value is required" errors)))

  (when (hash-has-key? params 'value)
    (let ([value (hash-ref params 'value)])
      (unless (string? value)
        (set! errors (cons "Value must be a string" errors)))))

  ;; Return validation result
  (if (null? errors)
      (cons #t #f)
      (cons #f errors)))

;;; ============================================================================
;;; Memory Block Templates
;;; ============================================================================

(define default-persona-template
  "You are a helpful AI assistant.")

(define default-human-template
  "User information will be stored here.")

(define (get-block-template label)
  "Get default template for block label"
  (match label
    ["persona" default-persona-template]
    ["human" default-human-template]
    [_ ""]))

(define (is-template-block? block)
  "Check if block is a template"
  (if (memory-block? block)
      (memory-block-is-template block)
      (hash-ref block 'is_template #f)))

(define (is-readonly-block? block)
  "Check if block is read-only"
  (if (memory-block? block)
      (memory-block-is-readonly block)
      (hash-ref block 'is_readonly #f)))

;;; ============================================================================
;;; Core Memory Structure
;;; ============================================================================

(struct core-memory
  (persona      ; Persona block
   human        ; Human block
   custom)      ; Custom blocks (hash)
  #:transparent
  #:mutable)

(define (make-default-core-memory)
  "Create default core memory"
  (core-memory
   default-persona-template
   default-human-template
   (hash)))

(define (core-memory-get-block memory label)
  "Get block from core memory by label"
  (match label
    ["persona" (core-memory-persona memory)]
    ["human" (core-memory-human memory)]
    [_ (hash-ref (core-memory-custom memory) label #f)]))

(define (core-memory-set-block! memory label value)
  "Set block in core memory"
  (match label
    ["persona" (set-core-memory-persona! memory value)]
    ["human" (set-core-memory-human! memory value)]
    [_ (hash-set! (core-memory-custom memory) label value)]))

(define (core-memory-has-block? memory label)
  "Check if core memory has block"
  (match label
    [(or "persona" "human") #t]
    [_ (hash-has-key? (core-memory-custom memory) label)]))

(define (core-memory-list-blocks memory)
  "List all block labels in core memory"
  (append '("persona" "human")
          (hash-keys (core-memory-custom memory))))

;;; ============================================================================
;;; Archival Memory Entry
;;; ============================================================================

(struct archival-entry
  (id           ; UUID
   agent-id     ; UUID
   content      ; Entry content
   embedding    ; Vector embedding (1536 dimensions)
   importance   ; Importance score (0.0-1.0)
   tags         ; Tags (list of strings)
   created-at)  ; Timestamp
  #:transparent)

(define (validate-archival-params params)
  "Validate archival memory parameters"
  (define errors '())

  ;; Validate content
  (unless (hash-has-key? params 'content)
    (set! errors (cons "Content is required" errors)))

  ;; Validate importance (if provided)
  (when (hash-has-key? params 'importance)
    (let ([importance (hash-ref params 'importance)])
      (unless (and (number? importance)
                   (>= importance 0.0)
                   (<= importance 1.0))
        (set! errors (cons "Importance must be between 0.0 and 1.0" errors)))))

  ;; Validate tags (if provided)
  (when (hash-has-key? params 'tags)
    (let ([tags (hash-ref params 'tags)])
      (unless (list? tags)
        (set! errors (cons "Tags must be a list" errors)))))

  ;; Return validation result
  (if (null? errors)
      (cons #t #f)
      (cons #f errors)))

;;; ============================================================================
;;; Recall Memory Entry
;;; ============================================================================

(struct recall-entry
  (id           ; UUID
   agent-id     ; UUID
   content      ; Entry content
   context      ; Context information
   created-at)  ; Timestamp
  #:transparent)

;;; ============================================================================
;;; Memory Statistics
;;; ============================================================================

(struct memory-stats
  (core-blocks-count      ; Number of core memory blocks
   archival-entries-count ; Number of archival entries
   recall-entries-count   ; Number of recall entries
   total-memory-size      ; Total memory size (bytes)
   last-updated)          ; Last update timestamp
  #:transparent)

(define (make-empty-memory-stats)
  "Create empty memory statistics"
  (memory-stats
   0
   0
   0
   0
   #f))

;;; ============================================================================
;;; Memory Conversion
;;; ============================================================================

(define (hash->memory-block h)
  "Convert hash to memory block struct"
  (memory-block
   (hash-ref h 'id)
   (hash-ref h 'agent_id)
   (hash-ref h 'label)
   (hash-ref h 'value)
   (hash-ref h 'is_template #f)
   (hash-ref h 'is_readonly #f)
   (hash-ref h 'created_at)
   (hash-ref h 'updated_at)))

(define (memory-block->hash block)
  "Convert memory block struct to hash"
  (hash 'id (memory-block-id block)
        'agent_id (memory-block-agent-id block)
        'label (memory-block-label block)
        'value (memory-block-value block)
        'is_template (memory-block-is-template block)
        'is_readonly (memory-block-is-readonly block)
        'created_at (memory-block-created-at block)
        'updated_at (memory-block-updated-at block)))

(define (hash->archival-entry h)
  "Convert hash to archival entry struct"
  (archival-entry
   (hash-ref h 'id)
   (hash-ref h 'agent_id)
   (hash-ref h 'content)
   (hash-ref h 'embedding #f)
   (hash-ref h 'importance 0.5)
   (hash-ref h 'tags '())
   (hash-ref h 'created_at)))

(define (archival-entry->hash entry)
  "Convert archival entry struct to hash"
  (hash 'id (archival-entry-id entry)
        'agent_id (archival-entry-agent-id entry)
        'content (archival-entry-content entry)
        'embedding (archival-entry-embedding entry)
        'importance (archival-entry-importance entry)
        'tags (archival-entry-tags entry)
        'created_at (archival-entry-created-at entry)))

;;; ============================================================================
;;; Memory Utilities
;;; ============================================================================

(define (memory-block-size block)
  "Calculate memory block size in bytes"
  (define value (if (memory-block? block)
                    (memory-block-value block)
                    (hash-ref block 'value "")))
  (string-length value))

(define (archival-entry-size entry)
  "Calculate archival entry size in bytes"
  (define content (if (archival-entry? entry)
                      (archival-entry-content entry)
                      (hash-ref entry 'content "")))
  (string-length content))

(define (calculate-total-memory-size blocks archival-entries)
  "Calculate total memory size"
  (define block-size (apply + (map memory-block-size blocks)))
  (define archival-size (apply + (map archival-entry-size archival-entries)))
  (+ block-size archival-size))
