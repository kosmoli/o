;;; memory/types.ss - Memory Type Definitions
;;;
;;; Type definitions for memory system (memos-compatible).

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format)

;;; ============================================================================
;;; Memory Block Types
;;; ============================================================================

(defstruct memory-block
  (id           ; UUID
   agent-id     ; UUID
   label        ; Block label (e.g., "persona", "human")
   value        ; Block content
   is-template  ; Is this a template block?
   is-readonly  ; Is this block read-only?
   created-at   ; Timestamp
   updated-at)  ; Timestamp
  transparent: #t)

;;; Standard block labels
(def standard-block-labels '("persona" "human"))

;;; ============================================================================
;;; Memory Block Validation
;;; ============================================================================

(def (valid-block-label? label)
  "Check if block label is valid"
  (and (string? label)
       (> (string-length label) 0)
       (<= (string-length label) 100)))

(def (standard-block? label)
  "Check if block is a standard block"
  (member label standard-block-labels))

(def (validate-block-params params)
  "Validate memory block parameters
   Returns: (cons #t #f) if valid, (cons #f errors) if invalid"
  (let ((errors '()))

    ;; Validate label
    (unless (hash-key? params 'label)
      (set! errors (cons "Label is required" errors)))

    (when (hash-key? params 'label)
      (let ((label (hash-ref params 'label)))
        (unless (valid-block-label? label)
          (set! errors (cons "Invalid label" errors)))))

    ;; Validate value
    (unless (hash-key? params 'value)
      (set! errors (cons "Value is required" errors)))

    (when (hash-key? params 'value)
      (let ((value (hash-ref params 'value)))
        (unless (string? value)
          (set! errors (cons "Value must be a string" errors)))))

    ;; Return validation result
    (if (null? errors)
        (cons #t #f)
        (cons #f errors))))

;;; ============================================================================
;;; Memory Block Templates
;;; ============================================================================

(def default-persona-template
  "You are a helpful AI assistant.")

(def default-human-template
  "User information will be stored here.")

(def (get-block-template label)
  "Get default template for block label"
  (case label
    (("persona") default-persona-template)
    (("human") default-human-template)
    (else "")))

(def (is-template-block? block)
  "Check if block is a template"
  (if (memory-block? block)
      (memory-block-is-template block)
      (hash-ref block 'is_template #f)))

(def (is-readonly-block? block)
  "Check if block is read-only"
  (if (memory-block? block)
      (memory-block-is-readonly block)
      (hash-ref block 'is_readonly #f)))

;;; ============================================================================
;;; Core Memory Structure
;;; ============================================================================

(defstruct core-memory
  (persona      ; Persona block
   human        ; Human block
   custom)      ; Custom blocks (hash)
  transparent: #t)

(def (make-default-core-memory)
  "Create default core memory"
  (make-core-memory
   persona: default-persona-template
   human: default-human-template
   custom: (hash)))

(def (core-memory-get-block memory label)
  "Get block from core memory by label"
  (case label
    (("persona") (core-memory-persona memory))
    (("human") (core-memory-human memory))
    (else (hash-ref (core-memory-custom memory) label #f))))

(def (core-memory-set-block! memory label value)
  "Set block in core memory"
  (case label
    (("persona") (set! (core-memory-persona memory) value))
    (("human") (set! (core-memory-human memory) value))
    (else (hash-put! (core-memory-custom memory) label value))))

(def (core-memory-has-block? memory label)
  "Check if core memory has block"
  (case label
    (("persona" "human") #t)
    (else (hash-key? (core-memory-custom memory) label))))

(def (core-memory-list-blocks memory)
  "List all block labels in core memory"
  (append '("persona" "human")
          (hash-keys (core-memory-custom memory))))

;;; ============================================================================
;;; Archival Memory Entry
;;; ============================================================================

(defstruct archival-entry
  (id           ; UUID
   agent-id     ; UUID
   content      ; Entry content
   embedding    ; Vector embedding (1536 dimensions)
   importance   ; Importance score (0.0-1.0)
   tags         ; Tags (list of strings)
   created-at)  ; Timestamp
  transparent: #t)

(def (validate-archival-params params)
  "Validate archival memory parameters"
  (let ((errors '()))

    ;; Validate content
    (unless (hash-key? params 'content)
      (set! errors (cons "Content is required" errors)))

    ;; Validate importance (if provided)
    (when (hash-key? params 'importance)
      (let ((importance (hash-ref params 'importance)))
        (unless (and (number? importance)
                     (>= importance 0.0)
                     (<= importance 1.0))
          (set! errors (cons "Importance must be between 0.0 and 1.0" errors)))))

    ;; Validate tags (if provided)
    (when (hash-key? params 'tags)
      (let ((tags (hash-ref params 'tags)))
        (unless (list? tags)
          (set! errors (cons "Tags must be a list" errors)))))

    ;; Return validation result
    (if (null? errors)
        (cons #t #f)
        (cons #f errors))))

;;; ============================================================================
;;; Recall Memory Entry
;;; ============================================================================

(defstruct recall-entry
  (id           ; UUID
   agent-id     ; UUID
   content      ; Entry content
   context      ; Context information
   created-at)  ; Timestamp
  transparent: #t)

;;; ============================================================================
;;; Memory Statistics
;;; ============================================================================

(defstruct memory-stats
  (core-blocks-count      ; Number of core memory blocks
   archival-entries-count ; Number of archival entries
   recall-entries-count   ; Number of recall entries
   total-memory-size      ; Total memory size (bytes)
   last-updated)          ; Last update timestamp
  transparent: #t)

(def (make-empty-memory-stats)
  "Create empty memory statistics"
  (make-memory-stats
   core-blocks-count: 0
   archival-entries-count: 0
   recall-entries-count: 0
   total-memory-size: 0
   last-updated: #f))

;;; ============================================================================
;;; Memory Conversion
;;; ============================================================================

(def (hash->memory-block h)
  "Convert hash to memory block struct"
  (make-memory-block
   id: (hash-ref h 'id)
   agent-id: (hash-ref h 'agent_id)
   label: (hash-ref h 'label)
   value: (hash-ref h 'value)
   is-template: (hash-ref h 'is_template #f)
   is-readonly: (hash-ref h 'is_readonly #f)
   created-at: (hash-ref h 'created_at)
   updated-at: (hash-ref h 'updated_at)))

(def (memory-block->hash block)
  "Convert memory block struct to hash"
  (hash
   'id (memory-block-id block)
   'agent_id (memory-block-agent-id block)
   'label (memory-block-label block)
   'value (memory-block-value block)
   'is_template (memory-block-is-template block)
   'is_readonly (memory-block-is-readonly block)
   'created_at (memory-block-created-at block)
   'updated_at (memory-block-updated-at block)))

(def (hash->archival-entry h)
  "Convert hash to archival entry struct"
  (make-archival-entry
   id: (hash-ref h 'id)
   agent-id: (hash-ref h 'agent_id)
   content: (hash-ref h 'content)
   embedding: (hash-ref h 'embedding #f)
   importance: (hash-ref h 'importance 0.5)
   tags: (hash-ref h 'tags '())
   created-at: (hash-ref h 'created_at)))

(def (archival-entry->hash entry)
  "Convert archival entry struct to hash"
  (hash
   'id (archival-entry-id entry)
   'agent_id (archival-entry-agent-id entry)
   'content (archival-entry-content entry)
   'embedding (archival-entry-embedding entry)
   'importance (archival-entry-importance entry)
   'tags (archival-entry-tags entry)
   'created_at (archival-entry-created-at entry)))

;;; ============================================================================
;;; Memory Utilities
;;; ============================================================================

(def (memory-block-size block)
  "Calculate memory block size in bytes"
  (let ((value (if (memory-block? block)
                   (memory-block-value block)
                   (hash-ref block 'value ""))))
    (string-length value)))

(def (archival-entry-size entry)
  "Calculate archival entry size in bytes"
  (let ((content (if (archival-entry? entry)
                     (archival-entry-content entry)
                     (hash-ref entry 'content ""))))
    (string-length content)))

(def (calculate-total-memory-size blocks archival-entries)
  "Calculate total memory size"
  (let ((block-size (apply + (map memory-block-size blocks)))
        (archival-size (apply + (map archival-entry-size archival-entries))))
    (+ block-size archival-size)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create memory block
(def block (make-memory-block
            id: "uuid-123"
            agent-id: "agent-456"
            label: "persona"
            value: "You are a helpful assistant."
            is-template: #f
            is-readonly: #f
            created-at: 1705401600
            updated-at: 1705401600))

;; Validate block parameters
(def params (hash 'label "persona" 'value "You are helpful."))
(def result (validate-block-params params))
(if (car result)
    (displayln "Valid block")
    (displayln (format "Invalid: ~a" (cdr result))))

;; Create core memory
(def memory (make-default-core-memory))
(displayln (format "Persona: ~a" (core-memory-persona memory)))

;; Get block template
(def template (get-block-template "persona"))
(displayln (format "Template: ~a" template))

;; Create archival entry
(def entry (make-archival-entry
            id: "uuid-789"
            agent-id: "agent-456"
            content: "Important information"
            embedding: #f
            importance: 0.8
            tags: '("important" "user_preference")
            created-at: 1705401600))
|#
