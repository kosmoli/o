#lang racket

;;; tools/types.rkt - Tool System Type Definitions
;;;
;;; Type definitions and validation for the tool system.

(provide (struct-out tool-definition)
         (struct-out tool-parameter)
         (struct-out tool-call)
         (struct-out tool-result)
         (struct-out tool-registry)
         (struct-out tool-execution-context)
         tool-categories
         valid-tool-category?
         tool-parameter-types
         valid-parameter-type?
         tool-call-statuses
         valid-tool-call-status?
         validate-tool-name
         validate-tool-parameters
         validate-tool-definition
         validate-tool-arguments
         tool-definition->hash
         tool-call->hash
         tool-result->hash
         hash->tool-call
         make-success-result
         make-error-result
         tool-call-pending?
         tool-call-approved?
         tool-call-executing?
         tool-call-completed?
         tool-call-failed?
         ;; Aliases for backward compatibility
         (rename-out [tool-definition tool-definition-internal])
         (rename-out [tool-parameter tool-parameter-internal])
         (rename-out [tool-call tool-execution])
         (rename-out [tool-definition-name tool-definition-internal-name])
         (rename-out [tool-definition-description tool-definition-internal-description])
         (rename-out [tool-definition-parameters tool-definition-internal-parameters])
         (rename-out [tool-definition-handler tool-definition-internal-handler])
         (rename-out [tool-definition-category tool-definition-internal-category])
         (rename-out [tool-definition-requires-approval tool-definition-internal-requires-approval])
         (rename-out [tool-definition-metadata tool-definition-internal-metadata])
         (rename-out [tool-call-id tool-execution-id])
         (rename-out [tool-call-tool-name tool-execution-tool-name])
         (rename-out [tool-call-arguments tool-execution-arguments])
         (rename-out [tool-call-timestamp tool-execution-timestamp])
         (rename-out [tool-call-agent-id tool-execution-agent-id])
         (rename-out [tool-call-status tool-execution-status])
         (rename-out [tool-call-result tool-execution-result])
         (rename-out [tool-call-error tool-execution-error])
         (rename-out [set-tool-call-status! set-tool-execution-status!])
         (rename-out [set-tool-call-result! set-tool-execution-result!])
         (rename-out [set-tool-call-error! set-tool-execution-error!])
         validate-tool-definition-internal
         tool-execution->hash
         tool-execution-pending?
         tool-execution-approved?
         tool-execution-executing?
         tool-execution-completed?
         tool-execution-failed?
         hash->tool-execution)

(require racket/hash
        racket/format
        racket/string
        racket/list)

;;; ============================================================================
;;; Tool Structures
;;; ============================================================================

(struct tool-definition
  (name              ; Tool name (string)
   description       ; Tool description (string)
   parameters        ; Parameter schema (hash)
   handler           ; Tool handler function
   category          ; Tool category (symbol: 'core, 'memory, 'custom)
   requires-approval ; Requires user approval? (boolean)
   metadata)         ; Additional metadata (hash)
  #:transparent)

(struct tool-parameter
  (name              ; Parameter name (string)
   type              ; Parameter type (symbol: 'string, 'number, 'boolean, 'object, 'array)
   description       ; Parameter description (string)
   required          ; Is required? (boolean)
   default           ; Default value (optional)
   enum              ; Allowed values (optional list)
   validation)       ; Custom validation function (optional)
  #:transparent)

(struct tool-call
  (id                ; Unique call ID (string)
   tool-name         ; Tool name (string)
   arguments         ; Tool arguments (hash)
   timestamp         ; Call timestamp (seconds)
   agent-id          ; Agent ID (string)
   status            ; Call status (symbol: 'pending, 'approved, 'rejected, 'executing, 'completed, 'failed)
   result            ; Tool result (optional)
   error)            ; Error message (optional)
  #:transparent
  #:mutable)

(struct tool-result
  (success           ; Was successful? (boolean)
   value             ; Result value (any)
   error             ; Error message (optional string)
   metadata)         ; Additional metadata (hash)
  #:transparent)

(struct tool-registry
  (tools             ; Hash mapping tool names to tool-definition
   categories)       ; Hash mapping categories to tool name lists
  #:transparent)

(struct tool-execution-context
  (agent-id          ; Agent ID
   call-id           ; Tool call ID
   arguments         ; Tool arguments
   timestamp         ; Execution timestamp
   metadata)         ; Additional context metadata
  #:transparent)

;;; ============================================================================
;;; Tool Categories
;;; ============================================================================

(define tool-categories
  '(core           ; Core tools (send_message, conversation_search)
    memory         ; Memory tools (core_memory_append, archival_memory_insert, etc.)
    system         ; System tools (internal operations)
    custom))       ; Custom user-defined tools

(define (valid-tool-category? category)
  "Check if category is valid"
  (member category tool-categories))

;;; ============================================================================
;;; Tool Parameter Types
;;; ============================================================================

(define tool-parameter-types
  '(string
    number
    integer
    boolean
    object
    array
    null))

(define (valid-parameter-type? type)
  "Check if parameter type is valid"
  (member type tool-parameter-types))

;;; ============================================================================
;;; Tool Call Status
;;; ============================================================================

(define tool-call-statuses
  '(pending        ; Waiting for approval
    approved       ; Approved, ready to execute
    rejected       ; Rejected by user
    executing      ; Currently executing
    completed      ; Successfully completed
    failed))       ; Failed with error

(define (valid-tool-call-status? status)
  "Check if tool call status is valid"
  (member status tool-call-statuses))

;;; ============================================================================
;;; Validation Functions
;;; ============================================================================

(define (validate-tool-name name)
  "Validate tool name

   Args:
     name: Tool name

   Returns:
     (cons #t #f) if valid, (cons #f errors) if invalid"

  (cond
   [(not (string? name))
    (cons #f (list "Tool name must be a string"))]

   [(string=? name "")
    (cons #f (list "Tool name cannot be empty"))]

   [(not (char-alphabetic? (string-ref name 0)))
    (cons #f (list "Tool name must start with a letter"))]

   [else
    (cons #t #f)]))

(define (validate-tool-parameters params)
  "Validate tool parameters schema

   Args:
     params: Parameter schema (hash)

   Returns:
     (cons #t #f) if valid, (cons #f errors) if invalid"

  (define errors '())
  (for ([(param-name param-def) (in-hash params)])
    (unless (hash? param-def)
      (set! errors (cons (format "Parameter ~a must be a hash" param-name) errors)))

    (unless (hash-has-key? param-def 'type)
      (set! errors (cons (format "Parameter ~a missing type" param-name) errors)))

    (when (hash-has-key? param-def 'type)
      (define type (hash-ref param-def 'type))
      (unless (valid-parameter-type? type)
        (set! errors (cons (format "Parameter ~a has invalid type: ~a" param-name type) errors)))))

  (if (null? errors)
      (cons #t #f)
      (cons #f (reverse errors))))

(define (validate-tool-definition tool-def)
  "Validate tool definition

   Args:
     tool-def: Tool definition structure

   Returns:
     (cons #t #f) if valid, (cons #f errors) if invalid"

  (define errors '())

  ;; Validate name
  (define name-result (validate-tool-name (tool-definition-name tool-def)))
  (unless (car name-result)
    (set! errors (append errors (cdr name-result))))

  ;; Validate description
  (unless (string? (tool-definition-description tool-def))
    (set! errors (cons "Tool description must be a string" errors)))

  ;; Validate parameters
  (define params-result (validate-tool-parameters (tool-definition-parameters tool-def)))
  (unless (car params-result)
    (set! errors (append errors (cdr params-result))))

  ;; Validate handler
  (unless (procedure? (tool-definition-handler tool-def))
    (set! errors (cons "Tool handler must be a procedure" errors)))

  ;; Validate category
  (unless (valid-tool-category? (tool-definition-category tool-def))
    (set! errors (cons (format "Invalid tool category: ~a" (tool-definition-category tool-def)) errors)))

  (if (null? errors)
      (cons #t #f)
      (cons #f (reverse errors))))

(define (validate-tool-arguments tool-def arguments)
  "Validate tool call arguments against parameter schema

   Args:
     tool-def: Tool definition
     arguments: Tool arguments (hash)

   Returns:
     (cons #t #f) if valid, (cons #f errors) if invalid"

  (define errors '())
  (define params (tool-definition-parameters tool-def))

  ;; Check required parameters
  (for ([(param-name param-def) (in-hash params)])
    (when (hash-ref param-def 'required #f)
      (unless (hash-has-key? arguments param-name)
        (set! errors (cons (format "Missing required parameter: ~a" param-name) errors)))))

  ;; Validate provided arguments
  (for ([(arg-name arg-value) (in-hash arguments)])
    (if (hash-has-key? params arg-name)
        (let* ([param-def (hash-ref params arg-name)]
               [type (hash-ref param-def 'type)])
          ;; Type validation
          (case type
            [(string)
             (unless (string? arg-value)
               (set! errors (cons (format "Parameter ~a must be a string" arg-name) errors)))]
            [(number)
             (unless (number? arg-value)
               (set! errors (cons (format "Parameter ~a must be a number" arg-name) errors)))]
            [(integer)
             (unless (integer? arg-value)
               (set! errors (cons (format "Parameter ~a must be an integer" arg-name) errors)))]
            [(boolean)
             (unless (boolean? arg-value)
               (set! errors (cons (format "Parameter ~a must be a boolean" arg-name) errors)))]
            [(object)
             (unless (hash? arg-value)
               (set! errors (cons (format "Parameter ~a must be an object" arg-name) errors)))]
            [(array)
             (unless (list? arg-value)
               (set! errors (cons (format "Parameter ~a must be an array" arg-name) errors)))])

          ;; Enum validation
          (when (hash-has-key? param-def 'enum)
            (define allowed (hash-ref param-def 'enum))
            (unless (member arg-value allowed)
              (set! errors (cons (format "Parameter ~a must be one of: ~a" arg-name allowed) errors))))

          ;; Custom validation
          (when (hash-has-key? param-def 'validation)
            (define validator (hash-ref param-def 'validation))
            (when (procedure? validator)
              (define result (validator arg-value))
              (unless (car result)
                (set! errors (append errors (cdr result)))))))
        ;; Unknown parameter
        (set! errors (cons (format "Unknown parameter: ~a" arg-name) errors))))

  (if (null? errors)
      (cons #t #f)
      (cons #f (reverse errors))))

;;; ============================================================================
;;; Conversion Functions
;;; ============================================================================

(define (tool-definition->hash tool-def)
  "Convert tool definition to hash

   Args:
     tool-def: Tool definition structure

   Returns:
     Hash representation"

  (hash 'name (tool-definition-name tool-def)
        'description (tool-definition-description tool-def)
        'parameters (tool-definition-parameters tool-def)
        'category (symbol->string (tool-definition-category tool-def))
        'requires_approval (tool-definition-requires-approval tool-def)
        'metadata (tool-definition-metadata tool-def)))

(define (tool-call->hash call)
  "Convert tool call to hash

   Args:
     call: Tool call structure

   Returns:
     Hash representation"

  (hash 'id (tool-call-id call)
        'tool_name (tool-call-tool-name call)
        'arguments (tool-call-arguments call)
        'timestamp (tool-call-timestamp call)
        'agent_id (tool-call-agent-id call)
        'status (symbol->string (tool-call-status call))
        'result (tool-call-result call)
        'error (tool-call-error call)))

(define (tool-result->hash result)
  "Convert tool result to hash

   Args:
     result: Tool result structure

   Returns:
     Hash representation"

  (hash 'success (tool-result-success result)
        'value (tool-result-value result)
        'error (tool-result-error result)
        'metadata (tool-result-metadata result)))

(define (hash->tool-call h)
  "Convert hash to tool call

   Args:
     h: Hash representation

   Returns:
     Tool call structure"

  (tool-call
   (hash-ref h 'id)
   (hash-ref h 'tool_name)
   (hash-ref h 'arguments)
   (hash-ref h 'timestamp)
   (hash-ref h 'agent_id)
   (string->symbol (hash-ref h 'status))
   (hash-ref h 'result #f)
   (hash-ref h 'error #f)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (make-success-result value #:metadata [metadata (hash)])
  "Create successful tool result

   Args:
     value: Result value
     metadata: Additional metadata (default: empty hash)

   Returns:
     Tool result structure"

  (tool-result #t value #f metadata))

(define (make-error-result error-message #:metadata [metadata (hash)])
  "Create error tool result

   Args:
     error-message: Error message
     metadata: Additional metadata (default: empty hash)

   Returns:
     Tool result structure"

  (tool-result #f #f error-message metadata))

(define (tool-call-pending? call)
  "Check if tool call is pending"
  (eq? (tool-call-status call) 'pending))

(define (tool-call-approved? call)
  "Check if tool call is approved"
  (eq? (tool-call-status call) 'approved))

(define (tool-call-executing? call)
  "Check if tool call is executing"
  (eq? (tool-call-status call) 'executing))

(define (tool-call-completed? call)
  "Check if tool call is completed"
  (eq? (tool-call-status call) 'completed))

(define (tool-call-failed? call)
  "Check if tool call failed"
  (eq? (tool-call-status call) 'failed))

;;; ============================================================================
;;; Backward Compatibility Aliases
;;; ============================================================================
;;; These aliases provide compatibility with code using the old naming convention

;; Function aliases (using rename-out for struct types above)
(define validate-tool-definition-internal validate-tool-definition)
(define tool-execution->hash tool-call->hash)
(define hash->tool-execution hash->tool-call)
(define tool-execution-pending? tool-call-pending?)
(define tool-execution-approved? tool-call-approved?)
(define tool-execution-executing? tool-call-executing?)
(define tool-execution-completed? tool-call-completed?)
(define tool-execution-failed? tool-call-failed?)
