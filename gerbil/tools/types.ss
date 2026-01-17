;;; tools/types.ss - Tool System Type Definitions
;;;
;;; Type definitions and validation for the tool system.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/text/json)

;;; ============================================================================
;;; Tool Structures
;;; ============================================================================

(defstruct tool-definition
  (name              ; Tool name (string)
   description       ; Tool description (string)
   parameters        ; Parameter schema (hash)
   handler           ; Tool handler function
   category          ; Tool category (symbol: :core, :memory, :custom)
   requires-approval ; Requires user approval? (boolean)
   metadata)         ; Additional metadata (hash)
  transparent: #t)

(defstruct tool-parameter
  (name              ; Parameter name (string)
   type              ; Parameter type (symbol: :string, :number, :boolean, :object, :array)
   description       ; Parameter description (string)
   required          ; Is required? (boolean)
   default           ; Default value (optional)
   enum              ; Allowed values (optional list)
   validation)       ; Custom validation function (optional)
  transparent: #t)

(defstruct tool-call
  (id                ; Unique call ID (string)
   tool-name         ; Tool name (string)
   arguments         ; Tool arguments (hash)
   timestamp         ; Call timestamp (seconds)
   agent-id          ; Agent ID (string)
   status            ; Call status (symbol: :pending, :approved, :rejected, :executing, :completed, :failed)
   result            ; Tool result (optional)
   error)            ; Error message (optional)
  transparent: #t)

(defstruct tool-result
  (success           ; Was successful? (boolean)
   value             ; Result value (any)
   error             ; Error message (optional string)
   metadata)         ; Additional metadata (hash)
  transparent: #t)

(defstruct tool-registry
  (tools             ; Hash mapping tool names to tool-definition
   categories)       ; Hash mapping categories to tool name lists
  transparent: #t)

(defstruct tool-execution-context
  (agent-id          ; Agent ID
   call-id           ; Tool call ID
   arguments         ; Tool arguments
   timestamp         ; Execution timestamp
   metadata)         ; Additional context metadata
  transparent: #t)

;;; ============================================================================
;;; Tool Categories
;;; ============================================================================

(def tool-categories
  '(:core           ; Core tools (send_message, conversation_search)
    :memory         ; Memory tools (core_memory_append, archival_memory_insert, etc.)
    :system         ; System tools (internal operations)
    :custom))       ; Custom user-defined tools

(def (valid-tool-category? category)
  "Check if category is valid"
  (member category tool-categories))

;;; ============================================================================
;;; Tool Parameter Types
;;; ============================================================================

(def tool-parameter-types
  '(:string
    :number
    :integer
    :boolean
    :object
    :array
    :null))

(def (valid-parameter-type? type)
  "Check if parameter type is valid"
  (member type tool-parameter-types))

;;; ============================================================================
;;; Tool Call Status
;;; ============================================================================

(def tool-call-statuses
  '(:pending        ; Waiting for approval
    :approved       ; Approved, ready to execute
    :rejected       ; Rejected by user
    :executing      ; Currently executing
    :completed      ; Successfully completed
    :failed))       ; Failed with error

(def (valid-tool-call-status? status)
  "Check if tool call status is valid"
  (member status tool-call-statuses))

;;; ============================================================================
;;; Validation Functions
;;; ============================================================================

(def (validate-tool-name name)
  "Validate tool name

   Args:
     name: Tool name

   Returns:
     (cons #t #f) if valid, (cons #f errors) if invalid"

  (cond
   ((not (string? name))
    (cons #f (list "Tool name must be a string")))

   ((string=? name "")
    (cons #f (list "Tool name cannot be empty")))

   ((not (char-alphabetic? (string-ref name 0)))
    (cons #f (list "Tool name must start with a letter")))

   (else
    (cons #t #f))))

(def (validate-tool-parameters params)
  "Validate tool parameters schema

   Args:
     params: Parameter schema (hash)

   Returns:
     (cons #t #f) if valid, (cons #f errors) if invalid"

  (let ((errors '()))
    (hash-for-each
     (lambda (param-name param-def)
       (unless (hash-table? param-def)
         (set! errors (cons (format "Parameter ~a must be a hash" param-name) errors)))

       (unless (hash-key? param-def 'type)
         (set! errors (cons (format "Parameter ~a missing type" param-name) errors)))

       (when (hash-key? param-def 'type)
         (let ((type (hash-ref param-def 'type)))
           (unless (valid-parameter-type? type)
             (set! errors (cons (format "Parameter ~a has invalid type: ~a" param-name type) errors))))))
     params)

    (if (null? errors)
        (cons #t #f)
        (cons #f (reverse errors)))))

(def (validate-tool-definition tool-def)
  "Validate tool definition

   Args:
     tool-def: Tool definition structure

   Returns:
     (cons #t #f) if valid, (cons #f errors) if invalid"

  (let ((errors '()))
    ;; Validate name
    (let ((name-result (validate-tool-name (tool-definition-name tool-def))))
      (unless (car name-result)
        (set! errors (append errors (cdr name-result)))))

    ;; Validate description
    (unless (string? (tool-definition-description tool-def))
      (set! errors (cons "Tool description must be a string" errors)))

    ;; Validate parameters
    (let ((params-result (validate-tool-parameters (tool-definition-parameters tool-def))))
      (unless (car params-result)
        (set! errors (append errors (cdr params-result)))))

    ;; Validate handler
    (unless (procedure? (tool-definition-handler tool-def))
      (set! errors (cons "Tool handler must be a procedure" errors)))

    ;; Validate category
    (unless (valid-tool-category? (tool-definition-category tool-def))
      (set! errors (cons (format "Invalid tool category: ~a" (tool-definition-category tool-def)) errors)))

    (if (null? errors)
        (cons #t #f)
        (cons #f (reverse errors)))))

(def (validate-tool-arguments tool-def arguments)
  "Validate tool call arguments against parameter schema

   Args:
     tool-def: Tool definition
     arguments: Tool arguments (hash)

   Returns:
     (cons #t #f) if valid, (cons #f errors) if invalid"

  (let ((errors '())
        (params (tool-definition-parameters tool-def)))

    ;; Check required parameters
    (hash-for-each
     (lambda (param-name param-def)
       (when (hash-ref param-def 'required #f)
         (unless (hash-key? arguments param-name)
           (set! errors (cons (format "Missing required parameter: ~a" param-name) errors)))))
     params)

    ;; Validate provided arguments
    (hash-for-each
     (lambda (arg-name arg-value)
       (if (hash-key? params arg-name)
           (let ((param-def (hash-ref params arg-name)))
             (let ((type (hash-ref param-def 'type)))
               ;; Type validation
               (case type
                 ((:string)
                  (unless (string? arg-value)
                    (set! errors (cons (format "Parameter ~a must be a string" arg-name) errors))))
                 ((:number)
                  (unless (number? arg-value)
                    (set! errors (cons (format "Parameter ~a must be a number" arg-name) errors))))
                 ((:integer)
                  (unless (integer? arg-value)
                    (set! errors (cons (format "Parameter ~a must be an integer" arg-name) errors))))
                 ((:boolean)
                  (unless (boolean? arg-value)
                    (set! errors (cons (format "Parameter ~a must be a boolean" arg-name) errors))))
                 ((:object)
                  (unless (hash-table? arg-value)
                    (set! errors (cons (format "Parameter ~a must be an object" arg-name) errors))))
                 ((:array)
                  (unless (list? arg-value)
                    (set! errors (cons (format "Parameter ~a must be an array" arg-name) errors)))))

               ;; Enum validation
               (when (hash-key? param-def 'enum)
                 (let ((allowed (hash-ref param-def 'enum)))
                   (unless (member arg-value allowed)
                     (set! errors (cons (format "Parameter ~a must be one of: ~a" arg-name allowed) errors)))))

               ;; Custom validation
               (when (hash-key? param-def 'validation)
                 (let ((validator (hash-ref param-def 'validation)))
                   (when (procedure? validator)
                     (let ((result (validator arg-value)))
                       (unless (car result)
                         (set! errors (append errors (cdr result))))))))))
           ;; Unknown parameter
           (set! errors (cons (format "Unknown parameter: ~a" arg-name) errors))))
     arguments)

    (if (null? errors)
        (cons #t #f)
        (cons #f (reverse errors)))))

;;; ============================================================================
;;; Conversion Functions
;;; ============================================================================

(def (tool-definition->hash tool-def)
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

(def (tool-call->hash call)
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

(def (tool-result->hash result)
  "Convert tool result to hash

   Args:
     result: Tool result structure

   Returns:
     Hash representation"

  (hash 'success (tool-result-success result)
        'value (tool-result-value result)
        'error (tool-result-error result)
        'metadata (tool-result-metadata result)))

(def (hash->tool-call h)
  "Convert hash to tool call

   Args:
     h: Hash representation

   Returns:
     Tool call structure"

  (make-tool-call
   id: (hash-ref h 'id)
   tool-name: (hash-ref h 'tool_name)
   arguments: (hash-ref h 'arguments)
   timestamp: (hash-ref h 'timestamp)
   agent-id: (hash-ref h 'agent_id)
   status: (string->symbol (hash-ref h 'status))
   result: (hash-ref h 'result #f)
   error: (hash-ref h 'error #f)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (make-success-result value #!key (metadata (hash)))
  "Create successful tool result

   Args:
     value: Result value
     metadata: Additional metadata (default: empty hash)

   Returns:
     Tool result structure"

  (make-tool-result
   success: #t
   value: value
   error: #f
   metadata: metadata))

(def (make-error-result error-message #!key (metadata (hash)))
  "Create error tool result

   Args:
     error-message: Error message
     metadata: Additional metadata (default: empty hash)

   Returns:
     Tool result structure"

  (make-tool-result
   success: #f
   value: #f
   error: error-message
   metadata: metadata))

(def (tool-call-pending? call)
  "Check if tool call is pending"
  (eq? (tool-call-status call) :pending))

(def (tool-call-approved? call)
  "Check if tool call is approved"
  (eq? (tool-call-status call) :approved))

(def (tool-call-executing? call)
  "Check if tool call is executing"
  (eq? (tool-call-status call) :executing))

(def (tool-call-completed? call)
  "Check if tool call is completed"
  (eq? (tool-call-status call) :completed))

(def (tool-call-failed? call)
  "Check if tool call failed"
  (eq? (tool-call-status call) :failed))
