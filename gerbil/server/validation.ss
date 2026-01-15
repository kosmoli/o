;;; server/validation.ss - Request Validation Utilities
;;;
;;; This module provides utilities for validating HTTP requests.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/srfi/13
  ./http)

;;; ============================================================================
;;; Validation Errors
;;; ============================================================================

(defstruct validation-error
  (field      ; field name that failed validation
   message    ; error message
   value)     ; invalid value
  transparent: #t)

(def (make-validation-error-response errors)
  "Create validation error response"
  (make-error-response
   "Validation failed"
   status: 400
   details: (hash
             'validation_errors
             (map (lambda (err)
                    (hash
                     'field (validation-error-field err)
                     'message (validation-error-message err)))
                  errors))))

;;; ============================================================================
;;; Field Validators
;;; ============================================================================

(def (validate-required field-name value)
  "Validate that field is present and not empty"
  (if (or (not value)
          (and (string? value) (string-null? value)))
      (make-validation-error
       field: field-name
       message: (format "Field '~a' is required" field-name)
       value: value)
      #f))

(def (validate-string field-name value)
  "Validate that field is a string"
  (if (not (string? value))
      (make-validation-error
       field: field-name
       message: (format "Field '~a' must be a string" field-name)
       value: value)
      #f))

(def (validate-number field-name value)
  "Validate that field is a number"
  (if (not (number? value))
      (make-validation-error
       field: field-name
       message: (format "Field '~a' must be a number" field-name)
       value: value)
      #f))

(def (validate-boolean field-name value)
  "Validate that field is a boolean"
  (if (not (boolean? value))
      (make-validation-error
       field: field-name
       message: (format "Field '~a' must be a boolean" field-name)
       value: value)
      #f))

(def (validate-hash field-name value)
  "Validate that field is a hash"
  (if (not (hash? value))
      (make-validation-error
       field: field-name
       message: (format "Field '~a' must be an object" field-name)
       value: value)
      #f))

(def (validate-list field-name value)
  "Validate that field is a list"
  (if (not (list? value))
      (make-validation-error
       field: field-name
       message: (format "Field '~a' must be an array" field-name)
       value: value)
      #f))

(def (validate-min-length field-name value min-len)
  "Validate minimum string length"
  (if (and (string? value) (< (string-length value) min-len))
      (make-validation-error
       field: field-name
       message: (format "Field '~a' must be at least ~a characters" field-name min-len)
       value: value)
      #f))

(def (validate-max-length field-name value max-len)
  "Validate maximum string length"
  (if (and (string? value) (> (string-length value) max-len))
      (make-validation-error
       field: field-name
       message: (format "Field '~a' must be at most ~a characters" field-name max-len)
       value: value)
      #f))

(def (validate-min-value field-name value min-val)
  "Validate minimum numeric value"
  (if (and (number? value) (< value min-val))
      (make-validation-error
       field: field-name
       message: (format "Field '~a' must be at least ~a" field-name min-val)
       value: value)
      #f))

(def (validate-max-value field-name value max-val)
  "Validate maximum numeric value"
  (if (and (number? value) (> value max-val))
      (make-validation-error
       field: field-name
       message: (format "Field '~a' must be at most ~a" field-name max-val)
       value: value)
      #f))

(def (validate-enum field-name value allowed-values)
  "Validate that value is in allowed set"
  (if (not (member value allowed-values))
      (make-validation-error
       field: field-name
       message: (format "Field '~a' must be one of: ~a" field-name allowed-values)
       value: value)
      #f))

(def (validate-pattern field-name value pattern)
  "Validate that value matches regex pattern"
  (if (and (string? value) (not (regexp-match pattern value)))
      (make-validation-error
       field: field-name
       message: (format "Field '~a' has invalid format" field-name)
       value: value)
      #f))

(def (validate-email field-name value)
  "Validate email format"
  (validate-pattern
   field-name
   value
   "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"))

(def (validate-uuid field-name value)
  "Validate UUID format"
  (validate-pattern
   field-name
   value
   "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"))

;;; ============================================================================
;;; Schema Validation
;;; ============================================================================

(defstruct field-schema
  (name          ; field name
   required?     ; is field required?
   type          ; expected type (:string :number :boolean :hash :list)
   validators)   ; list of validator functions
  transparent: #t)

(def (make-field-schema-instance
      name
      #!key
      (required? #f)
      (type #f)
      (validators '()))
  "Create field schema"
  (make-field-schema
   name: name
   required?: required?
   type: type
   validators: validators))

(def (validate-field schema value)
  "Validate field against schema"
  (let ((errors '()))

    ;; Check required
    (when (field-schema-required? schema)
      (let ((err (validate-required (field-schema-name schema) value)))
        (when err
          (set! errors (cons err errors)))))

    ;; Check type
    (when (and value (field-schema-type schema))
      (let ((type-validator
             (case (field-schema-type schema)
               ((:string) validate-string)
               ((:number) validate-number)
               ((:boolean) validate-boolean)
               ((:hash) validate-hash)
               ((:list) validate-list)
               (else #f))))
        (when type-validator
          (let ((err (type-validator (field-schema-name schema) value)))
            (when err
              (set! errors (cons err errors)))))))

    ;; Run custom validators
    (when value
      (for-each
       (lambda (validator)
         (let ((err (validator (field-schema-name schema) value)))
           (when err
             (set! errors (cons err errors)))))
       (field-schema-validators schema)))

    errors))

(def (validate-schema data schema-list)
  "Validate data against schema
   Returns: list of validation errors (empty if valid)"
  (let ((all-errors '()))

    (for-each
     (lambda (schema)
       (let* ((field-name (field-schema-name schema))
              (value (hash-ref data field-name #f))
              (errors (validate-field schema value)))
         (set! all-errors (append all-errors errors))))
     schema-list)

    all-errors))

;;; ============================================================================
;;; Request Validators
;;; ============================================================================

(def (validate-create-agent-request data)
  "Validate create agent request"
  (let ((schema (list
                 (make-field-schema-instance
                  'name
                  required?: #t
                  type: :string
                  validators: (list
                               (lambda (field value)
                                 (validate-min-length field value 1))
                               (lambda (field value)
                                 (validate-max-length field value 100))))

                 (make-field-schema-instance
                  'llm_config
                  required?: #f
                  type: :hash)

                 (make-field-schema-instance
                  'system_prompt
                  required?: #f
                  type: :string
                  validators: (list
                               (lambda (field value)
                                 (validate-max-length field value 10000))))

                 (make-field-schema-instance
                  'memory_config
                  required?: #f
                  type: :hash))))

    (validate-schema data schema)))

(def (validate-send-message-request data)
  "Validate send message request"
  (let ((schema (list
                 (make-field-schema-instance
                  'message
                  required?: #t
                  type: :string
                  validators: (list
                               (lambda (field value)
                                 (validate-min-length field value 1))
                               (lambda (field value)
                                 (validate-max-length field value 100000))))

                 (make-field-schema-instance
                  'stream
                  required?: #f
                  type: :boolean))))

    (validate-schema data schema)))

(def (validate-search-messages-request data)
  "Validate search messages request"
  (let ((schema (list
                 (make-field-schema-instance
                  'query
                  required?: #t
                  type: :string
                  validators: (list
                               (lambda (field value)
                                 (validate-min-length field value 1))))

                 (make-field-schema-instance
                  'limit
                  required?: #f
                  type: :number
                  validators: (list
                               (lambda (field value)
                                 (validate-min-value field value 1))
                               (lambda (field value)
                                 (validate-max-value field value 100))))

                 (make-field-schema-instance
                  'search_type
                  required?: #f
                  type: :string
                  validators: (list
                               (lambda (field value)
                                 (validate-enum field value '("text" "semantic"))))))))

    (validate-schema data schema)))

;;; ============================================================================
;;; Validation Middleware
;;; ============================================================================

(def (with-validation validator)
  "Create middleware that validates request body"
  (lambda (handler)
    (lambda (req)
      (if (not (http-request-json req))
          (make-error-response "JSON body required" status: 400)

          (let ((errors (validator (http-request-json req))))
            (if (null? errors)
                (handler req)
                (make-validation-error-response errors)))))))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Validate create agent request
(def data (hash
           'name "MyAgent"
           'llm_config (hash 'provider "openai")
           'system_prompt "You are helpful."))

(def errors (validate-create-agent-request data))

(if (null? errors)
    (displayln "Valid!")
    (displayln (format "Validation errors: ~a" errors)))

;; Use validation middleware
(def (my-handler req)
  (make-json-response (hash 'ok #t)))

(def validated-handler
  ((with-validation validate-create-agent-request) my-handler))

;; Register route with validation
(post! router "/v1/agents"
       ((with-validation validate-create-agent-request)
        create-agent-handler))
|#
