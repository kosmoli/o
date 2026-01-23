#lang racket

;;; server/validation.rkt - Request Validation Utilities
;;;
;;; This module provides utilities for validating HTTP requests.

(provide validation-error
         validation-error-field
         validation-error-message
         validation-error-value
         make-validation-error-response
         validate-required
         validate-string
         validate-number
         validate-boolean
         validate-hash
         validate-list
         validate-min-length
         validate-max-length
         validate-min-value
         validate-max-value
         validate-enum
         validate-pattern
         validate-email
         validate-uuid
         field-schema
         field-schema-name
         field-schema-required?
         field-schema-type
         field-schema-validators
         make-field-schema-instance
         validate-field
         validate-schema
         validate-create-agent-request
         validate-send-message-request
         validate-search-messages-request
         with-validation)

(require racket/hash
        racket/format
        "./http.rkt")

;;; ============================================================================
;;; Validation Errors
;;; ============================================================================

(struct validation-error
  (field      ; field name that failed validation
   message    ; error message
   value)     ; invalid value
  #:transparent)

(define (make-validation-error-response errors)
  "Create validation error response"
  (make-error-response
   "Validation failed"
   #:status 400
   #:code "VALIDATION_ERROR"))

;;; ============================================================================
;;; Field Validators
;;; ============================================================================

(define (validate-required field-name value)
  "Validate that field is present and not empty"
  (if (or (not value)
          (and (string? value) (string=? value "")))
      (validation-error
       field-name
       (format "Field '~a' is required" field-name)
       value)
      #f))

(define (validate-string field-name value)
  "Validate that field is a string"
  (if (and value (not (string? value)))
      (validation-error
       field-name
       (format "Field '~a' must be a string" field-name)
       value)
      #f))

(define (validate-number field-name value)
  "Validate that field is a number"
  (if (and value (not (number? value)))
      (validation-error
       field-name
       (format "Field '~a' must be a number" field-name)
       value)
      #f))

(define (validate-boolean field-name value)
  "Validate that field is a boolean"
  (if (and value (not (boolean? value)))
      (validation-error
       field-name
       (format "Field '~a' must be a boolean" field-name)
       value)
      #f))

(define (validate-hash field-name value)
  "Validate that field is a hash"
  (if (and value (not (hash? value)))
      (validation-error
       field-name
       (format "Field '~a' must be an object" field-name)
       value)
      #f))

(define (validate-list field-name value)
  "Validate that field is a list"
  (if (and value (not (list? value)))
      (validation-error
       field-name
       (format "Field '~a' must be an array" field-name)
       value)
      #f))

(define (validate-min-length field-name value min-len)
  "Validate minimum string length"
  (if (and (string? value) (< (string-length value) min-len))
      (validation-error
       field-name
       (format "Field '~a' must be at least ~a characters" field-name min-len)
       value)
      #f))

(define (validate-max-length field-name value max-len)
  "Validate maximum string length"
  (if (and (string? value) (> (string-length value) max-len))
      (validation-error
       field-name
       (format "Field '~a' must be at most ~a characters" field-name max-len)
       value)
      #f))

(define (validate-min-value field-name value min-val)
  "Validate minimum numeric value"
  (if (and (number? value) (< value min-val))
      (validation-error
       field-name
       (format "Field '~a' must be at least ~a" field-name min-val)
       value)
      #f))

(define (validate-max-value field-name value max-val)
  "Validate maximum numeric value"
  (if (and (number? value) (> value max-val))
      (validation-error
       field-name
       (format "Field '~a' must be at most ~a" field-name max-val)
       value)
      #f))

(define (validate-enum field-name value allowed-values)
  "Validate that value is in allowed set"
  (if (and value (not (member value allowed-values)))
      (validation-error
       field-name
       (format "Field '~a' must be one of: ~a" field-name allowed-values)
       value)
      #f))

(define (validate-pattern field-name value pattern)
  "Validate that value matches regex pattern"
  (if (and (string? value) (not (regexp-match? pattern value)))
      (validation-error
       field-name
       (format "Field '~a' has invalid format" field-name)
       value)
      #f))

(define (validate-email field-name value)
  "Validate email format"
  (validate-pattern
   field-name
   value
   #rx"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"))

(define (validate-uuid field-name value)
  "Validate UUID format"
  (validate-pattern
   field-name
   value
   #rx"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"))

;;; ============================================================================
;;; Schema Validation
;;; ============================================================================

(struct field-schema
  (name          ; field name
   required?     ; is field required?
   type          ; expected type ('string 'number 'boolean 'hash 'list)
   validators)   ; list of validator functions
  #:transparent)

(define (make-field-schema-instance
         name
         #:required? [required? #f]
         #:type [type #f]
         #:validators [validators '()])
  "Create field schema"
  (field-schema name required? type validators))

(define (validate-field schema value)
  "Validate field against schema"
  (define errors '())

  ;; Check required
  (when (field-schema-required? schema)
    (define err (validate-required (field-schema-name schema) value))
    (when err
      (set! errors (cons err errors))))

  ;; Check type
  (when (and value (field-schema-type schema))
    (define type-validator
      (case (field-schema-type schema)
        [(string) validate-string]
        [(number) validate-number]
        [(boolean) validate-boolean]
        [(hash) validate-hash]
        [(list) validate-list]
        [else #f]))
    (when type-validator
      (define err (type-validator (field-schema-name schema) value))
      (when err
        (set! errors (cons err errors)))))

  ;; Run custom validators
  (when value
    (for-each
     (lambda (validator)
       (define err (validator (field-schema-name schema) value))
       (when err
         (set! errors (cons err errors))))
     (field-schema-validators schema)))

  errors)

(define (validate-schema data schema-list)
  "Validate data against schema
   Returns: list of validation errors (empty if valid)"
  (define all-errors '())
  (for-each
   (lambda (schema)
     (define field-name (field-schema-name schema))
     (define value (hash-ref data field-name #f))
     (define errors (validate-field schema value))
     (set! all-errors (append all-errors errors)))
   schema-list)
  all-errors)

;;; ============================================================================
;;; Request Validators
;;; ============================================================================

(define (validate-create-agent-request data)
  "Validate create agent request"
  (define schema
    (list
     (make-field-schema-instance
      'name
      #:required? #t
      #:type 'string
      #:validators (list
                    (lambda (f v) (validate-min-length f v 1))
                    (lambda (f v) (validate-max-length f v 100))))
     (make-field-schema-instance
      'llm_config
      #:required? #f
      #:type 'hash)
     (make-field-schema-instance
      'system_prompt
      #:required? #f
      #:type 'string
      #:validators (list
                    (lambda (f v) (validate-max-length f v 10000))))
     (make-field-schema-instance
      'memory_config
      #:required? #f
      #:type 'hash)))
  (validate-schema data schema))

(define (validate-send-message-request data)
  "Validate send message request"
  (define schema
    (list
     (make-field-schema-instance
      'message
      #:required? #t
      #:type 'string
      #:validators (list
                    (lambda (f v) (validate-min-length f v 1))
                    (lambda (f v) (validate-max-length f v 100000))))
     (make-field-schema-instance
      'stream
      #:required? #f
      #:type 'boolean)))
  (validate-schema data schema))

(define (validate-search-messages-request data)
  "Validate search messages request"
  (define schema
    (list
     (make-field-schema-instance
      'query
      #:required? #t
      #:type 'string
      #:validators (list
                    (lambda (f v) (validate-min-length f v 1))))
     (make-field-schema-instance
      'limit
      #:required? #f
      #:type 'number
      #:validators (list
                    (lambda (f v) (validate-min-value f v 1))
                    (lambda (f v) (validate-max-value f v 100))))
     (make-field-schema-instance
      'search_type
      #:required? #f
      #:type 'string
      #:validators (list
                    (lambda (f v) (validate-enum f v '("text" "semantic")))))))
  (validate-schema data schema))

;;; ============================================================================
;;; Validation Middleware
;;; ============================================================================

(define (with-validation validator)
  "Create middleware that validates request body"
  (lambda (handler)
    (lambda (req)
      (if (not (http-request-json req))
          (make-error-response "JSON body required" #:status 400)
          (let ([errors (validator (http-request-json req))])
            (if (null? errors)
                (handler req)
                (make-json-response
                 (hash 'error "Validation failed"
                       'code "VALIDATION_ERROR"
                       'validation_errors
                       (map (lambda (err)
                              (hash 'field (validation-error-field err)
                                    'message (validation-error-message err)))
                            errors))
                 #:status 400)))))))
