#lang racket

;;; llm/types.rkt - Unified Type Definitions for LLM Clients
;;;
;;; This module defines common types and structures used across all LLM providers.

(provide (struct-out llm-message)
         (struct-out tool-call)
         (struct-out function-call)
         (struct-out tool-definition)
         (struct-out function-definition)
         (struct-out llm-response)
         (struct-out usage-stats)
         (struct-out llm-error)
         make-user-message
         make-assistant-message
         make-system-message
         make-tool-message
         make-tool-call-instance
         make-tool-definition-instance
         message->hash
         tool-call->hash
         tool-definition->hash
         hash->message
         hash->tool-call
         valid-message?
         valid-tool-definition?
         finish-reason->symbol
         extract-text-content
         has-tool-calls?)

(require racket/hash
        racket/match
        racket/format)

;;; ============================================================================
;;; Message Types
;;; ============================================================================

(struct llm-message
  (role      ; 'user, 'assistant, 'system, 'tool
   content   ; string or list of content blocks
   name      ; optional name (for tool messages)
   tool-calls ; optional list of tool-call structures
   tool-call-id) ; optional tool call ID (for tool response messages)
  #:transparent)

(define (make-user-message content)
  "Create a user message"
  (llm-message 'user content #f #f #f))

(define (make-assistant-message content [tool-calls #f])
  "Create an assistant message"
  (llm-message 'assistant content #f tool-calls #f))

(define (make-system-message content)
  "Create a system message"
  (llm-message 'system content #f #f #f))

(define (make-tool-message content tool-call-id)
  "Create a tool response message"
  (llm-message 'tool content #f #f tool-call-id))

;;; ============================================================================
;;; Tool Call Types
;;; ============================================================================

(struct tool-call
  (id        ; unique identifier
   type      ; "function" (standard)
   function)  ; function-call structure
  #:transparent)

(struct function-call
  (name      ; function name
   arguments) ; JSON string of arguments
  #:transparent)

(define (make-tool-call-instance id name arguments)
  "Create a tool call instance"
  (tool-call id "function"
             (function-call name arguments)))

;;; ============================================================================
;;; Tool Definition Types
;;; ============================================================================

(struct tool-definition
  (type      ; "function" (standard)
   function)  ; function-definition structure
  #:transparent)

(struct function-definition
  (name        ; function name
   description ; function description
   parameters) ; JSON schema for parameters
  #:transparent)

(define (make-tool-definition-instance name description parameters)
  "Create a tool definition"
  (tool-definition "function"
                  (function-definition name description parameters)))

;;; ============================================================================
;;; Response Types
;;; ============================================================================

(struct llm-response
  (id           ; response ID
   model        ; model used
   message      ; llm-message
   finish-reason ; 'stop, 'tool-calls, 'length, 'content-filter
   usage        ; usage-stats structure
   created      ; timestamp
   provider)    ; provider name ('openai, 'anthropic, 'groq, 'ollama)
  #:transparent)

(struct usage-stats
  (prompt-tokens      ; tokens in prompt
   completion-tokens  ; tokens in completion
   total-tokens)      ; total tokens
  #:transparent)

;;; ============================================================================
;;; Error Types
;;; ============================================================================

(struct llm-error
  (type      ; 'api-error, 'rate-limit, 'invalid-request, 'timeout
   message   ; error message
   status    ; HTTP status code (if applicable)
   provider  ; provider name
   details)  ; additional error details
  #:transparent)

;;; ============================================================================
;;; Conversion Helpers
;;; ============================================================================

(define (message->hash msg)
  "Convert llm-message to hash for API requests"
  (define base
    (hash 'role (symbol->string (llm-message-role msg))
          'content (llm-message-content msg)))

  ;; Add optional fields
  (define with-name
    (if (llm-message-name msg)
        (hash-set base 'name (llm-message-name msg))
        base))

  (define with-tool-calls
    (if (llm-message-tool-calls msg)
        (hash-set with-name 'tool_calls
                  (map tool-call->hash (llm-message-tool-calls msg)))
        with-name))

  (define with-tool-call-id
    (if (llm-message-tool-call-id msg)
        (hash-set with-tool-calls 'tool_call_id
                  (llm-message-tool-call-id msg))
        with-tool-calls))

  with-tool-call-id)

(define (tool-call->hash tc)
  "Convert tool-call to hash for API requests"
  (hash 'id (tool-call-id tc)
        'type (tool-call-type tc)
        'function (hash 'name (function-call-name (tool-call-function tc))
                        'arguments (function-call-arguments (tool-call-function tc)))))

(define (tool-definition->hash td)
  "Convert tool-definition to hash for API requests"
  (define func (tool-definition-function td))
  (hash 'type (tool-definition-type td)
        'function (hash 'name (function-definition-name func)
                        'description (function-definition-description func)
                        'parameters (function-definition-parameters func))))

(define (hash->message h)
  "Convert hash from API response to llm-message"
  (llm-message
   (string->symbol (hash-ref h 'role))
   (hash-ref h 'content "")
   (hash-ref h 'name #f)
   (let ([tcs (hash-ref h 'tool_calls #f)])
     (and tcs (map hash->tool-call tcs)))
   (hash-ref h 'tool_call_id #f)))

(define (hash->tool-call h)
  "Convert hash from API response to tool-call"
  (define func (hash-ref h 'function))
  (tool-call
   (hash-ref h 'id)
   (hash-ref h 'type)
   (function-call (hash-ref func 'name)
                   (hash-ref func 'arguments))))

;;; ============================================================================
;;; Validation Helpers
;;; ============================================================================

(define (valid-message? msg)
  "Check if message is valid"
  (and (llm-message? msg)
       (member (llm-message-role msg)
               '(user assistant system tool))
       (or (string? (llm-message-content msg))
           (list? (llm-message-content msg)))))

(define (valid-tool-definition? td)
  "Check if tool definition is valid"
  (and (tool-definition? td)
       (string? (function-definition-name (tool-definition-function td)))
       (string? (function-definition-description (tool-definition-function td)))
       (hash? (function-definition-parameters (tool-definition-function td)))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (finish-reason->symbol reason-str)
  "Convert finish reason string to symbol"
  (match reason-str
    ["stop" 'stop]
    ["tool_calls" 'tool-calls]
    ["length" 'length]
    ["content_filter" 'content-filter]
    [_ 'unknown]))

(define (extract-text-content msg)
  "Extract text content from message (handles both string and structured content)"
  (define content (llm-message-content msg))
  (cond
    [(string? content) content]
    [(list? content)
     ;; Extract text from content blocks
     (define texts
       (filter-map
        (lambda (block)
          (and (hash? block)
               (hash-ref block 'text #f)))
        content))
     (string-join texts "\n")]
    [else ""]))

(define (has-tool-calls? msg)
  "Check if message has tool calls"
  (and (llm-message? msg)
       (llm-message-tool-calls msg)
       (not (null? (llm-message-tool-calls msg)))))
