;;; llm/types.ss - Unified Type Definitions for LLM Clients
;;;
;;; This module defines common types and structures used across all LLM providers.

(export #t)

(import
  :std/sugar
  :std/misc/hash)

;;; ============================================================================
;;; Message Types
;;; ============================================================================

(defstruct llm-message
  (role      ; :user :assistant :system :tool
   content   ; string or list of content blocks
   name      ; optional name (for tool messages)
   tool-calls ; optional list of tool-call structures
   tool-call-id) ; optional tool call ID (for tool response messages)
  transparent: #t)

(def (make-user-message content)
  "Create a user message"
  (make-llm-message
   role: :user
   content: content
   name: #f
   tool-calls: #f
   tool-call-id: #f))

(def (make-assistant-message content #!optional tool-calls)
  "Create an assistant message"
  (make-llm-message
   role: :assistant
   content: content
   name: #f
   tool-calls: tool-calls
   tool-call-id: #f))

(def (make-system-message content)
  "Create a system message"
  (make-llm-message
   role: :system
   content: content
   name: #f
   tool-calls: #f
   tool-call-id: #f))

(def (make-tool-message content tool-call-id)
  "Create a tool response message"
  (make-llm-message
   role: :tool
   content: content
   name: #f
   tool-calls: #f
   tool-call-id: tool-call-id))

;;; ============================================================================
;;; Tool Call Types
;;; ============================================================================

(defstruct tool-call
  (id        ; unique identifier
   type      ; "function" (standard)
   function)  ; function-call structure
  transparent: #t)

(defstruct function-call
  (name      ; function name
   arguments) ; JSON string of arguments
  transparent: #t)

(def (make-tool-call-instance id name arguments)
  "Create a tool call instance"
  (make-tool-call
   id: id
   type: "function"
   function: (make-function-call
              name: name
              arguments: arguments)))

;;; ============================================================================
;;; Tool Definition Types
;;; ============================================================================

(defstruct tool-definition
  (type      ; "function" (standard)
   function)  ; function-definition structure
  transparent: #t)

(defstruct function-definition
  (name        ; function name
   description ; function description
   parameters) ; JSON schema for parameters
  transparent: #t)

(def (make-tool-definition-instance name description parameters)
  "Create a tool definition"
  (make-tool-definition
   type: "function"
   function: (make-function-definition
              name: name
              description: description
              parameters: parameters)))

;;; ============================================================================
;;; Response Types
;;; ============================================================================

(defstruct llm-response
  (id           ; response ID
   model        ; model used
   message      ; llm-message
   finish-reason ; :stop :tool-calls :length :content-filter
   usage        ; usage-stats structure
   created      ; timestamp
   provider)    ; provider name (:openai :anthropic :groq :ollama)
  transparent: #t)

(defstruct usage-stats
  (prompt-tokens      ; tokens in prompt
   completion-tokens  ; tokens in completion
   total-tokens)      ; total tokens
  transparent: #t)

;;; ============================================================================
;;; Error Types
;;; ============================================================================

(defstruct llm-error
  (type      ; :api-error :rate-limit :invalid-request :timeout
   message   ; error message
   status    ; HTTP status code (if applicable)
   provider  ; provider name
   details)  ; additional error details
  transparent: #t)

;;; ============================================================================
;;; Conversion Helpers
;;; ============================================================================

(def (message->hash msg)
  "Convert llm-message to hash for API requests"
  (let ((base (hash
               'role (symbol->string (llm-message-role msg))
               'content (llm-message-content msg))))

    ;; Add optional fields
    (when (llm-message-name msg)
      (hash-put! base 'name (llm-message-name msg)))

    (when (llm-message-tool-calls msg)
      (hash-put! base 'tool_calls
                 (map tool-call->hash (llm-message-tool-calls msg))))

    (when (llm-message-tool-call-id msg)
      (hash-put! base 'tool_call_id (llm-message-tool-call-id msg)))

    base))

(def (tool-call->hash tc)
  "Convert tool-call to hash for API requests"
  (hash
   'id (tool-call-id tc)
   'type (tool-call-type tc)
   'function (hash
              'name (function-call-name (tool-call-function tc))
              'arguments (function-call-arguments (tool-call-function tc)))))

(def (tool-definition->hash td)
  "Convert tool-definition to hash for API requests"
  (hash
   'type (tool-definition-type td)
   'function (let ((func (tool-definition-function td)))
               (hash
                'name (function-definition-name func)
                'description (function-definition-description func)
                'parameters (function-definition-parameters func)))))

(def (hash->message h)
  "Convert hash from API response to llm-message"
  (make-llm-message
   role: (string->symbol (hash-ref h 'role))
   content: (hash-ref h 'content "")
   name: (hash-ref h 'name #f)
   tool-calls: (let ((tcs (hash-ref h 'tool_calls #f)))
                 (if tcs
                     (map hash->tool-call tcs)
                     #f))
   tool-call-id: (hash-ref h 'tool_call_id #f)))

(def (hash->tool-call h)
  "Convert hash from API response to tool-call"
  (make-tool-call
   id: (hash-ref h 'id)
   type: (hash-ref h 'type)
   function: (let ((func (hash-ref h 'function)))
               (make-function-call
                name: (hash-ref func 'name)
                arguments: (hash-ref func 'arguments)))))

;;; ============================================================================
;;; Validation Helpers
;;; ============================================================================

(def (valid-message? msg)
  "Check if message is valid"
  (and (llm-message? msg)
       (member (llm-message-role msg) '(:user :assistant :system :tool))
       (or (string? (llm-message-content msg))
           (list? (llm-message-content msg)))))

(def (valid-tool-definition? td)
  "Check if tool definition is valid"
  (and (tool-definition? td)
       (string? (function-definition-name (tool-definition-function td)))
       (string? (function-definition-description (tool-definition-function td)))
       (hash? (function-definition-parameters (tool-definition-function td)))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (finish-reason->symbol reason-str)
  "Convert finish reason string to symbol"
  (cond
   ((equal? reason-str "stop") :stop)
   ((equal? reason-str "tool_calls") :tool-calls)
   ((equal? reason-str "length") :length)
   ((equal? reason-str "content_filter") :content-filter)
   (else :unknown)))

(def (extract-text-content msg)
  "Extract text content from message (handles both string and structured content)"
  (let ((content (llm-message-content msg)))
    (cond
     ((string? content) content)
     ((list? content)
      ;; Extract text from content blocks
      (string-join
       (filter-map
        (lambda (block)
          (if (hash? block)
              (hash-ref block 'text #f)
              #f))
        content)
       "\n"))
     (else ""))))

(def (has-tool-calls? msg)
  "Check if message has tool calls"
  (and (llm-message? msg)
       (llm-message-tool-calls msg)
       (not (null? (llm-message-tool-calls msg)))))
