;;; message/types.ss - Message Type Definitions
;;;
;;; Type definitions for message management system.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format)

;;; ============================================================================
;;; Message Types
;;; ============================================================================

(defstruct message
  (id           ; UUID
   agent-id     ; UUID
   role         ; "user" | "assistant" | "system" | "tool"
   content      ; Message content
   tool-calls   ; List of tool calls (for assistant messages)
   tool-call-id ; Tool call ID (for tool messages)
   tool-name    ; Tool name (for tool messages)
   prompt-tokens      ; Tokens in prompt
   completion-tokens  ; Tokens in completion
   total-tokens       ; Total tokens
   created-at)        ; Timestamp
  transparent: #t)

(defstruct tool-call
  (id       ; Tool call ID
   type     ; "function"
   function ; Function call details
   name     ; Function name
   arguments) ; Function arguments (JSON string)
  transparent: #t)

;;; ============================================================================
;;; Message Validation
;;; ============================================================================

(def valid-roles '("user" "assistant" "system" "tool"))

(def (valid-role? role)
  "Check if role is valid"
  (member role valid-roles))

(def (validate-message-params params)
  "Validate message parameters
   Returns: (cons #t #f) if valid, (cons #f errors) if invalid"
  (let ((errors '()))

    ;; Validate role
    (unless (hash-key? params 'role)
      (set! errors (cons "Role is required" errors)))

    (when (hash-key? params 'role)
      (let ((role (hash-ref params 'role)))
        (unless (valid-role? role)
          (set! errors (cons (format "Invalid role: ~a" role) errors)))))

    ;; Validate content
    (unless (hash-key? params 'content)
      (set! errors (cons "Content is required" errors)))

    (when (hash-key? params 'content)
      (let ((content (hash-ref params 'content)))
        (unless (string? content)
          (set! errors (cons "Content must be a string" errors)))
        (when (and (string? content) (= (string-length content) 0))
          (set! errors (cons "Content cannot be empty" errors)))))

    ;; Validate tool message requirements
    (when (and (hash-key? params 'role)
               (equal? (hash-ref params 'role) "tool"))
      (unless (hash-key? params 'tool_call_id)
        (set! errors (cons "Tool call ID required for tool messages" errors)))
      (unless (hash-key? params 'tool_name)
        (set! errors (cons "Tool name required for tool messages" errors))))

    ;; Return validation result
    (if (null? errors)
        (cons #t #f)
        (cons #f errors))))

;;; ============================================================================
;;; Message Filtering
;;; ============================================================================

(defstruct message-filter
  (role         ; Filter by role
   start-date   ; Filter by start date
   end-date     ; Filter by end date
   search-text  ; Filter by text content
   has-tool-calls ; Filter messages with tool calls
   limit        ; Limit number of results
   offset)      ; Offset for pagination
  transparent: #t)

(def (make-default-filter)
  "Create default message filter"
  (make-message-filter
   role: #f
   start-date: #f
   end-date: #f
   search-text: #f
   has-tool-calls: #f
   limit: 50
   offset: 0))

(def (filter-matches? message filter)
  "Check if message matches filter"
  (and
   ;; Role filter
   (or (not (message-filter-role filter))
       (equal? (message-role message) (message-filter-role filter)))

   ;; Date range filter
   (or (not (message-filter-start-date filter))
       (>= (message-created-at message) (message-filter-start-date filter)))

   (or (not (message-filter-end-date filter))
       (<= (message-created-at message) (message-filter-end-date filter)))

   ;; Text search filter
   (or (not (message-filter-search-text filter))
       (string-contains (message-content message) (message-filter-search-text filter)))

   ;; Tool calls filter
   (or (not (message-filter-has-tool-calls filter))
       (and (message-tool-calls message) #t))))

;;; ============================================================================
;;; Message Statistics
;;; ============================================================================

(defstruct message-stats
  (total-messages      ; Total number of messages
   user-messages       ; Number of user messages
   assistant-messages  ; Number of assistant messages
   system-messages     ; Number of system messages
   tool-messages       ; Number of tool messages
   total-tokens        ; Total tokens used
   prompt-tokens       ; Total prompt tokens
   completion-tokens   ; Total completion tokens
   first-message-at    ; Timestamp of first message
   last-message-at)    ; Timestamp of last message
  transparent: #t)

(def (make-empty-stats)
  "Create empty message statistics"
  (make-message-stats
   total-messages: 0
   user-messages: 0
   assistant-messages: 0
   system-messages: 0
   tool-messages: 0
   total-tokens: 0
   prompt-tokens: 0
   completion-tokens: 0
   first-message-at: #f
   last-message-at: #f))

(def (calculate-stats messages)
  "Calculate statistics from message list"
  (if (null? messages)
      (make-empty-stats)
      (let ((total 0)
            (user 0)
            (assistant 0)
            (system 0)
            (tool 0)
            (total-tokens 0)
            (prompt-tokens 0)
            (completion-tokens 0)
            (first-at #f)
            (last-at #f))

        (for-each
         (lambda (msg)
           (set! total (+ total 1))

           ;; Count by role
           (case (hash-ref msg 'role)
             (("user") (set! user (+ user 1)))
             (("assistant") (set! assistant (+ assistant 1)))
             (("system") (set! system (+ system 1)))
             (("tool") (set! tool (+ tool 1))))

           ;; Sum tokens
           (when (hash-key? msg 'total_tokens)
             (set! total-tokens (+ total-tokens (hash-ref msg 'total_tokens))))
           (when (hash-key? msg 'prompt_tokens)
             (set! prompt-tokens (+ prompt-tokens (hash-ref msg 'prompt_tokens))))
           (when (hash-key? msg 'completion_tokens)
             (set! completion-tokens (+ completion-tokens (hash-ref msg 'completion_tokens))))

           ;; Track timestamps
           (let ((created (hash-ref msg 'created_at)))
             (when (or (not first-at) (< created first-at))
               (set! first-at created))
             (when (or (not last-at) (> created last-at))
               (set! last-at created))))
         messages)

        (make-message-stats
         total-messages: total
         user-messages: user
         assistant-messages: assistant
         system-messages: system
         tool-messages: tool
         total-tokens: total-tokens
         prompt-tokens: prompt-tokens
         completion-tokens: completion-tokens
         first-message-at: first-at
         last-message-at: last-at))))

;;; ============================================================================
;;; Message Conversion
;;; ============================================================================

(def (hash->message h)
  "Convert hash to message struct"
  (make-message
   id: (hash-ref h 'id)
   agent-id: (hash-ref h 'agent_id)
   role: (hash-ref h 'role)
   content: (hash-ref h 'content)
   tool-calls: (hash-ref h 'tool_calls #f)
   tool-call-id: (hash-ref h 'tool_call_id #f)
   tool-name: (hash-ref h 'tool_name #f)
   prompt-tokens: (hash-ref h 'prompt_tokens 0)
   completion-tokens: (hash-ref h 'completion_tokens 0)
   total-tokens: (hash-ref h 'total_tokens 0)
   created-at: (hash-ref h 'created_at)))

(def (message->hash msg)
  "Convert message struct to hash"
  (hash
   'id (message-id msg)
   'agent_id (message-agent-id msg)
   'role (message-role msg)
   'content (message-content msg)
   'tool_calls (message-tool-calls msg)
   'tool_call_id (message-tool-call-id msg)
   'tool_name (message-tool-name msg)
   'prompt_tokens (message-prompt-tokens msg)
   'completion_tokens (message-completion-tokens msg)
   'total_tokens (message-total-tokens msg)
   'created_at (message-created-at msg)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (message-is-user? msg)
  "Check if message is from user"
  (equal? (if (message? msg) (message-role msg) (hash-ref msg 'role)) "user"))

(def (message-is-assistant? msg)
  "Check if message is from assistant"
  (equal? (if (message? msg) (message-role msg) (hash-ref msg 'role)) "assistant"))

(def (message-is-system? msg)
  "Check if message is system message"
  (equal? (if (message? msg) (message-role msg) (hash-ref msg 'role)) "system"))

(def (message-is-tool? msg)
  "Check if message is tool response"
  (equal? (if (message? msg) (message-role msg) (hash-ref msg 'role)) "tool"))

(def (message-has-tool-calls? msg)
  "Check if message has tool calls"
  (let ((tool-calls (if (message? msg)
                        (message-tool-calls msg)
                        (hash-ref msg 'tool_calls #f))))
    (and tool-calls (not (null? tool-calls)))))

(def (message-token-count msg)
  "Get total token count for message"
  (if (message? msg)
      (message-total-tokens msg)
      (hash-ref msg 'total_tokens 0)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create message filter
(def filter (make-message-filter
             role: "user"
             start-date: 1705315200
             limit: 10
             offset: 0))

;; Validate message parameters
(def params (hash 'role "user" 'content "Hello!"))
(def result (validate-message-params params))
(if (car result)
    (displayln "Valid message")
    (displayln (format "Invalid: ~a" (cdr result))))

;; Calculate statistics
(def messages (list
               (hash 'role "user" 'content "Hello" 'total_tokens 5 'created_at 1705315200)
               (hash 'role "assistant" 'content "Hi!" 'total_tokens 3 'created_at 1705315210)))
(def stats (calculate-stats messages))
(displayln (format "Total messages: ~a" (message-stats-total-messages stats)))
|#
