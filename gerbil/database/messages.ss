;;; database/messages.ss - Message Database Operations
;;;
;;; High-level message database operations built on top of the database client.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
;;
  ;; ;; (
  ;; :o/database/client (placeholder)) (placeholder)
 (placeholder)
)

;;; ============================================================================
;;; Message Management
;;; ============================================================================

(def (message-create! agent-id role content . rest
                      (tool-calls #f)
                      (tool-call-id #f)
                      (tool-name #f)
                      (prompt-tokens 0)
                      (completion-tokens 0))
  "Create a new message"

  (let* ((total-tokens (+ prompt-tokens completion-tokens))
         (params (let ((ht (make-hash-table)))
  (hash-put! ht 'role role)
  (hash-put! ht 'content content)
  (hash-put! ht 'tool_calls tool-calls)
  (hash-put! ht 'tool_call_id tool-call-id)
  (hash-put! ht 'tool_name tool-name)
  (hash-put! ht 'prompt_tokens prompt-tokens)
  (hash-put! ht 'completion_tokens completion-tokens)
  (hash-put! ht 'total_tokens total-tokens)
  ht))
         (message (db-create-message agent-id params)))

    (displayln (format "Message created: ~a (~a)" role (hash-ref message 'id)))
    message))

(def (message-get-list agent-id . rest
                       (limit 50)
                       (offset 0)
                       (role #f))
  "Get messages for agent"
  (db-get-messages agent-id
                   limit: limit
                   offset: offset
                   role: role))

(def (message-delete! message-id)
  "Delete message"
  (db-delete-message message-id)
  (displayln (format "Message deleted: ~a" message-id)))

;;; ============================================================================
;;; Conversation Management
;;; ============================================================================

(def (conversation-get agent-id . rest (limit 100))
  "Get full conversation history"
  (db-get-conversation agent-id limit: limit))

(def (conversation-clear! agent-id)
  "Clear all messages for agent"
  (let ((messages (conversation-get agent-id limit: 10000)))
    (for-each
     (lambda (msg)
       (message-delete! (hash-ref msg 'id)))
     messages)
    (displayln (format "Conversation cleared: ~a messages deleted" (length messages)))))

(def (conversation-get-recent agent-id n)
  "Get N most recent messages"
  (message-get-list agent-id limit: n offset: 0))

(def (conversation-get-by-role agent-id role . rest (limit 50))
  "Get messages filtered by role"
  (message-get-list agent-id limit: limit role: role))

;;; ============================================================================
;;; Message Creation Helpers
;;; ============================================================================

(def (message-user! agent-id content . rest (tokens 0))
  "Create user message"
  (message-create! agent-id "user" content
                   prompt-tokens: tokens))

(def (message-assistant! agent-id content . rest
                         (tool-calls #f)
                         (prompt-tokens 0)
                         (completion-tokens 0))
  "Create assistant message"
  (message-create! agent-id "assistant" content
                   tool-calls: tool-calls
                   prompt-tokens: prompt-tokens
                   completion-tokens: completion-tokens))

(def (message-system! agent-id content)
  "Create system message"
  (message-create! agent-id "system" content))

(def (message-tool! agent-id content tool-call-id tool-name)
  "Create tool response message"
  (message-create! agent-id "tool" content
                   tool-call-id: tool-call-id
                   tool-name: tool-name))

;;; ============================================================================
;;; Conversation Analysis
;;; ============================================================================

(def (conversation-count-messages agent-id)
  "Count total messages"
  (let ((stats (db-get-agent-statistics agent-id)))
    (hash-ref stats 'total_messages)))

(def (conversation-count-tokens agent-id)
  "Count total tokens used"
  (let ((stats (db-get-agent-statistics agent-id)))
    (hash-ref stats 'total_tokens)))

(def (conversation-get-stats agent-id)
  "Get conversation statistics"
  (let ((stats (db-get-agent-statistics agent-id)))
    (let ((ht (make-hash-table)))
  (hash-put! ht 'total_messages (hash-ref stats 'total_messages))
  ht)))

;;; ============================================================================
;;; Message Search
;;; ============================================================================

(def (message-search agent-id query . rest (limit 10))
  "Search messages by content (text search)"
  ;; TODO: Implement text search in database
  ;; For now, filter in memory
  (let ((messages (message-get-list agent-id limit: 1000)))
    (take
     (filter (lambda (msg)
               (string-contains (hash-ref msg 'content) query))
             messages)
     limit)))

(def (message-search-by-date agent-id start-date end-date)
  "Search messages by date range"
  ;; TODO: Implement date range search in database
  (let ((messages (message-get-list agent-id limit: 10000)))
    (filter (lambda (msg)
              (let ((created-at (hash-ref msg 'created_at)))
                (and (>= created-at start-date)
                     (<= created-at end-date))))
            messages)))

;;; ============================================================================
;;; Conversation Export
;;; ============================================================================

(def (conversation-export agent-id . rest (format 'json))
  "Export conversation to specified format"
  (let ((messages (conversation-get agent-id)))
    (case format
      (('json) 
       (json-object->string
        (let ((ht (make-hash-table)))
  (hash-put! ht 'agent_id agent-id)
  (hash-put! ht 'messages messages)
  (hash-put! ht 'exported_at (current-seconds))
  ht)))

      (('text) 
       (string-join
        (map (lambda (msg)
               (format "[~a] ~a: ~a"
                      (hash-ref msg 'created_at)
                      (hash-ref msg 'role)
                      (hash-ref msg 'content)))
             messages)
        "\n"))

      (else
       (error "Unsupported export format" format)))))

;;; ============================================================================
;;; Batch Operations
;;; ============================================================================

(def (conversation-create-from-list! agent-id messages)
  "Create multiple messages from list"
  (for-each
   (lambda (msg)
     (message-create! agent-id
                      (hash-ref msg 'role)
                      (hash-ref msg 'content)
                      prompt-tokens: (hash-ref msg 'prompt_tokens 0)
                      completion-tokens: (hash-ref msg 'completion_tokens 0)))
   messages))

(def (conversation-import! agent-id conversation-data)
  "Import conversation from exported data"
  (let ((messages (hash-ref conversation-data 'messages)))
    (conversation-create-from-list! agent-id messages)))

;;; ============================================================================
;;; Message Validation
;;; ============================================================================

(def (message-validate params)
  "Validate message parameters"
  (let ((errors '()))

    ;; Validate role
    (unless (hash-key? params 'role)
      (set! errors (cons "Role is required" errors)))

    (when (hash-key? params 'role)
      (let ((role (hash-ref params 'role)))
        (unless (member role '("user" "assistant" "system" "tool"))
          (set! errors (cons "Invalid role" errors)))))

    ;; Validate content
    (unless (hash-key? params 'content)
      (set! errors (cons "Content is required" errors)))

    ;; Validate tool message requirements
    (when (and (hash-key? params 'role)
               (equal? (hash-ref params 'role) "tool"))
      (unless (hash-key? params 'tool_call_id)
        (set! errors (cons "Tool call ID required for tool messages" errors)))
      (unless (hash-key? params 'tool_name)
        (set! errors (cons "Tool name required for tool messages" errors))))

    ;; Return validation result
    (if (null? errors)
        #t
        (cons #f errors))))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Connect to database
(db-connect!)

;; Create messages
(message-user! agent-id "Hello!" tokens: 5)
(message-assistant! agent-id "Hi there! How can I help?"
                    prompt-tokens: 10
                    completion-tokens: 15)

;; Get conversation
(def messages (conversation-get agent-id))

;; Get recent messages
(def recent (conversation-get-recent agent-id 10))

;; Get user messages only
(def user-messages (conversation-get-by-role agent-id "user"))

;; Search messages
(def results (message-search agent-id "hello"))

;; Get conversation stats
(def stats (conversation-get-stats agent-id))

;; Export conversation
(def json-export (conversation-export agent-id format: 'json))
(def text-export (conversation-export agent-id format: 'text))

;; Clear conversation
(conversation-clear! agent-id)
|#
