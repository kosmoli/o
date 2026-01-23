#lang racket

;;; database/messages.rkt - Message Database Operations
;;;
;;; High-level message database operations built on top of the database client.

(provide message-create!
         message-get-list
         message-delete!
         conversation-get
         conversation-clear!
         conversation-get-recent
         conversation-get-by-role
         message-user!
         message-assistant!
         message-system!
         message-tool!
         conversation-count-messages
         conversation-count-tokens
         conversation-get-stats
         message-search
         message-search-by-date
         conversation-export
         conversation-create-from-list!
         conversation-import!
         message-validate)

(require racket/hash
        racket/format
        racket/list
        racket/string
        json
        "./client.rkt")

;;; ============================================================================
;;; Message Management
;;; ============================================================================

(define (message-create! agent-id role content
                       #:tool-calls [tool-calls #f]
                       #:tool-call-id [tool-call-id #f]
                       #:tool-name [tool-name #f]
                       #:prompt-tokens [prompt-tokens 0]
                       #:completion-tokens [completion-tokens 0])
  "Create a new message"
  (define total-tokens (+ prompt-tokens completion-tokens))
  (define params (hash 'role role
                       'content content
                       'tool_calls tool-calls
                       'tool_call_id tool-call-id
                       'tool_name tool-name
                       'prompt_tokens prompt-tokens
                       'completion_tokens completion-tokens
                       'total_tokens total-tokens))
  (define message (db-create-message agent-id params))

  (displayln (format "Message created: ~a (~a)" role (hash-ref message 'id)))
  message)

(define (message-get-list agent-id
                          #:limit [limit 50]
                          #:offset [offset 0]
                          #:role [role #f])
  "Get messages for agent"
  (db-get-messages agent-id
                   #:limit limit
                   #:offset offset
                   #:role role))

(define (message-delete! message-id)
  "Delete message"
  (db-delete-message message-id)
  (displayln (format "Message deleted: ~a" message-id)))

;;; ============================================================================
;;; Conversation Management
;;; ============================================================================

(define (conversation-get agent-id #:limit [limit 100])
  "Get full conversation history"
  (db-get-conversation agent-id #:limit limit))

(define (conversation-clear! agent-id)
  "Clear all messages for agent"
  (define messages (conversation-get agent-id #:limit 10000))
  (for-each
   (lambda (msg)
     (message-delete! (hash-ref msg 'id)))
   messages)
  (displayln (format "Conversation cleared: ~a messages deleted" (length messages))))

(define (conversation-get-recent agent-id n)
  "Get N most recent messages"
  (message-get-list agent-id #:limit n #:offset 0))

(define (conversation-get-by-role agent-id role #:limit [limit 50])
  "Get messages filtered by role"
  (message-get-list agent-id #:limit limit #:role role))

;;; ============================================================================
;;; Message Creation Helpers
;;; ============================================================================

(define (message-user! agent-id content #:tokens [tokens 0])
  "Create user message"
  (message-create! agent-id "user" content
                   #:prompt-tokens tokens))

(define (message-assistant! agent-id content
                            #:tool-calls [tool-calls #f]
                            #:prompt-tokens [prompt-tokens 0]
                            #:completion-tokens [completion-tokens 0])
  "Create assistant message"
  (message-create! agent-id "assistant" content
                   #:tool-calls tool-calls
                   #:prompt-tokens prompt-tokens
                   #:completion-tokens completion-tokens))

(define (message-system! agent-id content)
  "Create system message"
  (message-create! agent-id "system" content))

(define (message-tool! agent-id content tool-call-id tool-name)
  "Create tool response message"
  (message-create! agent-id "tool" content
                   #:tool-call-id tool-call-id
                   #:tool-name tool-name))

;;; ============================================================================
;;; Conversation Analysis
;;; ============================================================================

(define (conversation-count-messages agent-id)
  "Count total messages"
  (define stats (db-get-agent-statistics agent-id))
  (hash-ref stats 'total_messages 0))

(define (conversation-count-tokens agent-id)
  "Count total tokens used"
  (define stats (db-get-agent-statistics agent-id))
  (hash-ref stats 'total_tokens 0))

(define (conversation-get-stats agent-id)
  "Get conversation statistics"
  (define stats (db-get-agent-statistics agent-id))
  (hash 'total_messages (hash-ref stats 'total_messages 0)
        'user_messages (hash-ref stats 'user_messages 0)
        'assistant_messages (hash-ref stats 'assistant_messages 0)
        'total_tokens (hash-ref stats 'total_tokens 0)
        'prompt_tokens (hash-ref stats 'prompt_tokens 0)
        'completion_tokens (hash-ref stats 'completion_tokens 0)
        'first_message_at (hash-ref stats 'first_message_at 0)
        'last_message_at (hash-ref stats 'last_message_at 0)))

;;; ============================================================================
;;; Message Search
;;; ============================================================================

(define (message-search agent-id query #:limit [limit 10])
  "Search messages by content (text search)"
  (define messages (message-get-list agent-id #:limit 1000))
  (define filtered
    (filter (lambda (msg)
              (string-contains? (hash-ref msg 'content "") query))
            messages))
  (take filtered (min limit (length filtered))))

(define (message-search-by-date agent-id start-date end-date)
  "Search messages by date range"
  (define messages (message-get-list agent-id #:limit 10000))
  (filter (lambda (msg)
            (define created-at (hash-ref msg 'created_at 0))
            (and (>= created-at start-date)
                 (<= created-at end-date)))
          messages))

;;; ============================================================================
;;; Conversation Export
;;; ============================================================================

(define (conversation-export agent-id #:format [format 'json])
  "Export conversation to specified format"
  (define messages (conversation-get agent-id))
  (case format
    [(json)
     (jsexpr->string
      (hash 'agent_id agent-id
            'messages messages
            'exported_at (current-seconds)))]
    [(text)
     (string-join
      (map (lambda (msg)
             (format "[~a] ~a: ~a"
                    (hash-ref msg 'created_at 0)
                    (hash-ref msg 'role "")
                    (hash-ref msg 'content "")))
           messages)
      "\n")]
    [else
     (error 'conversation-export "Unsupported export format" format)]))

;;; ============================================================================
;;; Batch Operations
;;; ============================================================================

(define (conversation-create-from-list! agent-id messages)
  "Create multiple messages from list"
  (for-each
   (lambda (msg)
     (message-create! agent-id
                      (hash-ref msg 'role "")
                      (hash-ref msg 'content "")
                      #:prompt-tokens (hash-ref msg 'prompt_tokens 0)
                      #:completion-tokens (hash-ref msg 'completion_tokens 0)))
   messages))

(define (conversation-import! agent-id conversation-data)
  "Import conversation from exported data"
  (define messages (hash-ref conversation-data 'messages))
  (conversation-create-from-list! agent-id messages))

;;; ============================================================================
;;; Message Validation
;;; ============================================================================

(define (message-validate params)
  "Validate message parameters"
  (define errors '())

  (define errors1
    (if (hash-has-key? params 'role)
        errors
        (cons "Role is required" errors)))

  (define errors2
    (if (hash-has-key? params 'role)
        (let ([role (hash-ref params 'role)])
          (if (member role '("user" "assistant" "system" "tool"))
              errors1
              (cons "Invalid role" errors1)))
        errors1))

  (define errors3
    (if (hash-has-key? params 'content)
        errors2
        (cons "Content is required" errors2)))

  (define errors4
    (if (and (hash-has-key? params 'role)
             (equal? (hash-ref params 'role) "tool"))
        (append
         (if (hash-has-key? params 'tool_call_id)
             '()
             (list "Tool call ID required for tool messages"))
         (if (hash-has-key? params 'tool_name)
             '()
             (list "Tool name required for tool messages")))
        errors3))

  (if (null? errors4)
      #t
      (cons #f errors4)))
