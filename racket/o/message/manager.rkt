#lang racket

;;; message/manager.rkt - Message Manager
;;;
;;; High-level message management system with advanced features.

(provide make-message-manager
         message-manager?
         manager-create-message!
         manager-create-user-message!
         manager-create-assistant-message!
         manager-create-system-message!
         manager-create-tool-message!
         manager-get-messages
         manager-get-conversation
         manager-get-recent
         manager-get-by-role
         manager-search
         manager-search-by-date
         manager-filter-messages
         manager-find-messages
         manager-get-stats
         manager-count-messages
         manager-count-tokens
         manager-calculate-stats
         manager-get-message-distribution
         manager-cache-message!
         manager-evict-cache!
         manager-get-cached
         manager-clear-cache!
         manager-clear-conversation!
         manager-export-conversation
         manager-import-conversation!
         manager-get-conversation-summary
         manager-get-last-user-message
         manager-get-last-assistant-message
         manager-get-context-window
         manager-truncate-to-limit
         manager-get-messages-since
         manager-get-messages-before
         manager-get-messages-between
         manager-create-messages-batch!
         manager-delete-messages-batch!
         manager-search-with-context
         manager-find-tool-calls
         manager-find-tool-responses
         manager-format-message
         manager-format-conversation
         manager-validate-message
         manager-validate-conversation
         page-result?
         page-result-messages
         page-result-total
         page-result-page
         page-result-page-size
         page-result-has-next?
         page-result-has-prev?
         manager-get-page
         manager-get-next-page
         manager-get-prev-page)

(require racket/hash
        racket/format
        racket/list
        racket/string
        json
        "../database/client.rkt"
        "./types.rkt")

;;; ============================================================================
;;; Message Manager State
;;; ============================================================================

(struct message-manager
  (agent-id         ; Agent ID this manager is for
   cache            ; Message cache
   cache-size)      ; Maximum cache size
  #:transparent
  #:mutable)

(define (make-message-manager agent-id #:cache-size [cache-size 100])
  "Create a new message manager for an agent"
  (message-manager agent-id (hash) cache-size))

;;; ============================================================================
;;; Message Creation
;;; ============================================================================

(define (manager-create-message! manager role content
                                  #:tool-calls [tool-calls #f]
                                  #:tool-call-id [tool-call-id #f]
                                  #:tool-name [tool-name #f]
                                  #:prompt-tokens [prompt-tokens 0]
                                  #:completion-tokens [completion-tokens 0]
                                  #:validate? [validate? #t])
  "Create a new message with validation"

  ;; Validate message parameters if requested
  (when validate?
    (define params (hash 'role role 'content content))
    (when tool-call-id
      (set! params (hash-set params 'tool_call_id tool-call-id)))
    (when tool-name
      (set! params (hash-set params 'tool_name tool-name)))

    (define result (validate-message-params params))
    (unless (and (pair? result) (car result))
      (error 'manager-create-message! "Invalid message parameters" (cdr result))))

  ;; Create message via database
  (define msg (message-create!
              (message-manager-agent-id manager)
              role
              content
              #:tool-calls tool-calls
              #:tool-call-id tool-call-id
              #:tool-name tool-name
              #:prompt-tokens prompt-tokens
              #:completion-tokens completion-tokens))

  ;; Add to cache
  (manager-cache-message! manager msg)

  msg)

(define (manager-create-user-message! manager content #:tokens [tokens 0])
  "Create user message"
  (manager-create-message! manager 'user content
                           #:prompt-tokens tokens))

(define (manager-create-assistant-message! manager content
                                            #:tool-calls [tool-calls #f]
                                            #:prompt-tokens [prompt-tokens 0]
                                            #:completion-tokens [completion-tokens 0])
  "Create assistant message"
  (manager-create-message! manager 'assistant content
                           #:tool-calls tool-calls
                           #:prompt-tokens prompt-tokens
                           #:completion-tokens completion-tokens))

(define (manager-create-system-message! manager content)
  "Create system message"
  (manager-create-message! manager 'system content))

(define (manager-create-tool-message! manager content tool-call-id tool-name)
  "Create tool response message"
  (manager-create-message! manager 'tool content
                           #:tool-call-id tool-call-id
                           #:tool-name tool-name))

;;; ============================================================================
;;; Message Retrieval
;;; ============================================================================

(define (manager-get-messages manager #:limit [limit 50] #:offset [offset 0] #:role [role #f])
  "Get messages with optional filtering"
  (message-get-list (message-manager-agent-id manager)
                    #:limit limit
                    #:offset offset
                    #:role role))

(define (manager-get-conversation manager #:limit [limit 100])
  "Get full conversation history"
  (conversation-get (message-manager-agent-id manager)
                    #:limit limit))

(define (manager-get-recent manager n)
  "Get N most recent messages"
  (conversation-get-recent (message-manager-agent-id manager) n))

(define (manager-get-by-role manager role #:limit [limit 50])
  "Get messages filtered by role"
  (conversation-get-by-role (message-manager-agent-id manager)
                            role
                            #:limit limit))

;;; ============================================================================
;;; Message Search and Filtering
;;; ============================================================================

(define (manager-search manager query #:limit [limit 10])
  "Search messages by content"
  (message-search (message-manager-agent-id manager)
                  query
                  #:limit limit))

(define (manager-search-by-date manager start-date end-date)
  "Search messages by date range"
  (message-search-by-date (message-manager-agent-id manager)
                          start-date
                          end-date))

(define (manager-filter-messages manager messages filter)
  "Filter messages using message-filter"
  (define filtered
    (filter (lambda (msg)
              (filter-matches? msg filter))
            messages))

  ;; Apply limit and offset
  (define offset (message-filter-offset filter))
  (define limit-val (message-filter-limit filter))
  (take (drop filtered offset) limit-val))

(define (manager-find-messages manager filter)
  "Find messages matching filter criteria"
  (define messages (manager-get-messages manager #:limit 1000))
  (manager-filter-messages manager messages filter))

;;; ============================================================================
;;; Message Pagination
;;; ============================================================================

(struct page-result
  (messages      ; List of messages in this page
   total         ; Total number of messages
   page          ; Current page number (0-indexed)
   page-size     ; Number of messages per page
   has-next?     ; Whether there's a next page
   has-prev?)    ; Whether there's a previous page
  #:transparent)

(define (manager-get-page manager page-number page-size
                         #:role [role #f] #:filter [filter #f])
  "Get paginated messages"
  (define offset (* page-number page-size))
  (define messages
    (if filter
        (manager-find-messages manager filter)
        (manager-get-messages manager
                             #:limit (+ page-size 1)
                             #:offset offset
                             #:role role)))
  (define has-next? (> (length messages) page-size))
  (define page-messages (take messages (min page-size (length messages))))
  (define total (conversation-count-messages (message-manager-agent-id manager)))

  (page-result page-messages
               total
               page-number
               page-size
               has-next?
               (> page-number 0)))

(define (manager-get-next-page page-result)
  "Get next page from page result"
  (if (page-result-has-next? page-result)
      (+ (page-result-page page-result) 1)
      #f))

(define (manager-get-prev-page page-result)
  "Get previous page from page result"
  (if (page-result-has-prev? page-result)
      (- (page-result-page page-result) 1)
      #f))

;;; ============================================================================
;;; Message Statistics
;;; ============================================================================

(define (manager-get-stats manager)
  "Get conversation statistics"
  (conversation-get-stats (message-manager-agent-id manager)))

(define (manager-count-messages manager)
  "Count total messages"
  (conversation-count-messages (message-manager-agent-id manager)))

(define (manager-count-tokens manager)
  "Count total tokens used"
  (conversation-count-tokens (message-manager-agent-id manager)))

(define (manager-calculate-stats manager messages)
  "Calculate statistics from message list"
  (calculate-stats messages))

(define (manager-get-message-distribution manager)
  "Get distribution of messages by role"
  (define stats (manager-get-stats manager))
  (hash
   'user (hash-ref stats 'user_messages)
   'assistant (hash-ref stats 'assistant_messages)
   'system 0
   'tool 0))

;;; ============================================================================
;;; Message Caching
;;; ============================================================================

(define (manager-cache-message! manager msg)
  "Add message to cache"
  (define cache (message-manager-cache manager))
  (define msg-id (message-id msg))
  (set-message-manager-cache! manager (hash-set cache msg-id msg))

  ;; Evict old messages if cache is full
  (when (> (hash-count (message-manager-cache manager))
           (message-manager-cache-size manager))
    (manager-evict-cache! manager)))

(define (manager-evict-cache! manager)
  "Evict oldest messages from cache"
  (define cache (message-manager-cache manager))
  (define messages (hash-values cache))
  (define sorted
    (sort messages <
          #:key (lambda (msg)
                  (message-timestamp msg))))
  (define to-remove (take sorted (quotient (hash-count cache) 2)))

  (define new-cache
    (for/fold ([acc cache])
              ([msg (in-list to-remove)])
      (hash-remove acc (message-id msg))))

  (set-message-manager-cache! manager new-cache))

(define (manager-get-cached manager msg-id)
  "Get message from cache"
  (hash-ref (message-manager-cache manager) msg-id #f))

(define (manager-clear-cache! manager)
  "Clear message cache"
  (set-message-manager-cache! manager (hash)))

;;; ============================================================================
;;; Conversation Management
;;; ============================================================================

(define (manager-clear-conversation! manager)
  "Clear all messages for agent"
  (conversation-clear! (message-manager-agent-id manager))
  (manager-clear-cache! manager))

(define (manager-export-conversation manager #:format [format 'json])
  "Export conversation to specified format"
  (conversation-export (message-manager-agent-id manager)
                       #:format format))

(define (manager-import-conversation! manager conversation-data)
  "Import conversation from exported data"
  (conversation-import! (message-manager-agent-id manager)
                        conversation-data)
  (manager-clear-cache! manager))

(define (manager-get-conversation-summary manager)
  "Get summary of conversation"
  (define stats (manager-get-stats manager))
  (hash
   'agent_id (message-manager-agent-id manager)
   'total_messages (hash-ref stats 'total_messages)
   'total_tokens (hash-ref stats 'total_tokens)
   'first_message_at (hash-ref stats 'first_message_at)
   'last_message_at (hash-ref stats 'last_message_at)
   'message_distribution (manager-get-message-distribution manager)))

;;; ============================================================================
;;; Message Utilities
;;; ============================================================================

(define (manager-get-last-user-message manager)
  "Get the last user message"
  (define messages (manager-get-by-role manager 'user #:limit 1))
  (if (null? messages)
      #f
      (car messages)))

(define (manager-get-last-assistant-message manager)
  "Get the last assistant message"
  (define messages (manager-get-by-role manager 'assistant #:limit 1))
  (if (null? messages)
      #f
      (car messages)))

(define (manager-get-context-window manager max-tokens)
  "Get messages that fit within token limit"
  (define messages (manager-get-conversation manager))
  (define reversed (reverse messages))
  (define (loop msgs result total-tokens)
    (cond
     [(or (null? msgs) (>= total-tokens max-tokens))
      (reverse result)]
     [else
      (define msg (car msgs))
      (define msg-tokens (message-tokens msg))
      (if (<= (+ total-tokens msg-tokens) max-tokens)
          (loop (cdr msgs) (cons msg result) (+ total-tokens msg-tokens))
          (reverse result))]))
  (loop reversed '() 0))

(define (manager-truncate-to-limit manager max-messages)
  "Get only the most recent N messages"
  (manager-get-recent manager max-messages))

(define (manager-get-messages-since manager timestamp)
  "Get all messages since a given timestamp"
  (define messages (manager-get-conversation manager))
  (filter (lambda (msg)
            (>= (message-timestamp msg) timestamp))
          messages))

(define (manager-get-messages-before manager timestamp)
  "Get all messages before a given timestamp"
  (define messages (manager-get-conversation manager))
  (filter (lambda (msg)
            (< (message-timestamp msg) timestamp))
          messages))

(define (manager-get-messages-between manager start-timestamp end-timestamp)
  "Get messages between two timestamps"
  (define messages (manager-get-conversation manager))
  (filter (lambda (msg)
            (define created (message-timestamp msg))
            (and (>= created start-timestamp)
                 (<= created end-timestamp)))
          messages))

;;; ============================================================================
;;; Batch Operations
;;; ============================================================================

(define (manager-create-messages-batch! manager message-list)
  "Create multiple messages in batch"
  (map (lambda (msg-params)
         (manager-create-message! manager
                                  (hash-ref msg-params 'role)
                                  (hash-ref msg-params 'content)
                                  #:tool-calls (hash-ref msg-params 'tool_calls #f)
                                  #:tool-call-id (hash-ref msg-params 'tool_call_id #f)
                                  #:tool-name (hash-ref msg-params 'tool_name #f)
                                  #:prompt-tokens (hash-ref msg-params 'prompt_tokens 0)
                                  #:completion-tokens (hash-ref msg-params 'completion_tokens 0)
                                  #:validate? #f))
       message-list))

(define (manager-delete-messages-batch! manager message-ids)
  "Delete multiple messages in batch"
  (for-each
   (lambda (msg-id)
     (message-delete! msg-id)
     (define cache (message-manager-cache manager))
     (set-message-manager-cache! manager (hash-remove cache msg-id)))
   message-ids))

;;; ============================================================================
;;; Advanced Search
;;; ============================================================================

(define (manager-search-with-context manager query #:context-size [context-size 2])
  "Search messages and include surrounding context"
  (define matches (manager-search manager query #:limit 100))
  (define all-messages (manager-get-conversation manager))
  (define results '())

  (define (add-context match)
    (define match-id (message-id match))
    (define match-idx
      (for/first ([i (in-naturals)]
                  [msg (in-list all-messages)]
                  #:when (equal? (message-id msg) match-id))
        i))

    (when match-idx
      (define start-idx (max 0 (- match-idx context-size)))
      (define end-idx (min (length all-messages)
                           (+ match-idx context-size 1)))
      (define context
        (take (drop all-messages start-idx)
              (- end-idx start-idx)))
      (set! results (cons (hash 'match match
                                'context context
                                'match_index match-idx)
                          results))))

  (for-each add-context matches)
  (reverse results))

(define (manager-find-tool-calls manager #:tool-name [tool-name #f] #:limit [limit 50])
  "Find messages with tool calls"
  (define messages (manager-get-by-role manager 'assistant #:limit limit))
  (filter (lambda (msg)
            (and (message-has-tool-calls? msg)
                 (or (not tool-name)
                     (let ([tool-calls (message-tool-calls msg)])
                       (for/or ([tc (in-list tool-calls)])
                         (equal? (hash-ref tc 'name) tool-name))))))
          messages))

(define (manager-find-tool-responses manager tool-call-id)
  "Find tool response messages for a given tool call"
  (define messages (manager-get-by-role manager 'tool #:limit 100))
  (filter (lambda (msg)
            (equal? (message-tool-call-id msg) tool-call-id))
          messages))

;;; ============================================================================
;;; Message Formatting
;;; ============================================================================

(define (manager-format-message msg #:format [format 'text])
  "Format message for display"
  (case format
    [(text)
     (format "~a: ~a"
             (message-role msg)
             (message-content msg))]
    [(json)
     (jsexpr->string (message->hash msg))]
    [(markdown)
     (format "**~a**: ~a"
             (message-role msg)
             (message-content msg))]
    [else
     (error 'manager-format-message "Unsupported format" format)]))

(define (manager-format-conversation manager #:format [format 'text] #:limit [limit 100])
  "Format entire conversation"
  (define messages (manager-get-conversation manager #:limit limit))
  (case format
    [(text)
     (string-join
      (map (lambda (msg)
             (manager-format-message msg #:format 'text))
           messages)
      "\n")]
    [(json)
     (jsexpr->string
      (hash 'agent_id (message-manager-agent-id manager)
            'messages (map message->hash messages)))]
    [(markdown)
     (string-join
      (map (lambda (msg)
             (manager-format-message msg #:format 'markdown))
           messages)
      "\n\n")]
    [else
     (error 'manager-format-conversation "Unsupported format" format)]))

;;; ============================================================================
;;; Message Validation
;;; ============================================================================

(define (manager-validate-message manager msg-params)
  "Validate message parameters"
  (validate-message-params msg-params))

(define (manager-validate-conversation manager)
  "Validate entire conversation for consistency"
  (define messages (manager-get-conversation manager))
  (define errors '())

  ;; Check for orphaned tool responses
  (define tool-messages (filter message-is-tool? messages))
  (define assistant-messages (filter message-is-assistant? messages))

  (for-each
   (lambda (tool-msg)
     (define tool-call-id (message-tool-call-id tool-msg))
     (define has-matching-call
       (for/or ([asst-msg (in-list assistant-messages)])
         (define tool-calls (message-tool-calls asst-msg))
         (and tool-calls
              (for/or ([tc (in-list tool-calls)])
                (equal? (hash-ref tc 'id) tool-call-id)))))
     (unless has-matching-call
       (set! errors (cons (format "Orphaned tool response: ~a" tool-call-id)
                         errors))))
   tool-messages)

  ;; Return validation result
  (if (null? errors)
      (cons #t #f)
      (cons #f errors)))

;;; ============================================================================
;;; Database Helper Functions (Placeholder implementations)
;;; ============================================================================

;; These would typically call into database/messages.ss
;; For now, providing placeholder implementations

(define (message-create! agent-id role content
                         #:tool-calls [tool-calls #f]
                         #:tool-call-id [tool-call-id #f]
                         #:tool-name [tool-name #f]
                         #:prompt-tokens [prompt-tokens 0]
                         #:completion-tokens [completion-tokens 0])
  "Create message via database (placeholder)"
  (define msg-id (format "~a-~a" (current-seconds) (random 1000000)))
  (define total-tokens (+ prompt-tokens completion-tokens))
  (define msg
    (case role
      [(user) (make-user-message agent-id content)]
      [(assistant) (make-assistant-message agent-id content #:tool-calls tool-calls)]
      [(system) (make-system-message agent-id content)]
      [(tool) (make-tool-message agent-id content tool-call-id)]
      [else (make-user-message agent-id content)]))
  msg)

(define (message-get-list agent-id #:limit [limit 50] #:offset [offset 0] #:role [role #f])
  "Get message list from database (placeholder)"
  '())

(define (conversation-get agent-id #:limit [limit 100])
  "Get conversation from database (placeholder)"
  '())

(define (conversation-get-recent agent-id n)
  "Get recent messages (placeholder)"
  '())

(define (conversation-get-by-role agent-id role #:limit [limit 100])
  "Get messages by role (placeholder)"
  '())

(define (message-search agent-id query #:limit [limit 10])
  "Search messages (placeholder)"
  '())

(define (message-search-by-date agent-id start-date end-date)
  "Search by date (placeholder)"
  '())

(define (conversation-count-messages agent-id)
  "Count messages (placeholder)"
  0)

(define (conversation-count-tokens agent-id)
  "Count tokens (placeholder)"
  0)

(define (conversation-get-stats agent-id)
  "Get stats (placeholder)"
  (hash 'total_messages 0
        'user_messages 0
        'assistant_messages 0
        'system_messages 0
        'tool_messages 0
        'total_tokens 0
        'first_message_at 0
        'last_message_at 0))

(define (conversation-clear! agent-id)
  "Clear conversation (placeholder)"
  (void))

(define (conversation-export agent-id #:format [format 'json])
  "Export conversation (placeholder)"
  "")

(define (conversation-import! agent-id data)
  "Import conversation (placeholder)"
  (void))

(define (message-delete! msg-id)
  "Delete message (placeholder)"
  (void))

(define (validate-message-params params)
  "Validate message parameters (placeholder)"
  (cons #t #f))

(define (message-filter-offset filter) 0)
(define (message-filter-limit filter) 50)
(define (filter-matches? msg filter) #t)
(define (calculate-stats messages)
  (message-stats 0 0 0 0 0 0 0))
(define (message-has-tool-calls? msg)
  (and (message-tool-calls msg) (not (null? (message-tool-calls msg)))))
(define (message-is-tool? msg)
  (eq? (message-role msg) 'tool))
(define (message-is-assistant? msg)
  (eq? (message-role msg) 'assistant))
