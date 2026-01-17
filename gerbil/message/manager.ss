;;; message/manager.ss - Message Manager
;;;
;;; High-level message management system with advanced features.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/sort
  :std/text/json
  :o/database/client
  :o/database/messages
  :o/message/types)

;;; ============================================================================
;;; Message Manager State
;;; ============================================================================

(defstruct message-manager
  (agent-id      ; Agent ID this manager is for
   cache         ; Message cache
   cache-size)   ; Maximum cache size
  transparent: #t)

(def (make-manager agent-id #!key (cache-size 100))
  "Create a new message manager for an agent"
  (make-message-manager
   agent-id: agent-id
   cache: (hash)
   cache-size: cache-size))

;;; ============================================================================
;;; Message Creation
;;; ============================================================================

(def (manager-create-message! manager role content
                              #!key
                              (tool-calls #f)
                              (tool-call-id #f)
                              (tool-name #f)
                              (prompt-tokens 0)
                              (completion-tokens 0)
                              (validate? #t))
  "Create a new message with validation"

  ;; Validate message parameters if requested
  (when validate?
    (let ((params (hash 'role role 'content content)))
      (when tool-call-id
        (hash-put! params 'tool_call_id tool-call-id))
      (when tool-name
        (hash-put! params 'tool_name tool-name))

      (let ((result (validate-message-params params)))
        (unless (car result)
          (error "Invalid message parameters" errors: (cdr result))))))

  ;; Create message via database
  (let ((msg (message-create! (message-manager-agent-id manager)
                              role content
                              tool-calls: tool-calls
                              tool-call-id: tool-call-id
                              tool-name: tool-name
                              prompt-tokens: prompt-tokens
                              completion-tokens: completion-tokens)))

    ;; Add to cache
    (manager-cache-message! manager msg)

    msg))

(def (manager-create-user-message! manager content #!key (tokens 0))
  "Create user message"
  (manager-create-message! manager "user" content
                          prompt-tokens: tokens))

(def (manager-create-assistant-message! manager content
                                        #!key
                                        (tool-calls #f)
                                        (prompt-tokens 0)
                                        (completion-tokens 0))
  "Create assistant message"
  (manager-create-message! manager "assistant" content
                          tool-calls: tool-calls
                          prompt-tokens: prompt-tokens
                          completion-tokens: completion-tokens))

(def (manager-create-system-message! manager content)
  "Create system message"
  (manager-create-message! manager "system" content))

(def (manager-create-tool-message! manager content tool-call-id tool-name)
  "Create tool response message"
  (manager-create-message! manager "tool" content
                          tool-call-id: tool-call-id
                          tool-name: tool-name))

;;; ============================================================================
;;; Message Retrieval
;;; ============================================================================

(def (manager-get-messages manager #!key (limit 50) (offset 0) (role #f))
  "Get messages with optional filtering"
  (message-get-list (message-manager-agent-id manager)
                    limit: limit
                    offset: offset
                    role: role))

(def (manager-get-conversation manager #!key (limit 100))
  "Get full conversation history"
  (conversation-get (message-manager-agent-id manager)
                    limit: limit))

(def (manager-get-recent manager n)
  "Get N most recent messages"
  (conversation-get-recent (message-manager-agent-id manager) n))

(def (manager-get-by-role manager role #!key (limit 50))
  "Get messages filtered by role"
  (conversation-get-by-role (message-manager-agent-id manager)
                            role
                            limit: limit))

;;; ============================================================================
;;; Message Search and Filtering
;;; ============================================================================

(def (manager-search manager query #!key (limit 10))
  "Search messages by content"
  (message-search (message-manager-agent-id manager)
                  query
                  limit: limit))

(def (manager-search-by-date manager start-date end-date)
  "Search messages by date range"
  (message-search-by-date (message-manager-agent-id manager)
                          start-date
                          end-date))

(def (manager-filter-messages manager messages filter)
  "Filter messages using message-filter"
  (let ((filtered (filter (lambda (msg)
                            (filter-matches? msg filter))
                          messages)))

    ;; Apply limit and offset
    (let ((offset (message-filter-offset filter))
          (limit (message-filter-limit filter)))
      (take (drop filtered offset) limit))))

(def (manager-find-messages manager filter)
  "Find messages matching filter criteria"
  (let ((messages (manager-get-messages manager
                                        limit: 1000)))
    (manager-filter-messages manager messages filter)))

;;; ============================================================================
;;; Message Pagination
;;; ============================================================================

(defstruct page-result
  (messages      ; List of messages in this page
   total         ; Total number of messages
   page          ; Current page number (0-indexed)
   page-size     ; Number of messages per page
   has-next?     ; Whether there's a next page
   has-prev?)    ; Whether there's a previous page
  transparent: #t)

(def (manager-get-page manager page-number page-size
                       #!key (role #f) (filter #f))
  "Get paginated messages"
  (let* ((offset (* page-number page-size))
         (messages (if filter
                       (manager-find-messages manager filter)
                       (manager-get-messages manager
                                            limit: (+ page-size 1)
                                            offset: offset
                                            role: role)))
         (has-next? (> (length messages) page-size))
         (page-messages (take messages (min page-size (length messages))))
         (total (conversation-count-messages (message-manager-agent-id manager))))

    (make-page-result
     messages: page-messages
     total: total
     page: page-number
     page-size: page-size
     has-next?: has-next?
     has-prev?: (> page-number 0))))

(def (manager-get-next-page page-result)
  "Get next page from page result"
  (if (page-result-has-next? page-result)
      (+ (page-result-page page-result) 1)
      #f))

(def (manager-get-prev-page page-result)
  "Get previous page from page result"
  (if (page-result-has-prev? page-result)
      (- (page-result-page page-result) 1)
      #f))

;;; ============================================================================
;;; Message Statistics
;;; ============================================================================

(def (manager-get-stats manager)
  "Get conversation statistics"
  (conversation-get-stats (message-manager-agent-id manager)))

(def (manager-count-messages manager)
  "Count total messages"
  (conversation-count-messages (message-manager-agent-id manager)))

(def (manager-count-tokens manager)
  "Count total tokens used"
  (conversation-count-tokens (message-manager-agent-id manager)))

(def (manager-calculate-stats manager messages)
  "Calculate statistics from message list"
  (calculate-stats messages))

(def (manager-get-message-distribution manager)
  "Get distribution of messages by role"
  (let ((stats (manager-get-stats manager)))
    (hash
     'user (hash-ref stats 'user_messages)
     'assistant (hash-ref stats 'assistant_messages)
     'system 0  ;; Not tracked in current stats
     'tool 0))) ;; Not tracked in current stats

;;; ============================================================================
;;; Message Caching
;;; ============================================================================

(def (manager-cache-message! manager msg)
  "Add message to cache"
  (let ((cache (message-manager-cache manager))
        (msg-id (hash-ref msg 'id)))
    (hash-put! cache msg-id msg)

    ;; Evict old messages if cache is full
    (when (> (hash-length cache) (message-manager-cache-size manager))
      (manager-evict-cache! manager))))

(def (manager-evict-cache! manager)
  "Evict oldest messages from cache"
  (let* ((cache (message-manager-cache manager))
         (messages (hash-values cache))
         (sorted (sort messages
                      (lambda (a b)
                        (< (hash-ref a 'created_at)
                           (hash-ref b 'created_at)))))
         (to-remove (take sorted (quotient (hash-length cache) 2))))

    (for-each
     (lambda (msg)
       (hash-remove! cache (hash-ref msg 'id)))
     to-remove)))

(def (manager-get-cached manager msg-id)
  "Get message from cache"
  (hash-ref (message-manager-cache manager) msg-id #f))

(def (manager-clear-cache! manager)
  "Clear message cache"
  (hash-clear! (message-manager-cache manager)))

;;; ============================================================================
;;; Conversation Management
;;; ============================================================================

(def (manager-clear-conversation! manager)
  "Clear all messages for agent"
  (conversation-clear! (message-manager-agent-id manager))
  (manager-clear-cache! manager))

(def (manager-export-conversation manager #!key (format 'json))
  "Export conversation to specified format"
  (conversation-export (message-manager-agent-id manager)
                       format: format))

(def (manager-import-conversation! manager conversation-data)
  "Import conversation from exported data"
  (conversation-import! (message-manager-agent-id manager)
                        conversation-data)
  (manager-clear-cache! manager))

(def (manager-get-conversation-summary manager)
  "Get summary of conversation"
  (let ((stats (manager-get-stats manager)))
    (hash
     'agent_id (message-manager-agent-id manager)
     'total_messages (hash-ref stats 'total_messages)
     'total_tokens (hash-ref stats 'total_tokens)
     'first_message_at (hash-ref stats 'first_message_at)
     'last_message_at (hash-ref stats 'last_message_at)
     'message_distribution (manager-get-message-distribution manager))))

;;; ============================================================================
;;; Message Utilities
;;; ============================================================================

(def (manager-get-last-user-message manager)
  "Get the last user message"
  (let ((messages (manager-get-by-role manager "user" limit: 1)))
    (if (null? messages)
        #f
        (car messages))))

(def (manager-get-last-assistant-message manager)
  "Get the last assistant message"
  (let ((messages (manager-get-by-role manager "assistant" limit: 1)))
    (if (null? messages)
        #f
        (car messages))))

(def (manager-get-context-window manager max-tokens)
  "Get messages that fit within token limit"
  (let* ((messages (manager-get-conversation manager))
         (reversed (reverse messages))
         (result '())
         (total-tokens 0))

    (let loop ((msgs reversed))
      (if (or (null? msgs)
              (>= total-tokens max-tokens))
          (reverse result)
          (let* ((msg (car msgs))
                 (msg-tokens (hash-ref msg 'total_tokens 0)))
            (if (<= (+ total-tokens msg-tokens) max-tokens)
                (begin
                  (set! result (cons msg result))
                  (set! total-tokens (+ total-tokens msg-tokens))
                  (loop (cdr msgs)))
                (reverse result)))))))

(def (manager-truncate-to-limit manager max-messages)
  "Get only the most recent N messages"
  (manager-get-recent manager max-messages))

(def (manager-get-messages-since manager timestamp)
  "Get all messages since a given timestamp"
  (let ((messages (manager-get-conversation manager)))
    (filter (lambda (msg)
              (>= (hash-ref msg 'created_at) timestamp))
            messages)))

(def (manager-get-messages-before manager timestamp)
  "Get all messages before a given timestamp"
  (let ((messages (manager-get-conversation manager)))
    (filter (lambda (msg)
              (< (hash-ref msg 'created_at) timestamp))
            messages)))

(def (manager-get-messages-between manager start-timestamp end-timestamp)
  "Get messages between two timestamps"
  (let ((messages (manager-get-conversation manager)))
    (filter (lambda (msg)
              (let ((created (hash-ref msg 'created_at)))
                (and (>= created start-timestamp)
                     (<= created end-timestamp))))
            messages)))

;;; ============================================================================
;;; Batch Operations
;;; ============================================================================

(def (manager-create-messages-batch! manager message-list)
  "Create multiple messages in batch"
  (map (lambda (msg-params)
         (manager-create-message! manager
                                 (hash-ref msg-params 'role)
                                 (hash-ref msg-params 'content)
                                 tool-calls: (hash-ref msg-params 'tool_calls #f)
                                 tool-call-id: (hash-ref msg-params 'tool_call_id #f)
                                 tool-name: (hash-ref msg-params 'tool_name #f)
                                 prompt-tokens: (hash-ref msg-params 'prompt_tokens 0)
                                 completion-tokens: (hash-ref msg-params 'completion_tokens 0)
                                 validate?: #f))
       message-list))

(def (manager-delete-messages-batch! manager message-ids)
  "Delete multiple messages in batch"
  (for-each
   (lambda (msg-id)
     (message-delete! msg-id)
     (hash-remove! (message-manager-cache manager) msg-id))
   message-ids))

;;; ============================================================================
;;; Advanced Search
;;; ============================================================================

(def (manager-search-with-context manager query #!key (context-size 2))
  "Search messages and include surrounding context"
  (let* ((matches (manager-search manager query limit: 100))
         (all-messages (manager-get-conversation manager))
         (results '()))

    (for-each
     (lambda (match)
       (let* ((match-id (hash-ref match 'id))
              (match-idx (list-index
                          (lambda (msg) (equal? (hash-ref msg 'id) match-id))
                          all-messages)))
         (when match-idx
           (let* ((start-idx (max 0 (- match-idx context-size)))
                  (end-idx (min (length all-messages)
                               (+ match-idx context-size 1)))
                  (context (take (drop all-messages start-idx)
                                (- end-idx start-idx))))
             (set! results (cons (hash 'match match
                                      'context context
                                      'match_index match-idx)
                                results))))))
     matches)

    (reverse results)))

(def (manager-find-tool-calls manager #!key (tool-name #f) (limit 50))
  "Find messages with tool calls"
  (let ((messages (manager-get-by-role manager "assistant" limit: limit)))
    (filter (lambda (msg)
              (and (message-has-tool-calls? msg)
                   (or (not tool-name)
                       (let ((tool-calls (hash-ref msg 'tool_calls)))
                         (any (lambda (tc)
                                (equal? (hash-ref tc 'name) tool-name))
                              tool-calls)))))
            messages)))

(def (manager-find-tool-responses manager tool-call-id)
  "Find tool response messages for a given tool call"
  (let ((messages (manager-get-by-role manager "tool" limit: 100)))
    (filter (lambda (msg)
              (equal? (hash-ref msg 'tool_call_id) tool-call-id))
            messages)))

;;; ============================================================================
;;; Message Formatting
;;; ============================================================================

(def (manager-format-message msg #!key (format 'text))
  "Format message for display"
  (case format
    ((text)
     (format "~a: ~a"
             (hash-ref msg 'role)
             (hash-ref msg 'content)))

    ((json)
     (json-object->string msg))

    ((markdown)
     (format "**~a**: ~a"
             (hash-ref msg 'role)
             (hash-ref msg 'content)))

    (else
     (error "Unsupported format" format: format))))

(def (manager-format-conversation manager #!key (format 'text) (limit 100))
  "Format entire conversation"
  (let ((messages (manager-get-conversation manager limit: limit)))
    (case format
      ((text)
       (string-join
        (map (lambda (msg)
               (manager-format-message msg format: 'text))
             messages)
        "\n"))

      ((json)
       (json-object->string
        (hash 'agent_id (message-manager-agent-id manager)
              'messages messages)))

      ((markdown)
       (string-join
        (map (lambda (msg)
               (manager-format-message msg format: 'markdown))
             messages)
        "\n\n"))

      (else
       (error "Unsupported format" format: format)))))

;;; ============================================================================
;;; Message Validation
;;; ============================================================================

(def (manager-validate-message manager msg-params)
  "Validate message parameters"
  (validate-message-params msg-params))

(def (manager-validate-conversation manager)
  "Validate entire conversation for consistency"
  (let ((messages (manager-get-conversation manager))
        (errors '()))

    ;; Check for orphaned tool responses
    (let ((tool-messages (filter message-is-tool? messages))
          (assistant-messages (filter message-is-assistant? messages)))

      (for-each
       (lambda (tool-msg)
         (let ((tool-call-id (hash-ref tool-msg 'tool_call_id)))
           (unless (any (lambda (asst-msg)
                          (let ((tool-calls (hash-ref asst-msg 'tool_calls #f)))
                            (and tool-calls
                                 (any (lambda (tc)
                                        (equal? (hash-ref tc 'id) tool-call-id))
                                      tool-calls))))
                        assistant-messages)
             (set! errors (cons (format "Orphaned tool response: ~a" tool-call-id)
                               errors)))))
       tool-messages))

    ;; Return validation result
    (if (null? errors)
        (cons #t #f)
        (cons #f errors))))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Connect to database
(db-connect!)

;; Create message manager for agent
(def manager (make-manager agent-id cache-size: 100))

;; Create messages
(manager-create-user-message! manager "Hello!" tokens: 5)
(manager-create-assistant-message! manager "Hi there!"
                                   prompt-tokens: 10
                                   completion-tokens: 15)

;; Get messages
(def messages (manager-get-messages manager limit: 50))
(def conversation (manager-get-conversation manager))
(def recent (manager-get-recent manager 10))

;; Search messages
(def results (manager-search manager "hello" limit: 10))
(def date-results (manager-search-by-date manager 1705315200 1705401600))

;; Pagination
(def page (manager-get-page manager 0 20))
(displayln (format "Page ~a of ~a messages"
                   (page-result-page page)
                   (page-result-total page)))

;; Statistics
(def stats (manager-get-stats manager))
(displayln (format "Total messages: ~a" (hash-ref stats 'total_messages)))
(displayln (format "Total tokens: ~a" (hash-ref stats 'total_tokens)))

;; Context window
(def context (manager-get-context-window manager 4000))
(displayln (format "Messages in context: ~a" (length context)))

;; Export conversation
(def json-export (manager-export-conversation manager format: 'json))
(def text-export (manager-export-conversation manager format: 'text))

;; Format conversation
(def formatted (manager-format-conversation manager format: 'markdown))
(displayln formatted)

;; Validate conversation
(def validation (manager-validate-conversation manager))
(if (car validation)
    (displayln "Conversation is valid")
    (displayln (format "Validation errors: ~a" (cdr validation))))

;; Clear conversation
(manager-clear-conversation! manager)
|#

