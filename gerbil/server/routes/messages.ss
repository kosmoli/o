;;; server/routes/messages.ss - Message Management Routes
;;;
;;; This module provides REST API endpoints for message management.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :o/server/http
  :o/server/router)

;;; ============================================================================
;;; Message Operations
;;; ============================================================================

(def (send-message-handler req)
  "POST /v1/agents/:id/messages - Send message to agent"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        (if (not (http-request-json req))
            (make-error-response "JSON body required" status: 400)

            (let ((data (http-request-json req)))
              ;; Validate required fields
              (cond
               ((not (hash-key? data 'message))
                (make-error-response "Field 'message' is required" status: 400))

               (else
                ;; TODO: Actually process message with agent
                (let ((message-id (generate-message-id))
                      (user-message (hash-ref data 'message))
                      (stream (hash-ref data 'stream #f)))

                  (if stream
                      ;; TODO: Implement streaming response
                      (make-error-response "Streaming not yet implemented" status: 501)

                      ;; Non-streaming response
                      (make-json-response
                       (let ((ht (make-hash-table)))
  (hash-put! ht 'id message-id)
  (hash-put! ht 'agent_id agent-id)
  (hash-put! ht 'messages (list
                                   (hash
                                    'role "user"
                                    'content user-message
                                    'timestamp (current-seconds)))
                                   (hash ('role "assistant") ('content "This is a mock response. Agent processing not yet implemented.") ('timestamp (current-seconds))))
  ht)
                       status: 201))))))))))

(def (get-messages-handler req)
  "GET /v1/agents/:id/messages - Get agent messages"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        (let ((limit (or (get-query-param req "limit") "50"))
              (offset (or (get-query-param req "offset") "0"))
              (role (get-query-param req "role")))

          ;; TODO: Actually fetch messages from database
          (make-json-response
           (let ((ht (make-hash-table)))
  (hash-put! ht 'messages (list
                       (hash
                        'id "msg-1"
                        'role "user"
                        'content "Hello!"
                        'timestamp (current-seconds)))
                       (hash ('id "msg-2") ('role "assistant") ('content "Hi there! How can I help you?") ('timestamp (current-seconds))))
  ht))))))

(def (get-message-handler req)
  "GET /v1/agents/:id/messages/'message_id - Get specific message"
  (let ((agent-id (get-path-param req 'id))
        (message-id (get-path-param req 'message_id)))

    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        (if (not message-id)
            (make-error-response "Message ID required" status: 400)

            ;; TODO: Actually fetch message from database
            (make-json-response
             (let ((ht (make-hash-table)))
  (hash-put! ht 'id message-id)
  (hash-put! ht 'agent_id agent-id)
  (hash-put! ht 'role "user")
  (hash-put! ht 'content "Hello!")
  (hash-put! ht 'timestamp (current-seconds))
  ht))))))

(def (delete-message-handler req)
  "DELETE /v1/agents/:id/messages/'message_id - Delete message"
  (let ((agent-id (get-path-param req 'id))
        (message-id (get-path-param req 'message_id)))

    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        (if (not message-id)
            (make-error-response "Message ID required" status: 400)

            ;; TODO: Actually delete message from database
            (make-json-response
             (let ((ht (make-hash-table)))
  (hash-put! ht 'deleted #t)
  (hash-put! ht 'id message-id)
  ht))))))

;;; ============================================================================
;;; Conversation Management
;;; ============================================================================

(def (get-conversation-handler req)
  "GET /v1/agents/:id/conversation - Get full conversation history"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        (let ((limit (or (get-query-param req "limit") "100"))
              (offset (or (get-query-param req "offset") "0")))

          ;; TODO: Actually fetch conversation from database
          (make-json-response
           (let ((ht (make-hash-table)))
  (hash-put! ht 'agent_id agent-id)
  (hash-put! ht 'messages (list
                       (hash
                        'id "msg-1"
                        'role "user"
                        'content "Hello!"
                        'timestamp (- (current-seconds)) 100))
                       (hash ('id "msg-2") ('role "assistant") ('content "Hi! How can I help?") ('timestamp (- (current-seconds)) 90))
                       (hash ('id "msg-3") ('role "user") ('content "Tell me about yourself.") ('timestamp (- (current-seconds)) 80))
                       (hash ('id "msg-4") ('role "assistant") ('content "I'm an AI assistant here to help you.") ('timestamp (- (current-seconds)) 70)))
  ht))))))

(def (clear-conversation-handler req)
  "DELETE /v1/agents/:id/conversation - Clear conversation history"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        ;; TODO: Actually clear conversation from database
        (make-json-response
         (let ((ht (make-hash-table)))
  (hash-put! ht 'cleared #t)
  (hash-put! ht 'agent_id agent-id)
  ht)))))

;;; ============================================================================
;;; Message Search
;;; ============================================================================

(def (search-messages-handler req)
  "POST /v1/agents/:id/messages/search - Search messages"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        (if (not (http-request-json req))
            (make-error-response "JSON body required" status: 400)

            (let ((data (http-request-json req)))
              (cond
               ((not (hash-key? data 'query))
                (make-error-response "Field 'query' is required" status: 400))

               (else
                (let ((query (hash-ref data 'query))
                      (limit (hash-ref data 'limit 10))
                      (search-type (hash-ref data 'search_type "text")))

                  ;; TODO: Actually search messages in database
                  (make-json-response
                   (let ((ht (make-hash-table)))
  (hash-put! ht 'query query)
  (hash-put! ht 'search_type search-type)
  (hash-put! ht 'results (list
                              (hash
                               'id "msg-1"
                               'role "user"
                               'content "Hello!"
                               'timestamp (current-seconds))
                               'score 0.95))
  ht))))))))))

;;; ============================================================================
;;; Message Statistics
;;; ============================================================================

(def (get-message-stats-handler req)
  "GET /v1/agents/:id/messages/stats - Get message statistics"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        ;; TODO: Actually calculate stats from database
        (make-json-response
         (let ((ht (make-hash-table)))
  (hash-put! ht 'agent_id agent-id)
  (hash-put! ht 'total_messages 100)
  (hash-put! ht 'user_messages 50)
  (hash-put! ht 'assistant_messages 50)
  (hash-put! ht 'total_tokens 5000)
  (hash-put! ht 'avg_message_length 50)
  (hash-put! ht 'first_message_at (- (current-seconds)) 86400)
  ht)))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (generate-message-id)
  "Generate unique message ID"
  (string-append
   "msg-"
   (number->string (current-seconds))
   "-"
   (number->string (random-integer 1000000))))

;;; ============================================================================
;;; Route Registration
;;; ============================================================================

(def (register-message-routes! router)
  "Register all message routes"

  ;; Message operations
  (post! router "/v1/agents/:id/messages" send-message-handler)
  (get! router "/v1/agents/:id/messages" get-messages-handler)
  (get! router "/v1/agents/:id/messages/:message_id" get-message-handler)
  (delete! router "/v1/agents/:id/messages/:message_id" delete-message-handler)

  ;; Conversation management
  (get! router "/v1/agents/:id/conversation" get-conversation-handler)
  (delete! router "/v1/agents/:id/conversation" clear-conversation-handler)

  ;; Message search
  (post! router "/v1/agents/:id/messages/search" search-messages-handler)

  ;; Message statistics
  (get! router "/v1/agents/:id/messages/stats" get-message-stats-handler)

  router)

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create router and register message routes
(def router (make-router-instance))
(register-message-routes! router)

;; Test with curl:

# Send message
curl -X POST http://localhost:8283/v1/agents/agent-123/messages \
  -H "Content-Type: application/json" \
  -d '{
    "message": "Hello, how are you?",
    "stream": false
  }'

# Get messages
curl http://localhost:8283/v1/agents/agent-123/messages?limit=50&offset=0

# Get specific message
curl http://localhost:8283/v1/agents/agent-123/messages/msg-456

# Delete message
curl -X DELETE http://localhost:8283/v1/agents/agent-123/messages/msg-456

# Get conversation
curl http://localhost:8283/v1/agents/agent-123/conversation

# Clear conversation
curl -X DELETE http://localhost:8283/v1/agents/agent-123/conversation

# Search messages
curl -X POST http://localhost:8283/v1/agents/agent-123/messages/search \
  -H "Content-Type: application/json" \
  -d '{
    "query": "hello",
    "limit": 10,
    "search_type": "text"
  }'

# Get message stats
curl http://localhost:8283/v1/agents/agent-123/messages/stats
|#
