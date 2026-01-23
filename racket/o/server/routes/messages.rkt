#lang racket

;;; server/routes/messages.rkt - Message Management Routes
;;;
;;; This module provides REST API endpoints for message management.

(provide send-message-handler
         get-messages-handler
         get-message-handler
         delete-message-handler
         get-conversation-handler
         clear-conversation-handler
         search-messages-handler
         get-message-stats-handler
         register-message-routes!)

(require racket/hash
        racket/format
        "../http.rkt"
        "../router.rkt"
        "../../database/messages.rkt")

;;; ============================================================================
;;; Message Operations
;;; ============================================================================

(define (send-message-handler req)
  "POST /v1/agents/:id/messages - Send message to agent"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      (if (not (http-request-json req))
          (make-error-response "JSON body required" #:status 400)
          (let ([data (http-request-json req)])
            ;; Validate required fields
            (cond
             [(not (hash-has-key? data 'message))
              (make-error-response "Field 'message' is required" #:status 400)]
             [else
              (define message-id (generate-message-id))
              (define user-message (hash-ref data 'message))
              (define stream (hash-ref data 'stream #f))

              (if stream
                  ;; TODO: Implement streaming response
                  (make-error-response "Streaming not yet implemented" #:status 501)
                  ;; Non-streaming response
                  ;; TODO: Actually process message with agent
                  (make-json-response
                   (hash 'id message-id
                         'agent_id agent-id
                         'messages (list
                                    (hash 'role "user"
                                          'content user-message
                                          'timestamp (current-seconds))
                                    (hash 'role "assistant"
                                          'content "This is a mock response. Agent processing not yet implemented."
                                          'timestamp (current-seconds)))
                         'usage (hash 'prompt_tokens 10
                                      'completion_tokens 15
                                      'total_tokens 25)
                         'created_at (current-seconds))
                   #:status 201))])))))

(define (get-messages-handler req)
  "GET /v1/agents/:id/messages - Get agent messages"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      (let* ([params (http-request-query-params req)]
             [limit-str (hash-ref params "limit" "50")]
             [offset-str (hash-ref params "offset" "0")]
             [role (hash-ref params "role" #f)]
             [limit (string->number limit-str)]
             [offset (string->number offset-str)])

        ;; TODO: Fetch messages from database
        (make-json-response
         (hash 'messages (list
                         (hash 'id "msg-1"
                               'role "user"
                               'content "Hello!"
                               'timestamp (current-seconds))
                         (hash 'id "msg-2"
                               'role "assistant"
                               'content "Hi there! How can I help you?"
                               'timestamp (current-seconds)))
               'total 2
               'limit limit
               'offset offset)))))

(define (get-message-handler req)
  "GET /v1/agents/:id/messages/:message_id - Get specific message"
  (define agent-id (get-path-param req 'id))
  (define message-id (get-path-param req 'message_id))

  (cond
   [(not agent-id)
    (make-error-response "Agent ID required" #:status 400)]
   [(not message-id)
    (make-error-response "Message ID required" #:status 400)]
   [else
    ;; TODO: Fetch message from database
    (make-json-response
     (hash 'id message-id
           'agent_id agent-id
           'role "user"
           'content "Hello!"
           'timestamp (current-seconds)))]))

(define (delete-message-handler req)
  "DELETE /v1/agents/:id/messages/:message_id - Delete message"
  (define agent-id (get-path-param req 'id))
  (define message-id (get-path-param req 'message_id))

  (cond
   [(not agent-id)
    (make-error-response "Agent ID required" #:status 400)]
   [(not message-id)
    (make-error-response "Message ID required" #:status 400)]
   [else
    ;; TODO: Delete message from database
    (make-json-response
     (hash 'deleted #t
           'id message-id))]))

;;; ============================================================================
;;; Conversation Management
;;; ============================================================================

(define (get-conversation-handler req)
  "GET /v1/agents/:id/conversation - Get full conversation history"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      (let* ([params (http-request-query-params req)]
             [limit-str (hash-ref params "limit" "100")]
             [offset-str (hash-ref params "offset" "0")]
             [limit (string->number limit-str)]
             [offset (string->number offset-str)])

        ;; TODO: Fetch conversation from database
        (make-json-response
         (hash 'agent_id agent-id
               'messages (list
                           (hash 'id "msg-1"
                                 'role "user"
                                 'content "Hello!"
                                 'timestamp (- (current-seconds) 100))
                           (hash 'id "msg-2"
                                 'role "assistant"
                                 'content "Hi! How can I help?"
                                 'timestamp (- (current-seconds) 90))
                           (hash 'id "msg-3"
                                 'role "user"
                                 'content "Tell me about yourself."
                                 'timestamp (- (current-seconds) 80))
                           (hash 'id "msg-4"
                                 'role "assistant"
                                 'content "I'm an AI assistant here to help you."
                                 'timestamp (- (current-seconds) 70)))
               'total 4
               'limit limit
               'offset offset)))))

(define (clear-conversation-handler req)
  "DELETE /v1/agents/:id/conversation - Clear conversation history"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      ;; TODO: Clear conversation from database
      (make-json-response
       (hash 'cleared #t
             'agent_id agent-id))))

;;; ============================================================================
;;; Message Search
;;; ============================================================================

(define (search-messages-handler req)
  "POST /v1/agents/:id/messages/search - Search messages"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      (if (not (http-request-json req))
          (make-error-response "JSON body required" #:status 400)
          (let ([data (http-request-json req)])
            (cond
             [(not (hash-has-key? data 'query))
              (make-error-response "Field 'query' is required" #:status 400)]
             [else
              (define query (hash-ref data 'query))
              (define limit (hash-ref data 'limit 10))
              (define search-type (hash-ref data 'search_type "text"))

              ;; TODO: Search messages in database
              (make-json-response
               (hash 'query query
                     'search_type search-type
                     'results (list
                                 (hash 'id "msg-1"
                                       'role "user"
                                       'content "Hello!"
                                       'timestamp (current-seconds)
                                       'score 0.95))
                     'total 1))])))))

;;; ============================================================================
;;; Message Statistics
;;; ============================================================================

(define (get-message-stats-handler req)
  "GET /v1/agents/:id/messages/stats - Get message statistics"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      ;; TODO: Calculate stats from database
      (make-json-response
       (hash 'agent_id agent-id
             'total_messages 100
             'user_messages 50
             'assistant_messages 50
             'total_tokens 5000
             'avg_message_length 50
             'first_message_at (- (current-seconds) 86400)
             'last_message_at (current-seconds)))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (generate-message-id)
  "Generate unique message ID"
  (format "msg-~a-~a"
          (current-seconds)
          (random 1000000)))

;;; ============================================================================
;;; Route Registration
;;; ============================================================================

(define (register-message-routes! router)
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
