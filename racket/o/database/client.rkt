#lang racket

;;; database/client.rkt - Racket Database Client
;;;
;;; This module provides a client for communicating with the Elixir database
;;; layer via MessagePack protocol over TCP sockets.

(provide db-connect!
         db-disconnect!
         db-connected?
         db-create-agent
         db-get-agent
         db-update-agent
         db-delete-agent
         db-list-agents
         db-get-memory-blocks
         db-create-memory-block
         db-update-memory-block
         db-delete-memory-block
         db-get-messages
         db-create-message
         db-delete-message
         db-search-archival-memory
         db-get-conversation
         db-get-conversation-by-role
         db-get-agent-statistics
         db-update-statistics
         db-initialize-agent-memory
         db-agent-exists?
         db-insert-archival-memory
         (struct-out db-client)
         *db-client*)

(require racket/hash
        racket/format
        racket/port
        "msgpack.rkt")

;;; ============================================================================
;;; Database Client State
;;; ============================================================================

(struct db-client
  (socket        ; tcp input/output port pair (in . out) or #f
   host          ; server host
   port          ; server port
   request-id)   ; counter for request IDs
  #:mutable
  #:transparent)

(define *db-client* #f)

;;; ============================================================================
;;; Client Initialization
;;; ============================================================================

(define (db-connect! #:host [host "localhost"] #:port [port 9000])
  (displayln (format "Connecting to database at ~a:~a..." host port))

  (let-values ([(in out) (tcp-connect host port)])
    (set! *db-client*
          (db-client (cons in out)  ; socket as (in . out) pair
                     host
                     port
                     0))

    (displayln (format "Database client connected to ~a:~a" host port))
    *db-client*))

(define (db-disconnect!)
  (when *db-client*
    (define sock-ports (db-client-socket *db-client*))
    (close-input-port (car sock-ports))
    (close-output-port (cdr sock-ports))
    (set! *db-client* #f)
    (displayln "Database client disconnected")))

(define (db-connected?)
  (and *db-client* (db-client-socket *db-client*) #t))

;;; ============================================================================
;;; Request/Response Protocol
;;; ============================================================================

(define (db-request operation params)
  (unless (db-connected?)
    (error 'db-request "Database client not connected"))

    (define client *db-client*)
    (define req-id (db-client-request-id client))

    ;; Increment request ID
    (set-db-client-request-id! client (+ req-id 1))

    ;; Build request
    (define request
      (hash 'type "db_request"
            'request_id req-id
            'operation (if (symbol? operation)
                           (symbol->string operation)
                           operation)
            'params params))

    ;; Send request as MessagePack
    (define request-bytes (msgpack-encode request))
    (displayln (format "DB request: ~a (id: ~a)" operation req-id))

    (define sock-ports (db-client-socket client))
    (write-bytes request-bytes (cdr sock-ports))
    (flush-output (cdr sock-ports))

    ;; Wait for response
    (define response-bytes (read-msgpack-bytes (car sock-ports)))
    (define response (msgpack-decode response-bytes))

    (displayln (format "DB response: ~a" (hash-ref response 'type #f)))

    (if (hash-ref response 'success #f)
        (hash-ref response 'data #f)
        (error 'db-request
               (format "Database operation failed: ~a"
                       (hash-ref response 'error "Unknown error")))))

(define (read-msgpack-bytes port)
  (read-bytes 65536 port))

;;; ============================================================================
;;; Agent Operations
;;; ============================================================================

(define (db-create-agent params)
  (db-request 'create_agent params))

(define (db-get-agent agent-id)
  (db-request 'get_agent (hash 'agent_id agent-id)))

(define (db-update-agent agent-id params)
  (db-request 'update_agent
              (hash 'agent_id agent-id
                    'params params)))

(define (db-delete-agent agent-id)
  (db-request 'delete_agent (hash 'agent_id agent-id)))

(define (db-list-agents #:limit [limit 10] #:offset [offset 0])
  (db-request 'list_agents
              (hash 'limit limit
                    'offset offset)))

;;; ============================================================================
;;; Memory Block Operations
;;; ============================================================================

(define (db-get-memory-blocks agent-id)
  (db-request 'get_memory_blocks (hash 'agent_id agent-id)))

(define (db-create-memory-block params)
  (db-request 'create_memory_block params))

(define (db-update-memory-block block-id value)
  (db-request 'update_memory_block
              (hash 'block_id block-id
                    'value value)))

(define (db-delete-memory-block agent-id label)
  (db-request 'delete_memory_block
              (hash 'agent_id agent-id
                    'label label)))

;;; ============================================================================
;;; Message Operations
;;; ============================================================================

(define (db-get-messages agent-id #:limit [limit 50] #:offset [offset 0] #:role [role #f])
  (define base-params (hash 'agent_id agent-id
                            'limit limit
                            'offset offset))
  (define final-params
    (if role
        (hash-set base-params 'role role)
        base-params))
  (db-request 'get_messages final-params))

(define (db-create-message params)
  (db-request 'create_message params))

(define (db-delete-message message-id)
  (db-request 'delete_message (hash 'message_id message-id)))

;;; ============================================================================
;;; Archival Memory Operations
;;; ============================================================================

(define (db-insert-archival-memory agent-id params)
  (db-request 'insert_archival_memory
              (hash 'agent_id agent-id
                    'params params)))

(define (db-search-archival-memory agent-id query #:limit [limit 10] #:search-type [search-type 'text])
  (db-request 'search_archival_memory
              (hash 'agent_id agent-id
                    'query query
                    'limit limit
                    'search_type (if (symbol? search-type)
                                    (symbol->string search-type)
                                    search-type))))

;;; ============================================================================
;;; Conversation Operations
;;; ============================================================================

(define (db-get-conversation agent-id #:limit [limit 100])
  (db-request 'get_conversation (hash 'agent_id agent-id 'limit limit)))

(define (db-get-conversation-by-role agent-id role #:limit [limit 100])
  (db-request 'get_conversation_by_role
              (hash 'agent_id agent-id
                    'role role
                    'limit limit)))

;;; ============================================================================
;;; Statistics Operations
;;; ============================================================================

(define (db-get-agent-statistics agent-id)
  (db-request 'get_agent_statistics (hash 'agent_id agent-id)))

(define (db-update-statistics agent-id updates)
  (db-request 'update_statistics
              (hash 'agent_id agent-id
                    'updates updates)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(define (db-initialize-agent-memory agent-id persona human)
  (db-create-memory-block
   (hash 'agent_id agent-id
         'label "persona"
         'value persona
         'block_type "persona"))

  (db-create-memory-block
   (hash 'agent_id agent-id
         'label "human"
         'value human
         'block_type "human")))

(define (db-agent-exists? agent-id)
  (with-handlers* ([exn:fail?
                     (lambda (e) #f)])
    (db-get-agent agent-id)
    #t))

(define (db-get-core-memory agent-id)
  (define blocks (db-get-memory-blocks agent-id))
  (for/fold ([h (hash)])
            ([block (in-list blocks)])
    (hash-set h
              (hash-ref block 'label "")
              (hash-ref block 'value ""))))

(define (db-create-messages agent-id messages)
  (map (lambda (msg)
         (db-create-message (hash-set msg 'agent_id agent-id)))
       messages))
