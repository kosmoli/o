;;; database/client.ss - Gerbil Database Client
;;;
;;; This module provides a client for communicating with the Elixir database
;;; layer via MessagePack protocol.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/net/socket
  :gerbil/gambit/ports
  ../agent/msgpack)

;;; ============================================================================
;;; Database Client State
;;; ============================================================================

(defstruct db-client
  (socket        ; socket connection to Elixir
   request-id)   ; counter for request IDs
  transparent: #t)

(def *db-client* #f)

;;; ============================================================================
;;; Client Initialization
;;; ============================================================================

(def (db-connect! #!key (host "localhost") (port 9000))
  "Connect to Elixir database service"
  (let ((sock (socket-connect host port)))
    (set! *db-client*
          (make-db-client
           socket: sock
           request-id: 0))
    (displayln (format "Database client connected to ~a:~a" host port))
    *db-client*))

(def (db-disconnect!)
  "Disconnect from database service"
  (when *db-client*
    (socket-close (db-client-socket *db-client*))
    (set! *db-client* #f)
    (displayln "Database client disconnected")))

(def (db-connected?)
  "Check if database client is connected"
  (and *db-client* #t))

;;; ============================================================================
;;; Request/Response Protocol
;;; ============================================================================

(def (db-request operation params)
  "Send database request and wait for response"
  (unless *db-client*
    (error "Database client not connected"))

  (let* ((client *db-client*)
         (request-id (db-client-request-id client))
         (request (hash
                   'type "db_request"
                   'request_id request-id
                   'operation operation
                   'params params)))

    ;; Increment request ID
    (set! (db-client-request-id client) (+ request-id 1))

    ;; Send request
    (send-msgpack (db-client-socket client) request)

    ;; Wait for response
    (let ((response (receive-msgpack (db-client-socket client))))
      (if (hash-ref response 'success)
          (hash-ref response 'data)
          (error "Database operation failed"
                 operation: operation
                 error: (hash-ref response 'error))))))

;;; ============================================================================
;;; Agent Operations
;;; ============================================================================

(def (db-create-agent params)
  "Create a new agent in database"
  (db-request 'create_agent params))

(def (db-get-agent agent-id)
  "Get agent by ID"
  (db-request 'get_agent (hash 'agent_id agent-id)))

(def (db-update-agent agent-id params)
  "Update agent"
  (db-request 'update_agent
              (hash 'agent_id agent-id
                    'params params)))

(def (db-delete-agent agent-id)
  "Delete agent (soft delete)"
  (db-request 'delete_agent (hash 'agent_id agent-id)))

(def (db-list-agents #!key (limit 10) (offset 0))
  "List agents with pagination"
  (db-request 'list_agents
              (hash 'limit limit 'offset offset)))

;;; ============================================================================
;;; Message Operations
;;; ============================================================================

(def (db-create-message agent-id params)
  "Create a new message"
  (db-request 'create_message
              (hash 'agent_id agent-id
                    'params params)))

(def (db-get-messages agent-id #!key (limit 50) (offset 0) (role #f))
  "Get messages for agent"
  (let ((params (hash 'agent_id agent-id
                      'limit limit
                      'offset offset)))
    (when role
      (hash-put! params 'role role))
    (db-request 'get_messages params)))

(def (db-delete-message message-id)
  "Delete message"
  (db-request 'delete_message (hash 'message_id message-id)))

;;; ============================================================================
;;; Memory Block Operations
;;; ============================================================================

(def (db-create-memory-block agent-id params)
  "Create or update memory block"
  (db-request 'create_memory_block
              (hash 'agent_id agent-id
                    'params params)))

(def (db-get-memory-blocks agent-id)
  "Get all memory blocks for agent"
  (db-request 'get_memory_blocks (hash 'agent_id agent-id)))

(def (db-update-memory-block agent-id label value)
  "Update memory block value"
  (db-request 'update_memory_block
              (hash 'agent_id agent-id
                    'label label
                    'value value)))

;;; ============================================================================
;;; Archival Memory Operations
;;; ============================================================================

(def (db-insert-archival-memory agent-id params)
  "Insert archival memory entry"
  (db-request 'insert_archival_memory
              (hash 'agent_id agent-id
                    'params params)))

(def (db-search-archival-memory agent-id query #!key (limit 10) (search-type 'text))
  "Search archival memory"
  (db-request 'search_archival_memory
              (hash 'agent_id agent-id
                    'query query
                    'limit limit
                    'search_type search-type)))

;;; ============================================================================
;;; Statistics Operations
;;; ============================================================================

(def (db-get-agent-statistics agent-id)
  "Get agent statistics"
  (db-request 'get_agent_statistics (hash 'agent_id agent-id)))

(def (db-update-statistics agent-id updates)
  "Update agent statistics"
  (db-request 'update_statistics
              (hash 'agent_id agent-id
                    'updates updates)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(def (db-agent-exists? agent-id)
  "Check if agent exists"
  (try
   (begin
     (db-get-agent agent-id)
     #t)
   (catch (e)
     #f)))

(def (db-get-conversation agent-id #!key (limit 100))
  "Get full conversation history"
  (db-get-messages agent-id limit: limit offset: 0))

(def (db-get-core-memory agent-id)
  "Get core memory blocks as hash"
  (let ((blocks (db-get-memory-blocks agent-id)))
    (list->hash
     (map (lambda (block)
            (cons (string->symbol (hash-ref block 'label))
                  (hash-ref block 'value)))
          blocks))))

(def (db-update-core-memory agent-id label value)
  "Update core memory block"
  (db-update-memory-block agent-id label value))

;;; ============================================================================
;;; Batch Operations
;;; ============================================================================

(def (db-create-messages agent-id messages)
  "Create multiple messages"
  (map (lambda (msg)
         (db-create-message agent-id msg))
       messages))

(def (db-initialize-agent-memory agent-id persona human)
  "Initialize agent memory blocks"
  (db-create-memory-block agent-id
                          (hash 'label "persona"
                                'value persona))
  (db-create-memory-block agent-id
                          (hash 'label "human"
                                'value human)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Connect to database
(db-connect! host: "localhost" port: 9000)

;; Create agent
(def agent (db-create-agent
            (hash 'name "MyAgent"
                  'llm_provider "openai"
                  'llm_model "gpt-4"
                  'system_prompt "You are a helpful assistant.")))

(def agent-id (hash-ref agent 'id))

;; Create message
(def message (db-create-message
              agent-id
              (hash 'role "user"
                    'content "Hello!"
                    'total_tokens 5)))

;; Get messages
(def messages (db-get-messages agent-id limit: 50))

;; Update core memory
(db-update-core-memory agent-id "persona" "You are a very helpful assistant.")

;; Get core memory
(def core-memory (db-get-core-memory agent-id))

;; Search archival memory
(def results (db-search-archival-memory
              agent-id
              "important"
              limit: 10
              search-type: 'text))

;; Get statistics
(def stats (db-get-agent-statistics agent-id))

;; Disconnect
(db-disconnect!)
|#
