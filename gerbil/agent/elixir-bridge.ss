;;; elixir-bridge.ss - Bridge for communication with Elixir supervisor
;;;
;;; This module provides functions for bidirectional communication
;;; between Gerbil and the Elixir supervision layer via MessagePack
;;; over stdin/stdout.

(export #t
  elixir-send
  elixir-receive
  elixir-checkpoint!
  elixir-wal-log!
  start-heartbeat-thread!
  wait-for-message)

(import :std/format
        :std/sugar
        :std/iter
        :std/misc/queue
        :std/misc/hash
        :std/misc/uuid
        :std/net/msgpack)

;;; Global state
(def *elixir-port* (current-output-port))
(def *elixir-input* (current-input-port))
(def *message-queue* (make-queue))
(def *heartbeat-thread* #f)

;;; Send message to Elixir
;;; 
;;; Format: [4-byte length][MessagePack payload]
;;; 
;;; Example:
;;;   (elixir-send "heartbeat" #t)
;;;   (elixir-send "checkpoint" (hash 'state state-data))
(def (elixir-send msg-type data)
  (let* ((msg (hash 'type msg-type
                    'data data
                    'timestamp (time->seconds (current-time))))
         (packed (msgpack-encode msg))
         (len (u8vector-length packed)))
    ;; Write 4-byte length prefix (big-endian)
    (write-u32 len *elixir-port*)
    ;; Write MessagePack payload
    (write-u8vector packed *elixir-port*)
    ;; Flush immediately
    (force-output *elixir-port*)))

;;; Receive message from Elixir
;;; 
;;; Blocks until a message is available.
;;; Returns decoded message hash or #f on error.
(def (elixir-receive)
  (try
   (let ((len (read-u32 *elixir-input*)))
     (when len
       (let ((data (read-u8vector len *elixir-input*)))
         (msgpack-decode data))))
   (catch (e)
     (displayln "Error receiving message: " e)
     #f)))

;;; Wait for specific message type with timeout
;;; 
;;; Example:
;;;   (wait-for-message "checkpoint_ack" timeout: 30)
(def (wait-for-message msg-type timeout: (timeout 30))
  (let ((deadline (+ (time->seconds (current-time)) timeout)))
    (let loop ()
      (if (> (time->seconds (current-time)) deadline)
          (error "Timeout waiting for message" type: msg-type)
          (let ((msg (elixir-receive)))
            (if (and msg (equal? (hash-ref msg 'type) msg-type))
                msg
                (loop)))))))

;;; Create checkpoint
;;; 
;;; Serializes agent state and sends to Elixir for persistence.
;;; Returns checkpoint ID.
(def (elixir-checkpoint! agent)
  (let ((snapshot (serialize-agent-state agent)))
    ;; Send checkpoint request
    (elixir-send "checkpoint" snapshot)
    
    ;; Wait for acknowledgment
    (let ((ack (wait-for-message "checkpoint_ack" timeout: 30)))
      (hash-ref ack 'checkpoint_id))))

;;; Log operation to WAL
;;; 
;;; Logs operation before execution for durability.
;;; 
;;; Example:
;;;   (elixir-wal-log! 'memory-add (hash 'content "some text"))
(def (elixir-wal-log! operation data)
  (elixir-send "wal_entry" 
               (hash 'operation (symbol->string operation)
                     'data data
                     'timestamp (time->seconds (current-time)))))

;;; Start heartbeat thread
;;; 
;;; Sends periodic heartbeat to Elixir to indicate liveness.
;;; Should be called once during agent initialization.
(def (start-heartbeat-thread!)
  (unless *heartbeat-thread*
    (set! *heartbeat-thread*
          (spawn
           (lambda ()
             (let loop ()
               (try
                (elixir-send "heartbeat" #t)
                (catch (e)
                  (displayln "Heartbeat error: " e)))
               (thread-sleep! 1)  ; 1 second interval
               (loop)))))))

;;; Helper: Write 32-bit unsigned integer (big-endian)
(def (write-u32 n port)
  (let ((bytes (make-u8vector 4)))
    (u8vector-set! bytes 0 (bitwise-and (arithmetic-shift n -24) #xff))
    (u8vector-set! bytes 1 (bitwise-and (arithmetic-shift n -16) #xff))
    (u8vector-set! bytes 2 (bitwise-and (arithmetic-shift n -8) #xff))
    (u8vector-set! bytes 3 (bitwise-and n #xff))
    (write-u8vector bytes port)))

;;; Helper: Read 32-bit unsigned integer (big-endian)
(def (read-u32 port)
  (let ((bytes (read-u8vector 4 port)))
    (when (and bytes (= (u8vector-length bytes) 4))
      (+ (arithmetic-shift (u8vector-ref bytes 0) 24)
         (arithmetic-shift (u8vector-ref bytes 1) 16)
         (arithmetic-shift (u8vector-ref bytes 2) 8)
         (u8vector-ref bytes 3)))))

;;; Helper: Serialize agent state
;;; 
;;; Converts agent structure to serializable format.
;;; Override this in your agent implementation.
(def (serialize-agent-state agent)
  (hash 'id (agent-id agent)
        'name (agent-name agent)
        'version (agent-version agent)
        'state (agent-state agent)
        'memory (serialize-memory (agent-memory agent))
        'tools (serialize-tools (agent-tools agent))))

;;; Helper: Serialize memory (placeholder)
(def (serialize-memory memory)
  ;; TODO: Implement actual memory serialization
  (hash 'blocks '()
        'count 0))

;;; Helper: Serialize tools (placeholder)
(def (serialize-tools tools)
  ;; TODO: Implement actual tools serialization
  (hash 'registered '()
        'count 0))

;;; Example usage:
;;;
;;; (import :agent/elixir-bridge)
;;;
;;; ;; Start heartbeat
;;; (start-heartbeat-thread!)
;;;
;;; ;; Create checkpoint
;;; (let ((checkpoint-id (elixir-checkpoint! my-agent)))
;;;   (displayln "Checkpoint created: " checkpoint-id))
;;;
;;; ;; Log operation
;;; (elixir-wal-log! 'memory-add (hash 'content "Hello, world!"))
;;;
;;; ;; Send custom message
;;; (elixir-send "metrics" (hash 'latency 10.5 'errors 0))
