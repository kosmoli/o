#lang racket

;;; elixir-bridge.rkt - Bridge for communication with Elixir supervisor
;;;
;;; This module provides functions for bidirectional communication
;;; between Racket and the Elixir supervision layer via MessagePack
;;; over stdin/stdout.

(provide elixir-send
         elixir-receive
         elixir-send-request
         elixir-connect!
         elixir-disconnect!
         elixir-checkpoint!
         elixir-wal-log!
         start-heartbeat-thread!
         stop-heartbeat-thread!
         wait-for-message
         serialize-agent-state
         serialize-memory
         serialize-tools
         elixir-connected?
         *elixir-port*
         *elixir-input*
         *message-queue*)

(require racket/format
        racket/match
        racket/bool
        racket/function
        racket/async-channel
        racket/port
        racket/bytes
        racket/date
        json)

;;; ============================================================================
;;; Global State
;;; ============================================================================

(define *elixir-port* (current-output-port))
(define *elixir-input* (current-input-port))
(define *message-queue* (make-async-channel))
(define *heartbeat-thread* #f)
(define *elixir-connected* #f)

;;; ============================================================================
;;; Connection Management
;;; ============================================================================

(define (elixir-connected?)
  "Check if connected to Elixir backend"
  *elixir-connected*)

(define (elixir-connect!
         #:input-port [in-port (current-input-port)]
         #:output-port [out-port (current-output-port)]
         #:use-msgpack? [use-msgpack? #f])
  "Connect to Elixir backend.
   For now, uses JSON encoding instead of MessagePack (Racket limitation)."
  (set! *elixir-input* in-port)
  (set! *elixir-port* out-port)
  (set! *elixir-connected* #t)
  (displayln "Elixir bridge connected")
  ;; Start heartbeat automatically
  (start-heartbeat-thread!)
  (void))

(define (elixir-disconnect!)
  "Disconnect from Elixir backend"
  (stop-heartbeat-thread!)
  (set! *elixir-connected* #f)
  (displayln "Elixir bridge disconnected")
  (void))

;;; ============================================================================
;;; Message Sending
;;; ============================================================================

(define (elixir-send msg-type data)
  "Send message to Elixir.
   Format: [4-byte length][JSON payload]
   Example: (elixir-send 'heartbeat #t)
            (elixir-send 'checkpoint (hash 'state state-data))"
  (define msg
    (hash 'type (if (symbol? msg-type)
                    (symbol->string msg-type)
                    msg-type)
          'data data
          'timestamp (current-seconds)))

  (define json-str (jsexpr->string msg))
  (define json-bytes (string->bytes/utf-8 json-str))
  (define len (bytes-length json-bytes))

  ;; Write 4-byte length prefix (big-endian)
  (write-u32 len *elixir-port*)
  ;; Write JSON payload
  (write-bytes json-bytes *elixir-port*)
  ;; Flush immediately
  (flush-output *elixir-port*)
  (void))

(define (elixir-send-request request)
  "Send request to Elixir and wait for response.
   Returns response hash or #f on error."
  (define request-id (format "req-~a" (random 1000000)))
  (define msg
    (hash 'type "request"
          'request_id request-id
          'data request
          'timestamp (current-seconds)))

  ;; Send request
  (define json-str (jsexpr->string msg))
  (define json-bytes (string->bytes/utf-8 json-str))
  (write-u32 (bytes-length json-bytes) *elixir-port*)
  (write-bytes json-bytes *elixir-port*)
  (flush-output *elixir-port*)

  ;; Wait for response (with timeout)
  (define response (wait-for-message "response" #:timeout 5))
  response)

;;; ============================================================================
;;; Message Receiving
;;; ============================================================================

(define (elixir-receive)
  "Receive message from Elixir.
   Blocks until a message is available.
   Returns decoded message hash or #f on error."
  (with-handlers* ([exn:fail?
                    (lambda (e)
                      (displayln (format "Error receiving message: ~a" (exn-message e)))
                      #f)])
    (define len (read-u32 *elixir-input*))
    (when len
      (define data-bytes (read-bytes len *elixir-input*))
      (define json-str (bytes->string/utf-8 data-bytes))
      (string->jsexpr json-str))))

(define (wait-for-message msg-type
                         #:timeout [timeout-seconds 30])
  "Wait for specific message type with timeout.
   Example: (wait-for-message 'checkpoint_ack #:timeout 30)"
  (define deadline (+ (current-seconds) timeout-seconds))

  (define (loop)
    (cond
     [(> (current-seconds) deadline)
      (error 'wait-for-message "Timeout waiting for message"
             'type msg-type
             'timeout timeout-seconds)]
     [else
      (define msg (elixir-receive))
      (cond
       [(not msg)
        (sleep 0.1)
        (loop)]
       [(equal? (hash-ref msg 'type #f)
                (if (symbol? msg-type)
                    (symbol->string msg-type)
                    msg-type))
        msg]
       [else
        ;; Put other messages in queue for async processing
        (async-channel-put *message-queue* msg)
        (sleep 0.01)
        (loop)])]))

  (loop))

;;; ============================================================================
;;; Checkpoint and WAL
;;; ============================================================================

(define (elixir-checkpoint! agent)
  "Create checkpoint.
   Serializes agent state and sends to Elixir for persistence.
   Returns checkpoint ID."
  (define snapshot (serialize-agent-state agent))

  ;; Send checkpoint request
  (elixir-send "checkpoint" snapshot)

  ;; Wait for acknowledgment
  (define ack (wait-for-message "checkpoint_ack" #:timeout 30))
  (hash-ref ack 'checkpoint_id #f))

(define (elixir-wal-log! operation data)
  "Log operation to WAL.
   Logs operation before execution for durability.
   Example: (elixir-wal-log! 'memory-add (hash 'content \"some text\"))"
  (elixir-send "wal_entry"
               (hash 'operation (if (symbol? operation)
                                   (symbol->string operation)
                                   operation)
                     'data data
                     'timestamp (current-seconds)))
  (void))

;;; ============================================================================
;;; Heartbeat Thread
;;; ============================================================================

(define (start-heartbeat-thread!
         #:interval [interval-seconds 1])
  "Start heartbeat thread.
   Sends periodic heartbeat to Elixir to indicate liveness.
   Should be called once during agent initialization."
  (unless *heartbeat-thread*
    (set! *heartbeat-thread*
          (thread
           (lambda ()
             (let loop ()
               (when *elixir-connected*
                 (with-handlers* ([exn:fail?
                                   (lambda (e)
                                     (displayln (format "Heartbeat error: ~a"
                                                      (exn-message e))))])
                   (elixir-send "heartbeat" #t))
                 (sleep interval-seconds)
                 (loop))))))
    (void)))

(define (stop-heartbeat-thread!)
  "Stop heartbeat thread"
  (when *heartbeat-thread*
    (kill-thread *heartbeat-thread*)
    (set! *heartbeat-thread* #f))
  (void))

;;; ============================================================================
;;; Byte Encoding Helpers
;;; ============================================================================

(define (write-u32 n port)
  "Write 32-bit unsigned integer (big-endian)"
  (define bytes
    (bytes (bitwise-and (arithmetic-shift n -24) #xFF)
           (bitwise-and (arithmetic-shift n -16) #xFF)
           (bitwise-and (arithmetic-shift n -8) #xFF)
           (bitwise-and n #xFF)))
  (write-bytes bytes port)
  (void))

(define (read-u32 port)
  "Read 32-bit unsigned integer (big-endian)"
  (define bytes (read-bytes 4 port))
  (when (and bytes (= (bytes-length bytes) 4))
    (+ (arithmetic-shift (bytes-ref bytes 0) 24)
       (arithmetic-shift (bytes-ref bytes 1) 16)
       (arithmetic-shift (bytes-ref bytes 2) 8)
       (bytes-ref bytes 3))))

(define (write-bytes bytes port)
  "Write bytes to port"
  (for ([b (in-bytes bytes)])
    (write-byte b port))
  (void))

;;; ============================================================================
;;; Serialization Helpers
;;; ============================================================================

(define (serialize-agent-state agent)
  "Serialize agent state to hash.
   Converts agent structure to serializable format.
   Generic implementation - override for specific agent types."
  (cond
   [(hash? agent)
    ;; If agent is already a hash, add metadata
    (hash-set (hash-set agent 'serialized_at (current-seconds))
              'type 'agent)]
   [(procedure? agent)
    ;; If agent is a procedure/closure, store minimal info
    (hash 'type 'agent
          'procedure #t
          'serialized_at (current-seconds))]
   [else
    ;; Generic serialization
    (hash 'type 'agent
          'data (format "~a" agent)
          'serialized_at (current-seconds))]))

(define (agent-state->hash state)
  "Convert agent state to hash for serialization"
  (cond
   [(hash? state) state]
   [else (hash 'state (format "~a" state))]))

(define (serialize-memory memory)
  "Serialize memory to hash.
   TODO: Implement actual memory serialization with blocks"
  (cond
   [(hash? memory)
    (hash-set memory 'type 'memory)]
   [else
    (hash 'type 'memory
          'data (format "~a" memory))]))

(define (memory-block->hash block)
  "Convert memory block to hash"
  (cond
   [(hash? block) block]
   [else (hash 'content (format "~a" block))]))

(define (serialize-tools tools)
  "Serialize tools registry to hash.
   TODO: Implement actual tools serialization"
  (if (and tools (hash? tools))
      (hash 'registered (hash-keys tools)
            'count (hash-count tools))
      (hash 'registered '()
            'count 0)))

;;; ============================================================================
;;; Queue Management
;;; ============================================================================

(define (get-queued-messages)
  "Get all queued messages from async channel"
  (define msgs '())
  (let loop ()
    (define msg (async-channel-try-get *message-queue*))
    (when msg
      (set! msgs (cons msg msgs))
      (loop)))
  (reverse msgs))

(define (process-queued-messages processor)
  "Process all queued messages with given processor function"
  (for ([msg (in-list (get-queued-messages))])
    (processor msg))
  (void))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Example 1: Connect and start heartbeat
(elixir-connect!)

;; Example 2: Create checkpoint
(define checkpoint-id (elixir-checkpoint! my-agent))
(displayln (format "Checkpoint created: ~a" checkpoint-id))

;; Example 3: Log operation
(elixir-wal-log! 'memory-add (hash 'content "Hello, world!"))

;; Example 4: Send custom message
(elixir-send "metrics" (hash 'latency 10.5 'errors 0))

;; Example 5: Send request and wait for response
(define response (elixir-send-request (hash 'action "get_status")))

;; Example 6: Process queued messages
(process-queued-messages
 (lambda (msg)
   (displayln (format "Got message: ~a" msg))))
|#
