;;; message/stream.ss - Message Streaming
;;;
;;; Server-Sent Events (SSE) implementation for streaming message responses.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/net/httpd
  :std/text/json
  :o/database/client
  :o/database/messages
  :o/message/types
  :o/message/manager)

;;; ============================================================================
;;; Stream State Management
;;; ============================================================================

(defstruct message-stream
  (id              ; Stream ID
   agent-id        ; Agent ID
   manager         ; Message manager
   active?         ; Is stream active?
   buffer          ; Chunk buffer
   buffer-size     ; Maximum buffer size
   on-chunk        ; Chunk callback
   on-complete     ; Completion callback
   on-error)       ; Error callback
  transparent: #t)

(def *active-streams* (hash))

(def (make-stream agent-id manager
                   #!key
                   (buffer-size 10)
                   (on-chunk #f)
                   (on-complete #f)
                   (on-error #f))
  "Create a new message stream"
  (let* ((stream-id (uuid-generate))
         (stream (make-message-stream
                  id: stream-id
                  agent-id: agent-id
                  manager: manager
                  active?: #t
                  buffer: '()
                  buffer-size: buffer-size
                  on-chunk: on-chunk
                  on-complete: on-complete
                  on-error: on-error)))

    ;; Register stream
    (hash-put! *active-streams* stream-id stream)

    stream))

(def (stream-active? stream)
  "Check if stream is active"
  (message-stream-active? stream))

(def (stream-close! stream)
  "Close stream"
  (set! (message-stream-active? stream) #f)
  (hash-remove! *active-streams* (message-stream-id stream)))

(def (get-stream stream-id)
  "Get stream by ID"
  (hash-ref *active-streams* stream-id #f))

;;; ============================================================================
;;; Server-Sent Events (SSE)
;;; ============================================================================

(def (sse-format-event data #!key (event #f) (id #f) (retry #f))
  "Format data as SSE event"
  (let ((lines '()))

    ;; Add event type
    (when event
      (set! lines (cons (format "event: ~a\n" event) lines)))

    ;; Add event ID
    (when id
      (set! lines (cons (format "id: ~a\n" id) lines)))

    ;; Add retry interval
    (when retry
      (set! lines (cons (format "retry: ~a\n" retry) lines)))

    ;; Add data (can be multiple lines)
    (if (string? data)
        (set! lines (cons (format "data: ~a\n" data) lines))
        (set! lines (cons (format "data: ~a\n" (json-object->string data)) lines)))

    ;; Add blank line to terminate event
    (set! lines (cons "\n" lines))

    ;; Return formatted event
    (string-join (reverse lines) "")))

(def (sse-send-chunk stream chunk)
  "Send chunk via SSE"
  (when (stream-active? stream)
    (let ((event (sse-format-event
                  (hash 'type "chunk"
                        'content chunk
                        'stream_id (message-stream-id stream))
                  event: "message"
                  id: (uuid-generate))))

      ;; Call chunk callback if provided
      (when (message-stream-on-chunk stream)
        ((message-stream-on-chunk stream) chunk))

      event)))

(def (sse-send-complete stream)
  "Send completion event via SSE"
  (let ((event (sse-format-event
                (hash 'type "complete"
                      'stream_id (message-stream-id stream))
                event: "complete")))

    ;; Call completion callback if provided
    (when (message-stream-on-complete stream)
      ((message-stream-on-complete stream)))

    ;; Close stream
    (stream-close! stream)

    event))

(def (sse-send-error stream error-msg)
  "Send error event via SSE"
  (let ((event (sse-format-event
                (hash 'type "error"
                      'error error-msg
                      'stream_id (message-stream-id stream))
                event: "error")))

    ;; Call error callback if provided
    (when (message-stream-on-error stream)
      ((message-stream-on-error stream) error-msg))

    ;; Close stream
    (stream-close! stream)

    event))

;;; ============================================================================
;;; Chunk Buffering
;;; ============================================================================

(def (stream-buffer-chunk! stream chunk)
  "Add chunk to buffer"
  (let ((buffer (message-stream-buffer stream)))
    (set! (message-stream-buffer stream) (cons chunk buffer))

    ;; Flush buffer if full
    (when (>= (length (message-stream-buffer stream))
              (message-stream-buffer-size stream))
      (stream-flush-buffer! stream))))

(def (stream-flush-buffer! stream)
  "Flush buffered chunks"
  (let ((buffer (reverse (message-stream-buffer stream))))
    (unless (null? buffer)
      ;; Combine chunks
      (let ((combined (string-join buffer "")))
        ;; Send combined chunk
        (sse-send-chunk stream combined))

      ;; Clear buffer
      (set! (message-stream-buffer stream) '()))))

(def (stream-get-buffer stream)
  "Get current buffer contents"
  (reverse (message-stream-buffer stream)))

(def (stream-clear-buffer! stream)
  "Clear buffer without flushing"
  (set! (message-stream-buffer stream) '()))

;;; ============================================================================
;;; Stream Lifecycle
;;; ============================================================================

(def (stream-start! stream)
  "Start streaming"
  (set! (message-stream-active? stream) #t)
  (displayln (format "Stream started: ~a" (message-stream-id stream))))

(def (stream-pause! stream)
  "Pause streaming"
  (set! (message-stream-active? stream) #f)
  (displayln (format "Stream paused: ~a" (message-stream-id stream))))

(def (stream-resume! stream)
  "Resume streaming"
  (set! (message-stream-active? stream) #t)
  (displayln (format "Stream resumed: ~a" (message-stream-id stream))))

(def (stream-abort! stream (reason "Aborted by user"))
  "Abort streaming"
  (sse-send-error stream reason)
  (displayln (format "Stream aborted: ~a - ~a" (message-stream-id stream) reason)))

;;; ============================================================================
;;; Message Streaming
;;; ============================================================================

(def (stream-create-message! stream role content
                             #!key
                             (tool-calls #f)
                             (tool-call-id #f)
                             (tool-name #f)
                             (prompt-tokens 0)
                             (completion-tokens 0))
  "Create message and send via stream"
  (try
   (let ((msg (manager-create-message! (message-stream-manager stream)
                                       role content
                                       tool-calls: tool-calls
                                       tool-call-id: tool-call-id
                                       tool-name: tool-name
                                       prompt-tokens: prompt-tokens
                                       completion-tokens: completion-tokens)))

     ;; Send message via stream
     (sse-send-chunk stream (hash 'type "message"
                                   'message msg))

     msg)
   (catch (e)
     (stream-abort! stream (error-message e))
     (raise e))))

(def (stream-send-chunk! stream content)
  "Send content chunk via stream"
  (when (stream-active? stream)
    (stream-buffer-chunk! stream content)))

(def (stream-send-chunks! stream chunks)
  "Send multiple chunks via stream"
  (for-each (lambda (chunk)
              (stream-send-chunk! stream chunk))
            chunks))

(def (stream-complete! stream (final-message #f))
  "Complete streaming"
  (when (stream-active? stream)
    ;; Flush any remaining buffered chunks
    (stream-flush-buffer! stream)

    ;; Send final message if provided
    (when final-message
      (sse-send-chunk stream (hash 'type "final"
                                    'message final-message)))

    ;; Send completion event
    (sse-send-complete stream)))

;;; ============================================================================
;;; LLM Streaming Integration
;;; ============================================================================

(def (stream-llm-response stream llm-chunks
                          #!key
                          (prompt-tokens 0)
                          (completion-tokens 0))
  "Stream LLM response chunks"
  (let ((accumulated ""))

    ;; Process each chunk
    (for-each
     (lambda (chunk)
       (when (stream-active? stream)
         ;; Accumulate content
         (set! accumulated (string-append accumulated chunk))

         ;; Send chunk
         (stream-send-chunk! stream chunk)))
     llm-chunks)

    ;; Flush buffer
    (stream-flush-buffer! stream)

    ;; Create assistant message with accumulated content
    (when (stream-active? stream)
      (stream-create-message! stream "assistant" accumulated
                             prompt-tokens: prompt-tokens
                             completion-tokens: completion-tokens))

    accumulated))

(def (stream-llm-with-tools stream llm-chunks tool-calls
                            #!key
                            (prompt-tokens 0)
                            (completion-tokens 0))
  "Stream LLM response with tool calls"
  (let ((accumulated ""))

    ;; Process content chunks
    (for-each
     (lambda (chunk)
       (when (stream-active? stream)
         (set! accumulated (string-append accumulated chunk))
         (stream-send-chunk! stream chunk)))
     llm-chunks)

    ;; Flush buffer
    (stream-flush-buffer! stream)

    ;; Send tool calls
    (when (and (stream-active? stream) tool-calls)
      (sse-send-chunk stream (hash 'type "tool_calls"
                                    'tool_calls tool-calls)))

    ;; Create assistant message with tool calls
    (when (stream-active? stream)
      (stream-create-message! stream "assistant" accumulated
                             tool-calls: tool-calls
                             prompt-tokens: prompt-tokens
                             completion-tokens: completion-tokens))

    accumulated))

;;; ============================================================================
;;; HTTP SSE Handler
;;; ============================================================================

(def (sse-response-headers)
  "Get SSE response headers"
  (hash
   "Content-Type" "text/event-stream"
   "Cache-Control" "no-cache"
   "Connection" "keep-alive"
   "X-Accel-Buffering" "no"))

(def (create-sse-handler stream-fn)
  "Create SSE HTTP handler
   stream-fn: (lambda (stream) ...) - Function that uses stream"
  (lambda (req)
    (try
     ;; Create stream
     (let* ((agent-id (hash-ref (http-request-params req) 'agent_id))
            (manager (make-manager agent-id))
            (stream (make-stream agent-id manager)))

       ;; Set response headers
       (let ((response (make-http-response
                        status: 200
                        headers: (sse-response-headers)
                        body: "")))

         ;; Start streaming
         (stream-start! stream)

         ;; Call stream function
         (try
          (stream-fn stream)
          (catch (e)
            (stream-abort! stream (error-message e))))

         response))
     (catch (e)
       (make-http-response
        status: 500
        headers: (hash "Content-Type" "application/json")
        body: (json-object->string
               (hash 'error (error-message e))))))))

;;; ============================================================================
;;; Stream Utilities
;;; ============================================================================

(def (stream-get-stats stream)
  "Get stream statistics"
  (hash
   'stream_id (message-stream-id stream)
   'agent_id (message-stream-agent-id stream)
   'active (message-stream-active? stream)
   'buffer_size (length (message-stream-buffer stream))
   'max_buffer_size (message-stream-buffer-size stream)))

(def (get-active-streams)
  "Get all active streams"
  (hash-values *active-streams*))

(def (count-active-streams)
  "Count active streams"
  (hash-length *active-streams*))

(def (close-all-streams!)
  "Close all active streams"
  (for-each stream-close! (get-active-streams)))

(def (cleanup-inactive-streams!)
  "Remove inactive streams from registry"
  (let ((inactive '()))
    (hash-for-each
     (lambda (id stream)
       (unless (stream-active? stream)
         (set! inactive (cons id inactive))))
     *active-streams*)

    (for-each
     (lambda (id)
       (hash-remove! *active-streams* id))
     inactive)

    (length inactive)))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(def (uuid-generate)
  "Generate UUID (placeholder - use proper UUID library)"
  (format "~a-~a-~a-~a-~a"
          (random-integer 65536)
          (random-integer 65536)
          (random-integer 65536)
          (random-integer 65536)
          (random-integer 65536)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Connect to database
(db-connect!)

;; Create message manager
(def manager (make-manager agent-id))

;; Create stream
(def stream (make-stream agent-id manager
                         buffer-size: 10
                         on-chunk: (lambda (chunk)
                                    (displayln (format "Chunk: ~a" chunk)))
                         on-complete: (lambda ()
                                       (displayln "Stream complete"))
                         on-error: (lambda (error)
                                    (displayln (format "Error: ~a" error)))))

;; Start streaming
(stream-start! stream)

;; Send chunks
(stream-send-chunk! stream "Hello ")
(stream-send-chunk! stream "world!")

;; Flush buffer
(stream-flush-buffer! stream)

;; Complete stream
(stream-complete! stream)

;; SSE Example
(def sse-event (sse-format-event
                (hash 'message "Hello")
                event: "message"
                id: "123"))
(displayln sse-event)

;; LLM Streaming Example
(def llm-chunks (list "Hello" " " "world" "!"))
(def result (stream-llm-response stream llm-chunks
                                 prompt-tokens: 10
                                 completion-tokens: 15))

;; HTTP Handler Example
(def handler (create-sse-handler
              (lambda (stream)
                (stream-send-chunk! stream "Starting...")
                (stream-send-chunk! stream "Processing...")
                (stream-complete! stream))))
|#

