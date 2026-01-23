#lang racket

;;; message/stream.rkt - Message Streaming
;;;
;;; Server-Sent Events (SSE) implementation for streaming message responses.

(provide (struct-out message-stream)
         make-stream
         stream-active?
         stream-close!
         get-stream
         sse-format-event
         sse-send-chunk
         sse-send-complete
         sse-send-error
         stream-buffer-chunk!
         stream-flush-buffer!
         stream-get-buffer
         stream-clear-buffer!
         stream-start!
         stream-pause!
         stream-resume!
         stream-abort!
         stream-send-chunk!
         stream-send-chunks!
         stream-complete!
         stream-llm-response
         stream-llm-with-tools
         sse-response-headers
         create-sse-handler
         stream-get-stats
         get-active-streams
         count-active-streams
         close-all-streams!
         cleanup-inactive-streams!)

(require racket/hash
        racket/format
        json
        "./types.rkt"
        "./manager.rkt")

;;; ============================================================================
;;; Stream State Management
;;; ============================================================================

(define *active-streams* (make-hash))

(struct message-stream
  (id
   agent-id
   manager
   active?
   buffer
   buffer-size
   on-chunk
   on-complete
   on-error)
  #:mutable
  #:transparent)

(define (make-stream agent-id manager
                     #:buffer-size [buffer-size 10]
                     #:on-chunk [on-chunk #f]
                     #:on-complete [on-complete #f]
                     #:on-error [on-error #f])
  "Create a new message stream"
  (define stream-id (uuid-generate))
  (define stream (message-stream
                  stream-id
                  agent-id
                  manager
                  #t
                  '()
                  buffer-size
                  on-chunk
                  on-complete
                  on-error))

  (hash-set! *active-streams* stream-id stream)
  stream)

(define (stream-active? stream)
  "Check if stream is active"
  (message-stream-active? stream))

(define (stream-close! stream)
  "Close stream"
  (set-message-stream-active?! stream #f)
  (hash-remove! *active-streams* (message-stream-id stream)))

(define (get-stream stream-id)
  "Get stream by ID"
  (hash-ref *active-streams* stream-id #f))

;;; ============================================================================
;;; Server-Sent Events (SSE)
;;; ============================================================================

(define (sse-format-event data #:event [event #f] #:id [id #f] #:retry [retry #f])
  "Format data as SSE event"
  (define lines '())

  (define result-lines
    (cond
     [event
      (cons (format "event: ~a\n" event) lines)]
     [else lines]))

  (define result-lines2
    (cond
     [id
      (cons (format "id: ~a\n" id) result-lines)]
     [else result-lines]))

  (define result-lines3
    (cond
     [retry
      (cons (format "retry: ~a\n" retry) result-lines2)]
     [else result-lines2]))

  (define data-line
    (if (string? data)
        (format "data: ~a\n" data)
        (format "data: ~a\n" (jsexpr->string data))))

  (define all-lines
    (cons "\n" (cons data-line result-lines3)))

  (apply string-append (reverse all-lines)))

(define (sse-send-chunk stream chunk)
  "Send chunk via SSE"
  (when (stream-active? stream)
    (define event
      (sse-format-event
       (hash 'type "chunk"
             'content chunk
             'stream_id (message-stream-id stream))
       #:event "message"
       #:id (uuid-generate)))

    (when (message-stream-on-chunk stream)
      ((message-stream-on-chunk stream) chunk))

    event))

(define (sse-send-complete stream)
  "Send completion event via SSE"
  (define event
    (sse-format-event
     (hash 'type "complete"
           'stream_id (message-stream-id stream))
     #:event "complete"))

  (when (message-stream-on-complete stream)
    ((message-stream-on-complete stream)))

  (stream-close! stream)
  event)

(define (sse-send-error stream error-msg)
  "Send error event via SSE"
  (define event
    (sse-format-event
     (hash 'type "error"
           'error error-msg
           'stream_id (message-stream-id stream))
     #:event "error"))

  (when (message-stream-on-error stream)
    ((message-stream-on-error stream) error-msg))

  (stream-close! stream)
  event)

;;; ============================================================================
;;; Chunk Buffering
;;; ============================================================================

(define (stream-buffer-chunk! stream chunk)
  "Add chunk to buffer"
  (define buffer (message-stream-buffer stream))
  (set-message-stream-buffer! stream (cons chunk buffer))

  (when (>= (length (message-stream-buffer stream))
            (message-stream-buffer-size stream))
    (stream-flush-buffer! stream)))

(define (stream-flush-buffer! stream)
  "Flush buffered chunks"
  (define buffer (reverse (message-stream-buffer stream)))
  (unless (null? buffer)
    (define combined (apply string-append buffer))
    (sse-send-chunk stream combined)
    (set-message-stream-buffer! stream '())))

(define (stream-get-buffer stream)
  "Get current buffer contents"
  (reverse (message-stream-buffer stream)))

(define (stream-clear-buffer! stream)
  "Clear buffer without flushing"
  (set-message-stream-buffer! stream '()))

;;; ============================================================================
;;; Stream Lifecycle
;;; ============================================================================

(define (stream-start! stream)
  "Start streaming"
  (set-message-stream-active?! stream #t)
  (displayln (format "Stream started: ~a" (message-stream-id stream))))

(define (stream-pause! stream)
  "Pause streaming"
  (set-message-stream-active?! stream #f)
  (displayln (format "Stream paused: ~a" (message-stream-id stream))))

(define (stream-resume! stream)
  "Resume streaming"
  (set-message-stream-active?! stream #t)
  (displayln (format "Stream resumed: ~a" (message-stream-id stream))))

(define (stream-abort! stream #:reason [reason "Aborted by user"])
  "Abort streaming"
  (sse-send-error stream reason)
  (displayln (format "Stream aborted: ~a - ~a" (message-stream-id stream) reason)))

;;; ============================================================================
;;; Message Streaming
;;; ============================================================================

(define (stream-create-message! stream role content
                                 #:tool-calls [tool-calls #f]
                                 #:tool-call-id [tool-call-id #f]
                                 #:tool-name [tool-name #f]
                                 #:prompt-tokens [prompt-tokens 0]
                                 #:completion-tokens [completion-tokens 0])
  "Create message and send via stream"
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (stream-abort! stream #:reason (exn-message e))
                     (raise e))])
    (define msg (manager-create-message! (message-stream-manager stream)
                                         role content
                                         #:tool-calls tool-calls
                                         #:tool-call-id tool-call-id
                                         #:tool-name tool-name
                                         #:prompt-tokens prompt-tokens
                                         #:completion-tokens completion-tokens))
    (sse-send-chunk stream (hash 'type "message"
                                  'message msg))
    msg))

(define (stream-send-chunk! stream content)
  "Send content chunk via stream"
  (when (stream-active? stream)
    (stream-buffer-chunk! stream content)))

(define (stream-send-chunks! stream chunks)
  "Send multiple chunks via stream"
  (for-each (lambda (chunk)
              (stream-send-chunk! stream chunk))
            chunks))

(define (stream-complete! stream #:final-message [final-message #f])
  "Complete streaming"
  (when (stream-active? stream)
    (stream-flush-buffer! stream)
    (when final-message
      (sse-send-chunk stream (hash 'type "final"
                                   'message final-message)))
    (sse-send-complete stream)))

;;; ============================================================================
;;; LLM Streaming Integration
;;; ============================================================================

(define (stream-llm-response stream llm-chunks
                            #:prompt-tokens [prompt-tokens 0]
                            #:completion-tokens [completion-tokens 0])
  "Stream LLM response chunks"
  (define accumulated "")

  (for ([chunk llm-chunks])
    (when (stream-active? stream)
      (set! accumulated (string-append accumulated chunk))
      (stream-send-chunk! stream chunk)))

  (stream-flush-buffer! stream)

  (when (stream-active? stream)
    (stream-create-message! stream "assistant" accumulated
                           #:prompt-tokens prompt-tokens
                           #:completion-tokens completion-tokens))

  accumulated)

(define (stream-llm-with-tools stream llm-chunks tool-calls
                               #:prompt-tokens [prompt-tokens 0]
                               #:completion-tokens [completion-tokens 0])
  "Stream LLM response with tool calls"
  (define accumulated "")

  (for ([chunk llm-chunks])
    (when (stream-active? stream)
      (set! accumulated (string-append accumulated chunk))
      (stream-send-chunk! stream chunk)))

  (stream-flush-buffer! stream)

  (when (and (stream-active? stream) tool-calls)
    (sse-send-chunk stream (hash 'type "tool_calls"
                                 'tool_calls tool-calls)))

  (when (stream-active? stream)
    (stream-create-message! stream "assistant" accumulated
                           #:tool-calls tool-calls
                           #:prompt-tokens prompt-tokens
                           #:completion-tokens completion-tokens))

  accumulated)

;;; ============================================================================
;;; HTTP SSE Headers
;;; ============================================================================

(define (sse-response-headers)
  "Get SSE response headers for Server-Sent Events

   Returns:
     Hash table with HTTP headers for SSE"
  (hash "Content-Type" "text/event-stream"
        "Cache-Control" "no-cache"
        "Connection" "keep-alive"
        "X-Accel-Buffering" "no"))

;;; ============================================================================
;;; HTTP SSE Handler
;;; ============================================================================

(define (create-sse-handler stream-fn)
  "Create SSE HTTP handler for streaming responses

   Args:
     stream-fn: Function that takes a stream and performs streaming
               (lambda (stream) ...)

   Returns:
     HTTP request handler function

   Example:
     (define handler (create-sse-handler
                       (lambda (stream)
                         (stream-send-chunk! stream \"Starting...\")
                         (stream-send-chunk! stream \"Processing...\")
                         (stream-complete! stream))))"
  (lambda (req)
    (with-handlers* ([exn:fail?
                      (lambda (e)
                        ;; Return error response
                        (hash 'status 500
                              'headers (hash "Content-Type" "application/json")
                              'body (jsexpr->string
                                     (hash 'error "Streaming failed"
                                           'message (exn-message e)))))])
      ;; Create stream (would get agent-id from request params)
      (define agent-id "default-agent")
      (define manager #f)  ; Would be created from context
      (define stream (make-stream agent-id manager))

      ;; Start streaming
      (stream-start! stream)

      ;; Call stream function with error handling
      (with-handlers* ([exn:fail?
                         (lambda (e)
                           (stream-abort! stream #:reason (exn-message e))
                           (raise e))])
        (stream-fn stream))

      ;; Return SSE response
      (hash 'status 200
            'headers (sse-response-headers)
            'body ""))))

;;; ============================================================================
;;; Stream Utilities
;;; ============================================================================

(define (stream-get-stats stream)
  "Get stream statistics

   Args:
     stream: Message stream

   Returns:
     Hash with stream statistics including:
     - stream_id: Unique stream identifier
     - agent_id: Associated agent ID
     - active: Whether stream is active
     - buffer_size: Current buffer size
     - max_buffer_size: Maximum buffer size"
  (hash 'stream_id (message-stream-id stream)
        'agent_id (message-stream-agent-id stream)
        'active (message-stream-active? stream)
        'buffer_size (length (message-stream-buffer stream))
        'max_buffer_size (message-stream-buffer-size stream)))

(define (get-active-streams)
  "Get all active streams

   Returns:
     List of all active message streams"
  (hash-values *active-streams*))

(define (count-active-streams)
  "Count active streams

   Returns:
     Number of active streams"
  (hash-count *active-streams*))

(define (close-all-streams!)
  "Close all active streams

   This function will close all currently active streams
   and remove them from the active streams registry"
  (for-each stream-close! (get-active-streams)))

(define (cleanup-inactive-streams!)
  "Remove inactive streams from registry

   This function scans the active streams registry and removes
   any streams that are no longer active

   Returns:
     Number of streams cleaned up"
  (define inactive '())
  (for ([(id stream) (in-hash *active-streams*)])
    (unless (stream-active? stream)
      (set! inactive (cons id inactive))))

  (for ([id inactive])
    (hash-remove! *active-streams* id))

  (length inactive))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (uuid-generate)
  "Generate UUID (placeholder implementation)

   Returns:
     A pseudo-random UUID string

   Note: For production use, replace with a proper UUID library
   such as 'uuid' package"
  (format "~a-~a-~a-~a-~a"
          (random 65536)
          (random 65536)
          (random 65536)
          (random 65536)
          (random 65536)))

;;; ============================================================================
;;; Example Usage
;;; ============================================================================
;;;
;;; ;; Create message manager
;;; (define manager (make-manager "agent-123"))
;;;
;;; ;; Create stream with callbacks
;;; (define stream (make-stream "agent-123" manager
;;;                             #:buffer-size 10
;;;                             #:on-chunk (lambda (chunk)
;;;                                          (displayln (format "Chunk: ~a" chunk)))
;;;                             #:on-complete (lambda ()
;;;                                              (displayln "Stream complete"))
;;;                             #:on-error (lambda (error)
;;;                                          (displayln (format "Error: ~a" error)))))
;;;
;;; ;; Start streaming
;;; (stream-start! stream)
;;;
;;; ;; Send chunks
;;; (stream-send-chunk! stream "Hello ")
;;; (stream-send-chunk! stream "world!")
;;;
;;; ;; Flush buffer
;;; (stream-flush-buffer! stream)
;;;
;;; ;; Complete stream
;;; (stream-complete! stream)
;;;
;;; ;; SSE Event Formatting
;;; (define sse-event (sse-format-event
;;;                    (hash 'message "Hello")
;;;                    #:event "message"
;;;                    #:id "123"))
;;; (displayln sse-event)
;;;
;;; ;; LLM Streaming Example
;;; (define llm-chunks (list "Hello" " " "world" "!"))
;;; (define result (stream-llm-response stream llm-chunks
;;;                                     #:prompt-tokens 10
;;;                                     #:completion-tokens 15))
;;;
;;; ;; HTTP Handler Example
;;; (define handler (create-sse-handler
;;;                   (lambda (stream)
;;;                     (stream-send-chunk! stream "Starting...")
;;;                     (stream-send-chunk! stream "Processing...")
;;;                     (stream-complete! stream))))
;;;
;;; ;;; ============================================================================
