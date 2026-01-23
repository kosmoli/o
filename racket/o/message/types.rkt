#lang racket

;;; message/types.rkt - Message Type Definitions
;;;
;;; Type definitions for the message management system.

(provide (struct-out message)
         (struct-out message-stats)
         (struct-out conversation-summary)
         message-role-user
         message-role-assistant
         message-role-system
         message-role-tool
         valid-message-roles
         make-user-message
         make-assistant-message
         make-system-message
         make-tool-message
         message-role?
         valid-message?
         message->hash
         hash->message
         calculate-message-tokens
         trim-conversation-by-tokens
         filter-messages-by-role)

(require racket/hash
        racket/match
        racket/format
        racket/date)

;;; ============================================================================
;;; Message Structure
;;; ============================================================================

(struct message
  (id              ; Unique message ID (string)
   agent-id        ; Agent ID (string)
   role            ; Message role (symbol: 'user, 'assistant, 'system, 'tool)
   content         ; Message content (string)
   tool-calls      ; Optional tool calls (list of hashes)
   tool-call-id    ; Optional tool call ID (for tool responses)
   tokens          ; Token count (integer)
   timestamp       ; Message timestamp (seconds)
   metadata)       ; Additional metadata (hash)
  #:transparent)

;;; ============================================================================
;;; Message Roles
;;; ============================================================================

(define message-role-user 'user)
(define message-role-assistant 'assistant)
(define message-role-system 'system)
(define message-role-tool 'tool)

(define valid-message-roles
  '(user assistant system tool))

;;; ============================================================================
;;; Constructors
;;; ============================================================================

(define (make-user-message agent-id content)
  "Create a user message"
  (message
   (uuid-generate)
   agent-id
   message-role-user
   content
   #f
   #f
   (estimate-tokens content)
   (current-seconds)
   (hash)))

(define (make-assistant-message agent-id content #:tool-calls [tool-calls #f])
  "Create an assistant message"
  (message
   (uuid-generate)
   agent-id
   message-role-assistant
   content
   tool-calls
   #f
   (estimate-tokens content)
   (current-seconds)
   (hash)))

(define (make-system-message agent-id content)
  "Create a system message"
  (message
   (uuid-generate)
   agent-id
   message-role-system
   content
   #f
   #f
   (estimate-tokens content)
   (current-seconds)
   (hash)))

(define (make-tool-message agent-id content tool-call-id)
  "Create a tool response message"
  (message
   (uuid-generate)
   agent-id
   message-role-tool
   content
   #f
   tool-call-id
   (estimate-tokens content)
   (current-seconds)
   (hash)))

;;; ============================================================================
;;; Predicates
;;; ============================================================================

(define (message-role? role)
  "Check if role is valid"
  (member role valid-message-roles))

(define (valid-message? msg)
  "Check if message is valid"
  (and (message? msg)
       (message-role? (message-role msg))
       (string? (message-content msg))
       (> (string-length (message-content msg)) 0)))

;;; ============================================================================
;;; Conversion Functions
;;; ============================================================================

(define (message->hash msg)
  "Convert message to hash"
  (define base
    (hash 'id (message-id msg)
          'agent_id (message-agent-id msg)
          'role (symbol->string (message-role msg))
          'content (message-content msg)
          'tokens (message-tokens msg)
          'timestamp (message-timestamp msg)
          'metadata (message-metadata msg)))

  (define with-tool-calls
    (if (message-tool-calls msg)
        (hash-set base 'tool_calls (message-tool-calls msg))
        base))

  (define with-tool-call-id
    (if (message-tool-call-id msg)
        (hash-set with-tool-calls 'tool_call_id (message-tool-call-id msg))
        with-tool-calls))

  with-tool-call-id)

(define (hash->message h)
  "Convert hash to message"
  (message
   (hash-ref h 'id)
   (hash-ref h 'agent_id)
   (string->symbol (hash-ref h 'role))
   (hash-ref h 'content)
   (hash-ref h 'tool_calls #f)
   (hash-ref h 'tool_call_id #f)
   (hash-ref h 'tokens 0)
   (hash-ref h 'timestamp (current-seconds))
   (hash-ref h 'metadata (hash))))

;;; ============================================================================
;;; Message Statistics
;;; ============================================================================

(struct message-stats
  (total-messages     ; Total message count
   user-messages      ; User message count
   assistant-messages ; Assistant message count
   system-messages    ; System message count
   tool-messages      ; Tool message count
   total-tokens       ; Total token count
   avg-tokens-per-message) ; Average tokens per message
  #:transparent)

;;; ============================================================================
;;; Conversation Summary
;;; ============================================================================

(struct conversation-summary
  (agent-id          ; Agent ID
   message-count     ; Number of messages
   total-tokens      ; Total tokens
   start-time        ; Conversation start time
   end-time          ; Conversation end time
   last-message      ; Last message content
   metadata)         ; Additional metadata
  #:transparent)

;;; ============================================================================
;;; Token Utilities
;;; ============================================================================

(define (estimate-tokens text)
  "Estimate token count (rough approximation: ~4 chars per token)"
  (if (string? text)
      (quotient (string-length text) 4)
      0))

(define (calculate-message-tokens msg)
  "Calculate message tokens"
  (message-tokens msg))

(define (uuid-generate)
  "Generate UUID (simple implementation)"
  (format "~a-~a" (current-seconds) (random 1000000)))

;;; ============================================================================
;;; Conversation Utilities
;;; ============================================================================

(define (trim-conversation-by-tokens messages max-tokens)
  "Trim conversation to fit within max tokens
   Removes oldest messages first, keeps system messages"
  (define (keep-system? msg)
    (eq? (message-role msg) message-role-system))

  (define system-msgs
    (filter keep-system? messages))

  (define non-system-msgs
    (filter (negate keep-system?) messages))

  (define (trim-helper msgs token-count)
    (cond
     [(<= token-count max-tokens) (reverse msgs)]
     [(null? msgs) '()]
     [else
      (define tokens (message-tokens (car msgs)))
      (trim-helper (cdr msgs) (- token-count tokens))]))

  (define current-tokens
    (apply + (map message-tokens non-system-msgs)))

  (define trimmed
    (trim-helper (reverse non-system-msgs) current-tokens))

  (append system-msgs trimmed))

(define (filter-messages-by-role messages role)
  "Filter messages by role"
  (filter (lambda (msg)
            (eq? (message-role msg) role))
          messages))
