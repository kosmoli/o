#lang racket

;;; agent/state.rkt - Agent State Management and Context
;;;
;;; This module manages agent state, context, and execution history.
;;; It provides structures and functions for tracking agent state transitions,
;;; maintaining execution context, and managing conversation history.

(provide (struct-out agent-state-internal)
         (struct-out execution-context-internal)
         (struct-out history-entry)
         (struct-out conversation-message)
         make-agent-state-instance
         make-initial-context
         state-set!
         state-get
         state-has?
         state-delete!
         state-clear!
         context-set!
         context-get
         context-push-stack!
         context-pop-stack!
         context-peek-stack
         create-nested-context
         switch-context!
         restore-parent-context!
         add-history-entry!
         get-history
         get-history-entry
         clear-history!
         add-message!
         get-conversation
         get-conversation-context
         clear-conversation!
         enqueue-action!
         dequeue-action!
         peek-action
         has-pending-actions?
         clear-pending-actions!
         snapshot-state-variables
         snapshot-context
         snapshot-state
         restore-state-from-snapshot
         restore-context-from-snapshot
         conversation-message->hash
         hash->conversation-message
         state-set-metadata!
         state-get-metadata
         state-stats
         validate-state)

(require racket/hash
        racket/format
        racket/list
        racket/date
        "../elixir-bridge.rkt")

;;; ============================================================================
;;; State Structure
;;; ============================================================================

(struct agent-state-internal
  (id                ; Unique state identifier
   context           ; Current execution context
   history           ; Execution history (list of state snapshots)
   variables         ; State variables (hash table)
   conversation      ; Conversation history
   pending-actions   ; Queue of pending actions
   metadata          ; Additional metadata
   created-at        ; Creation timestamp
   updated-at)       ; Last update timestamp
  #:transparent
  #:mutable)

;;; ============================================================================
;;; Context Structure
;;; ============================================================================

(struct execution-context-internal
  (id                ; Context identifier
   parent-id         ; Parent context (for nested contexts)
   type              ; Context type: 'main 'tool 'evolution 'recovery
   input             ; Current input being processed
   output            ; Current output
   variables         ; Context-local variables
   stack             ; Execution stack
   metadata          ; Context metadata
   created-at)       ; Creation timestamp
  #:transparent
  #:mutable)

;;; ============================================================================
;;; History Entry Structure
;;; ============================================================================

(struct history-entry
  (id                ; Entry identifier
   timestamp         ; When this entry was created
   state-snapshot    ; Snapshot of state at this point
   context-snapshot  ; Snapshot of context
   action            ; Action that was taken
   result            ; Result of the action
   metadata)         ; Additional metadata
  #:transparent)

;;; ============================================================================
;;; Conversation Message Structure
;;; ============================================================================

(struct conversation-message
  (id                ; Message identifier
   role              ; Role: 'user 'assistant 'system 'tool
   content           ; Message content
   metadata          ; Additional metadata (tool calls, etc.)
   timestamp)        ; Message timestamp
  #:transparent)

;;; ============================================================================
;;; State Creation
;;; ============================================================================

(define (make-agent-state-instance)
  "Create a new agent state instance"
  (define now (current-seconds))
  (agent-state-internal
   (uuid-generate)
   (make-initial-context)
   '()
   (hash)
   '()
   '()
   (hash)
   now
   now))

(define (make-initial-context)
  "Create initial execution context"
  (execution-context-internal
   (uuid-generate)
   #f
   'main
   #f
   #f
   (hash)
   '()
   (hash)
   (current-seconds)))

;;; ============================================================================
;;; State Variables
;;; ============================================================================

(define (state-set! state key value)
  "Set a state variable"
  (hash-set! (agent-state-internal-variables state) key value)
  (set-agent-state-internal-updated-at! state (current-seconds))

  ;; Log to WAL
  (elixir-wal-log! 'state-set
                   (hash 'state_id (agent-state-internal-id state)
                         'key (symbol->string key)
                         'value value)))

(define (state-get state key #:default [default #f])
  "Get a state variable"
  (hash-ref (agent-state-internal-variables state) key default))

(define (state-has? state key)
  "Check if state has a variable"
  (hash-has-key? (agent-state-internal-variables state) key))

(define (state-delete! state key)
  "Delete a state variable"
  (define vars (agent-state-internal-variables state))
  (set-agent-state-internal-variables! state (hash-remove vars key))
  (set-agent-state-internal-updated-at! state (current-seconds))

  ;; Log to WAL
  (elixir-wal-log! 'state-delete
                   (hash 'state_id (agent-state-internal-id state)
                         'key (symbol->string key))))

(define (state-clear! state)
  "Clear all state variables"
  (set-agent-state-internal-variables! state (hash))
  (set-agent-state-internal-updated-at! state (current-seconds))

  ;; Log to WAL
  (elixir-wal-log! 'state-clear
                   (hash 'state_id (agent-state-internal-id state))))

;;; ============================================================================
;;; Context Management
;;; ============================================================================

(define (context-set! context key value)
  "Set a context variable"
  (define vars (execution-context-internal-variables context))
  (set-execution-context-internal-variables! context (hash-set vars key value)))

(define (context-get context key #:default [default #f])
  "Get a context variable"
  (hash-ref (execution-context-internal-variables context) key default))

(define (context-push-stack! context item)
  "Push item onto execution stack"
  (define stack (execution-context-internal-stack context))
  (set-execution-context-internal-stack! context (cons item stack)))

(define (context-pop-stack! context)
  "Pop item from execution stack"
  (define stack (execution-context-internal-stack context))
  (if (null? stack)
      #f
      (let ([item (car stack)])
        (set-execution-context-internal-stack! context (cdr stack))
        item)))

(define (context-peek-stack context)
  "Peek at top of execution stack"
  (define stack (execution-context-internal-stack context))
  (if (null? stack) #f (car stack)))

;;; ============================================================================
;;; Nested Context Management
;;; ============================================================================

(define (create-nested-context parent-context type)
  "Create a nested execution context"
  (execution-context-internal
   (uuid-generate)
   (execution-context-internal-id parent-context)
   type
   #f
   #f
   (hash)
   '()
   (hash)
   (current-seconds)))

(define (switch-context! state new-context)
  "Switch to a new execution context"
  (define old-context (agent-state-internal-context state))
  ;; Save old context to stack
  (context-push-stack! new-context old-context)

  ;; Switch to new context
  (set-agent-state-internal-context! state new-context)
  (set-agent-state-internal-updated-at! state (current-seconds)))

(define (restore-parent-context! state)
  "Restore parent context from stack"
  (define current-context (agent-state-internal-context state))
  (define parent-context (context-pop-stack! current-context))
  (when parent-context
    (set-agent-state-internal-context! state parent-context)
    (set-agent-state-internal-updated-at! state (current-seconds)))
  parent-context)

;;; ============================================================================
;;; History Management
;;; ============================================================================

(define (add-history-entry! state action result)
  "Add an entry to execution history"
  (define entry
    (history-entry
     (uuid-generate)
     (current-seconds)
     (snapshot-state-variables state)
     (snapshot-context (agent-state-internal-context state))
     action
     result
     (hash)))

  ;; Add to history (keep last 1000 entries)
  (define history (agent-state-internal-history state))
  (set-agent-state-internal-history! state (take (cons entry history) 1000))

  (set-agent-state-internal-updated-at! state (current-seconds))

  ;; Log to WAL
  (elixir-wal-log! 'history-entry
                   (hash 'state_id (agent-state-internal-id state)
                         'entry_id (history-entry-id entry)
                         'action action)))

(define (get-history state #:limit [limit 100])
  "Get execution history (most recent first)"
  (take (agent-state-internal-history state) limit))

(define (get-history-entry state entry-id)
  "Get a specific history entry by ID"
  (for/first ([entry (in-list (agent-state-internal-history state))]
              #:when (equal? (history-entry-id entry) entry-id))
    entry))

(define (clear-history! state)
  "Clear execution history"
  (set-agent-state-internal-history! state '())
  (set-agent-state-internal-updated-at! state (current-seconds)))

;;; ============================================================================
;;; Conversation Management
;;; ============================================================================

(define (add-message! state role content #:metadata [metadata (hash)])
  "Add a message to conversation history"
  (define message
    (conversation-message
     (uuid-generate)
     role
     content
     metadata
     (current-seconds)))

  ;; Add to conversation (keep last 1000 messages)
  (define conv (agent-state-internal-conversation state))
  (set-agent-state-internal-conversation! state (take (cons message conv) 1000))

  (set-agent-state-internal-updated-at! state (current-seconds))

  ;; Log to WAL
  (elixir-wal-log! 'conversation-message
                   (hash 'state_id (agent-state-internal-id state)
                         'message_id (conversation-message-id message)
                         'role (symbol->string role)))

  message)

(define (get-conversation state #:limit [limit 100])
  "Get conversation history (most recent first)"
  (take (agent-state-internal-conversation state) limit))

(define (get-conversation-context state #:limit [limit 10])
  "Get recent conversation for context (oldest first)"
  (reverse (take (agent-state-internal-conversation state) limit)))

(define (clear-conversation! state)
  "Clear conversation history"
  (set-agent-state-internal-conversation! state '())
  (set-agent-state-internal-updated-at! state (current-seconds)))

;;; ============================================================================
;;; Pending Actions Queue
;;; ============================================================================

(define (enqueue-action! state action)
  "Add action to pending queue"
  (define actions (agent-state-internal-pending-actions state))
  (set-agent-state-internal-pending-actions! state (append actions (list action)))
  (set-agent-state-internal-updated-at! state (current-seconds)))

(define (dequeue-action! state)
  "Remove and return next action from queue"
  (define actions (agent-state-internal-pending-actions state))
  (if (null? actions)
      #f
      (let ([action (car actions)])
        (set-agent-state-internal-pending-actions! state (cdr actions))
        (set-agent-state-internal-updated-at! state (current-seconds))
        action)))

(define (peek-action state)
  "Peek at next action without removing"
  (define actions (agent-state-internal-pending-actions state))
  (if (null? actions) #f (car actions)))

(define (has-pending-actions? state)
  "Check if there are pending actions"
  (not (null? (agent-state-internal-pending-actions state))))

(define (clear-pending-actions! state)
  "Clear all pending actions"
  (set-agent-state-internal-pending-actions! state '())
  (set-agent-state-internal-updated-at! state (current-seconds)))

;;; ============================================================================
;;; State Snapshots
;;; ============================================================================

(define (snapshot-state-variables state)
  "Create a snapshot of state variables"
  (for/hash ([(k v) (in-hash (agent-state-internal-variables state))])
    (values k v)))

(define (snapshot-context context)
  "Create a snapshot of execution context"
  (hash 'id (execution-context-internal-id context)
        'parent_id (execution-context-internal-parent-id context)
        'type (symbol->string (execution-context-internal-type context))
        'variables (hash->list (execution-context-internal-variables context))
        'stack (execution-context-internal-stack context)))

(define (snapshot-state state)
  "Create a complete snapshot of agent state"
  (hash 'id (agent-state-internal-id state)
        'context (snapshot-context (agent-state-internal-context state))
        'variables (hash->list (agent-state-internal-variables state))
        'conversation (map conversation-message->hash
                           (take (agent-state-internal-conversation state) 100))
        'pending_actions (agent-state-internal-pending-actions state)
        'metadata (hash->list (agent-state-internal-metadata state))
        'created_at (agent-state-internal-created-at state)
        'updated_at (agent-state-internal-updated-at state)))

;;; ============================================================================
;;; State Restoration
;;; ============================================================================

(define (restore-state-from-snapshot snapshot)
  "Restore agent state from snapshot"
  (agent-state-internal
   (hash-ref snapshot 'id)
   (restore-context-from-snapshot (hash-ref snapshot 'context))
   '()  ; History not restored from snapshot
   (list->hash-helper (hash-ref snapshot 'variables))
   (map hash->conversation-message (hash-ref snapshot 'conversation))
   (hash-ref snapshot 'pending_actions)
   (list->hash-helper (hash-ref snapshot 'metadata))
   (hash-ref snapshot 'created_at)
   (hash-ref snapshot 'updated_at)))

(define (restore-context-from-snapshot snapshot)
  "Restore execution context from snapshot"
  (execution-context-internal
   (hash-ref snapshot 'id)
   (hash-ref snapshot 'parent_id)
   (string->symbol (hash-ref snapshot 'type))
   #f
   #f
   (list->hash-helper (hash-ref snapshot 'variables))
   (hash-ref snapshot 'stack)
   (hash)
   (current-seconds)))

;;; ============================================================================
;;; Serialization Helpers
;;; ============================================================================

(define (conversation-message->hash msg)
  "Convert conversation message to hash"
  (hash 'id (conversation-message-id msg)
        'role (symbol->string (conversation-message-role msg))
        'content (conversation-message-content msg)
        'metadata (hash->list (conversation-message-metadata msg))
        'timestamp (conversation-message-timestamp msg)))

(define (hash->conversation-message h)
  "Convert hash to conversation message"
  (conversation-message
   (hash-ref h 'id)
   (string->symbol (hash-ref h 'role))
   (hash-ref h 'content)
   (list->hash-helper (hash-ref h 'metadata))
   (hash-ref h 'timestamp)))

;;; ============================================================================
;;; State Metadata
;;; ============================================================================

(define (state-set-metadata! state key value)
  "Set state metadata"
  (define meta (agent-state-internal-metadata state))
  (set-agent-state-internal-metadata! state (hash-set meta key value))
  (set-agent-state-internal-updated-at! state (current-seconds)))

(define (state-get-metadata state key #:default [default #f])
  "Get state metadata"
  (hash-ref (agent-state-internal-metadata state) key default))

;;; ============================================================================
;;; State Statistics
;;; ============================================================================

(define (state-stats state)
  "Get state statistics"
  (hash 'id (agent-state-internal-id state)
        'num_variables (hash-count (agent-state-internal-variables state))
        'num_history_entries (length (agent-state-internal-history state))
        'num_conversation_messages (length (agent-state-internal-conversation state))
        'num_pending_actions (length (agent-state-internal-pending-actions state))
        'created_at (agent-state-internal-created-at state)
        'updated_at (agent-state-internal-updated-at state)
        'age (- (current-seconds) (agent-state-internal-created-at state))))

;;; ============================================================================
;;; State Validation
;;; ============================================================================

(define (validate-state state)
  "Validate state integrity"
  (and (agent-state-internal? state)
       (string? (agent-state-internal-id state))
       (execution-context-internal? (agent-state-internal-context state))
       (hash? (agent-state-internal-variables state))
       (list? (agent-state-internal-history state))
       (list? (agent-state-internal-conversation state))
       (list? (agent-state-internal-pending-actions state))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (uuid-generate)
  "Generate UUID (simple implementation)"
  (format "~a-~a" (current-seconds) (random 1000000)))

(define (list->hash-helper lst)
  "Convert list of pairs to hash"
  (for/hash ([pair (in-list lst)])
    (values (car pair) (cdr pair))))

;;; ============================================================================
;;; Elixir Bridge Integration
;;; ============================================================================
;;; The elixir-wal-log! function is imported from ../elixir-bridge.rkt
