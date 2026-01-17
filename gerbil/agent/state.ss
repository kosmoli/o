;;; agent/state.ss - Agent State Management and Context
;;;
;;; This module manages agent state, context, and execution history.
;;; It provides structures and functions for tracking agent state transitions,
;;; maintaining execution context, and managing conversation history.

(export #t)

(import
  :std/misc/threads
  :std/sugar
  :std/srfi/1
  :std/srfi/13
  :std/misc/hash
  :std/misc/uuid
  :o/agent/elixir-bridge)

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

;;; Helper function to convert association list to hash table
(def (list->hash lst)
  "Convert association list to hash table"
  (let ((h (hash)))
    (for-each (lambda (pair)
                (hash-put! h (car pair) (cdr pair)))
              lst)
    h))

;;; ============================================================================
;;; State Structure
;;; ============================================================================

(defstruct agent-state
  (id                ; Unique state identifier
   context           ; Current execution context
   history           ; Execution history (list of state snapshots)
   variables         ; State variables (hash table)
   conversation      ; Conversation history
   pending-actions   ; Queue of pending actions
   metadata          ; Additional metadata
   created-at        ; Creation timestamp
   updated-at)       ; Last update timestamp
  transparent: #t)

;;; ============================================================================
;;; Context Structure
;;; ============================================================================

(defstruct execution-context
  (id                ; Context identifier
   parent-id         ; Parent context (for nested contexts)
   type              ; Context type: :main :tool :evolution :recovery
   input             ; Current input being processed
   output            ; Current output
   variables         ; Context-local variables
   stack             ; Execution stack
   metadata          ; Context metadata
   created-at)       ; Creation timestamp
  transparent: #t)

;;; ============================================================================
;;; History Entry Structure
;;; ============================================================================

(defstruct history-entry
  (id                ; Entry identifier
   timestamp         ; When this entry was created
   state-snapshot    ; Snapshot of state at this point
   context-snapshot  ; Snapshot of context
   action            ; Action that was taken
   result            ; Result of the action
   metadata)         ; Additional metadata
  transparent: #t)

;;; ============================================================================
;;; Conversation Message Structure
;;; ============================================================================

(defstruct conversation-message
  (id                ; Message identifier
   role              ; Role: :user :assistant :system :tool
   content           ; Message content
   metadata          ; Additional metadata (tool calls, etc.)
   timestamp)        ; Message timestamp
  transparent: #t)

;;; ============================================================================
;;; State Creation
;;; ============================================================================

(def (make-agent-state-instance)
  "Create a new agent state instance"
  (let ((now (time->seconds (current-time))))
    (make-agent-state
     id: (uuid->string (make-uuid))
     context: (make-initial-context)
     history: '()
     variables: (hash)
     conversation: '()
     pending-actions: '()
     metadata: (hash)
     created-at: now
     updated-at: now)))

(def (make-initial-context)
  "Create initial execution context"
  (make-execution-context
   id: (uuid->string (make-uuid))
   parent-id: #f
   type: ':main
   input: #f
   output: #f
   variables: (hash)
   stack: '()
   metadata: (hash)
   created-at: (time->seconds (current-time))))

;;; ============================================================================
;;; State Variables
;;; ============================================================================

(def (state-set! state key value)
  "Set a state variable"
  (hash-put! (agent-state-variables state) key value)
  (set! (agent-state-updated-at state) (time->seconds (current-time)))

  ;; Log to WAL
  (elixir-wal-log! 'state-set
                   (hash ('state_id (agent-state-id state))
                         ('key (symbol->string key))
                         ('value value))))

(def (state-get state key (default #f))
  "Get a state variable"
  (hash-ref (agent-state-variables state) key default))

(def (state-has? state key)
  "Check if state has a variable"
  (hash-key? (agent-state-variables state) key))

(def (state-delete! state key)
  "Delete a state variable"
  (hash-remove! (agent-state-variables state) key)
  (set! (agent-state-updated-at state) (time->seconds (current-time)))

  ;; Log to WAL
  (elixir-wal-log! 'state-delete
                   (hash ('state_id (agent-state-id state))
                         ('key (symbol->string key)))))

(def (state-clear! state)
  "Clear all state variables"
  (set! (agent-state-variables state) (hash))
  (set! (agent-state-updated-at state) (time->seconds (current-time)))

  ;; Log to WAL
  (elixir-wal-log! 'state-clear
                   (hash ('state_id (agent-state-id state)))))

;;; ============================================================================
;;; Context Management
;;; ============================================================================

(def (context-set! context key value)
  "Set a context variable"
  (hash-put! (execution-context-variables context) key value))

(def (context-get context key (default #f))
  "Get a context variable"
  (hash-ref (execution-context-variables context) key default))

(def (context-push-stack! context item)
  "Push item onto execution stack"
  (set! (execution-context-stack context)
        (cons item (execution-context-stack context))))

(def (context-pop-stack! context)
  "Pop item from execution stack"
  (let ((stack (execution-context-stack context)))
    (if (null? stack)
        #f
        (let ((item (car stack)))
          (set! (execution-context-stack context) (cdr stack))
          item))))

(def (context-peek-stack context)
  "Peek at top of execution stack"
  (let ((stack (execution-context-stack context)))
    (if (null? stack) #f (car stack))))

;;; ============================================================================
;;; Nested Context Management
;;; ============================================================================

(def (create-nested-context parent-context type)
  "Create a nested execution context"
  (make-execution-context
   id: (uuid->string (make-uuid))
   parent-id: (execution-context-id parent-context)
   type: type
   input: #f
   output: #f
   variables: (hash)
   stack: '()
   metadata: (hash)
   created-at: (time->seconds (current-time))))

(def (switch-context! state new-context)
  "Switch to a new execution context"
  (let ((old-context (agent-state-context state)))
    ;; Save old context to stack
    (context-push-stack! new-context old-context)

    ;; Switch to new context
    (set! (agent-state-context state) new-context)
    (set! (agent-state-updated-at state) (time->seconds (current-time)))))

(def (restore-parent-context! state)
  "Restore parent context from stack"
  (let* ((current-context (agent-state-context state))
         (parent-context (context-pop-stack! current-context)))
    (when parent-context
      (set! (agent-state-context state) parent-context)
      (set! (agent-state-updated-at state) (time->seconds (current-time))))
    parent-context))

;;; ============================================================================
;;; History Management
;;; ============================================================================

(def (add-history-entry! state action result)
  "Add an entry to execution history"
  (let* ((entry (make-history-entry
                 id: (uuid->string (make-uuid))
                 timestamp: (time->seconds (current-time))
                 state-snapshot: (snapshot-state-variables state)
                 context-snapshot: (snapshot-context (agent-state-context state))
                 action: action
                 result: result
                 metadata: (hash)))
         (history (agent-state-history state)))

    ;; Add to history (keep last 1000 entries)
    (set! (agent-state-history state)
          (take (cons entry history) 1000))

    (set! (agent-state-updated-at state) (time->seconds (current-time)))

    ;; Log to WAL
    (elixir-wal-log! 'history-entry
                     (hash ('state_id (agent-state-id state))
                           ('entry_id (history-entry-id entry))
                           ('action action)))))

(def (get-history state (limit 100))
  "Get execution history (most recent first)"
  (take (agent-state-history state) limit))

(def (get-history-entry state entry-id)
  "Get a specific history entry by ID"
  (find (lambda (entry)
          (equal? (history-entry-id entry) entry-id))
        (agent-state-history state)))

(def (clear-history! state)
  "Clear execution history"
  (set! (agent-state-history state) '())
  (set! (agent-state-updated-at state) (time->seconds (current-time))))

;;; ============================================================================
;;; Conversation Management
;;; ============================================================================

(def (add-message! state role content (metadata (hash)))
  "Add a message to conversation history"
  (let ((message (make-conversation-message
                  id: (uuid->string (make-uuid))
                  role: role
                  content: content
                  metadata: metadata
                  timestamp: (time->seconds (current-time)))))

    ;; Add to conversation (keep last 1000 messages)
    (set! (agent-state-conversation state)
          (take (cons message (agent-state-conversation state)) 1000))

    (set! (agent-state-updated-at state) (time->seconds (current-time)))

    ;; Log to WAL
    (elixir-wal-log! 'conversation-message
                     (hash ('state_id (agent-state-id state))
                           ('message_id (conversation-message-id message))
                           ('role (symbol->string role))))

    message))

(def (get-conversation state (limit 100))
  "Get conversation history (most recent first)"
  (take (agent-state-conversation state) limit))

(def (get-conversation-context state (limit 10))
  "Get recent conversation for context (oldest first)"
  (reverse (take (agent-state-conversation state) limit)))

(def (clear-conversation! state)
  "Clear conversation history"
  (set! (agent-state-conversation state) '())
  (set! (agent-state-updated-at state) (time->seconds (current-time))))

;;; ============================================================================
;;; Pending Actions Queue
;;; ============================================================================

(def (enqueue-action! state action)
  "Add action to pending queue"
  (set! (agent-state-pending-actions state)
        (append (agent-state-pending-actions state) (list action)))
  (set! (agent-state-updated-at state) (time->seconds (current-time))))

(def (dequeue-action! state)
  "Remove and return next action from queue"
  (let ((actions (agent-state-pending-actions state)))
    (if (null? actions)
        #f
        (let ((action (car actions)))
          (set! (agent-state-pending-actions state) (cdr actions))
          (set! (agent-state-updated-at state) (time->seconds (current-time)))
          action))))

(def (peek-action state)
  "Peek at next action without removing"
  (let ((actions (agent-state-pending-actions state)))
    (if (null? actions) #f (car actions))))

(def (has-pending-actions? state)
  "Check if there are pending actions"
  (not (null? (agent-state-pending-actions state))))

(def (clear-pending-actions! state)
  "Clear all pending actions"
  (set! (agent-state-pending-actions state) '())
  (set! (agent-state-updated-at state) (time->seconds (current-time))))

;;; ============================================================================
;;; State Snapshots
;;; ============================================================================

(def (snapshot-state-variables state)
  "Create a snapshot of state variables"
  (hash-copy (agent-state-variables state)))

(def (snapshot-context context)
  "Create a snapshot of execution context"
  (hash
   ('id (execution-context-id context))
   ('parent_id (execution-context-parent-id context))
   ('type (symbol->string (execution-context-type context)))
   ('variables (hash->list (execution-context-variables context)))
   ('stack (execution-context-stack context))))

(def (snapshot-state state)
  "Create a complete snapshot of agent state"
  (hash
   ('id (agent-state-id state))
   ('context (snapshot-context (agent-state-context state)))
   ('variables (hash->list (agent-state-variables state)))
   ('conversation (map conversation-message->hash
                       (take (agent-state-conversation state) 100)))
   ('pending_actions (agent-state-pending-actions state))
   ('metadata (hash->list (agent-state-metadata state)))
   ('created_at (agent-state-created-at state))
   ('updated_at (agent-state-updated-at state))))

;;; ============================================================================
;;; State Restoration
;;; ============================================================================

(def (restore-state-from-snapshot snapshot)
  "Restore agent state from snapshot"
  (let ((state (make-agent-state
                id: (hash-ref snapshot 'id)
                context: (restore-context-from-snapshot
                          (hash-ref snapshot 'context))
                history: '()  ; History not restored from snapshot
                variables: (list->hash (hash-ref snapshot 'variables))
                conversation: (map hash->conversation-message
                                   (hash-ref snapshot 'conversation))
                pending-actions: (hash-ref snapshot 'pending_actions)
                metadata: (list->hash (hash-ref snapshot 'metadata))
                created-at: (hash-ref snapshot 'created_at)
                updated-at: (hash-ref snapshot 'updated_at))))
    state))

(def (restore-context-from-snapshot snapshot)
  "Restore execution context from snapshot"
  (make-execution-context
   id: (hash-ref snapshot 'id)
   parent-id: (hash-ref snapshot 'parent_id)
   type: (string->symbol (hash-ref snapshot 'type))
   input: #f
   output: #f
   variables: (list->hash (hash-ref snapshot 'variables))
   stack: (hash-ref snapshot 'stack)
   metadata: (hash)
   created-at: (time->seconds (current-time))))

;;; ============================================================================
;;; Serialization Helpers
;;; ============================================================================

(def (conversation-message->hash msg)
  "Convert conversation message to hash"
  (hash
   ('id (conversation-message-id msg))
   ('role (symbol->string (conversation-message-role msg)))
   ('content (conversation-message-content msg))
   ('metadata (hash->list (conversation-message-metadata msg)))
   ('timestamp (conversation-message-timestamp msg))))

(def (hash->conversation-message h)
  "Convert hash to conversation message"
  (make-conversation-message
   (hash-ref h 'id)
   (string->symbol (hash-ref h 'role))
   (hash-ref h 'content)
   (list->hash (hash-ref h 'metadata))
   (hash-ref h 'timestamp)))

;;; ============================================================================
;;; State Metadata
;;; ============================================================================

(def (state-set-metadata! state key value)
  "Set state metadata"
  (hash-put! (agent-state-metadata state) key value)
  (set! (agent-state-updated-at state) (time->seconds (current-time))))

(def (state-get-metadata state key (default #f))
  "Get state metadata"
  (hash-ref (agent-state-metadata state) key default))

;;; ============================================================================
;;; State Statistics
;;; ============================================================================

(def (state-stats state)
  "Get state statistics"
  (hash
   ('id (agent-state-id state))
   ('num_variables (hash-length (agent-state-variables state)))
   ('num_history_entries (length (agent-state-history state)))
   ('num_conversation_messages (length (agent-state-conversation state)))
   ('num_pending_actions (length (agent-state-pending-actions state)))
   ('created_at (agent-state-created-at state))
   ('updated_at (agent-state-updated-at state))
   ('age (- (time->seconds (current-time)) (agent-state-created-at state)))))

;;; ============================================================================
;;; State Validation
;;; ============================================================================

(def (validate-state state)
  "Validate state integrity"
  (and
   (agent-state? state)
   (string? (agent-state-id state))
   (execution-context? (agent-state-context state))
   (hash-table? (agent-state-variables state))
   (list? (agent-state-history state))
   (list? (agent-state-conversation state))
   (list? (agent-state-pending-actions state))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (take lst n)
  "Take first n elements from list"
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(def (hash-copy h)
  "Create a shallow copy of hash"
  (let ((new-hash (hash)))
    (hash-for-each
     (lambda (k v)
       (hash-put! new-hash k v))
     h)
    new-hash))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create state
(def my-state (make-agent-state-instance))

;; Set variables
(state-set! my-state 'counter 0)
(state-set! my-state 'name "TestAgent")

;; Add conversation
(add-message! my-state :user "Hello, agent!")
(add-message! my-state :assistant "Hello! How can I help you?")

;; Add history entry
(add-history-entry! my-state
                    (hash ('type 'process-input) ('input "test"))
                    (hash ('status 'success)))

;; Create snapshot
(def snapshot (snapshot-state my-state))

;; Restore from snapshot
(def restored-state (restore-state-from-snapshot snapshot))
|#
