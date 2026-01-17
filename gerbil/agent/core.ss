;;; agent/core.ss - Core Agent Structure and Lifecycle Management
;;;
;;; This module defines the fundamental agent structure, lifecycle management,
;;; and state transitions for self-evolving AI agents.

(export #t)

(import
  :std/misc/threads
  :std/sugar
  :std/format
  :std/srfi/1
  :std/srfi/13
  :std/misc/hash
  :std/misc/uuid
  ./elixir-bridge)

;;; ============================================================================
;;; Agent Structure
;;; ============================================================================

(defstruct agent
  (id            ; Unique agent identifier (UUID)
   name          ; Human-readable name
   version       ; Current version number
   state         ; Current agent state (see agent/state.ss)
   memory        ; Memory system (see agent/memory.ss)
   tools         ; Available tools registry (see agent/tools.ss)
   config        ; Configuration parameters
   metadata      ; Additional metadata
   lifecycle     ; Lifecycle state: :initializing :running :evolving :suspended :terminated
   created-at    ; Creation timestamp
   updated-at    ; Last update timestamp
   checkpoint-id ; Last checkpoint ID from Elixir
   )
  transparent: #t)

;;; ============================================================================
;;; Agent Lifecycle States
;;; ============================================================================

(def agent-lifecycle-states
  '(:initializing   ; Agent is being initialized
    :running        ; Agent is actively processing
    :evolving       ; Agent is generating/testing new code
    :suspended      ; Agent is temporarily paused
    :terminated))   ; Agent has been shut down

(def (valid-lifecycle-state? state)
  "Check if a lifecycle state is valid"
  (member state agent-lifecycle-states))

;;; ============================================================================
;;; Agent Creation
;;; ============================================================================

(def (make-agent-instance
      name: (name "Agent")
      version: (version "0.1.0")
      config: (config (hash))
      metadata: (metadata (hash)))
  "Create a new agent instance with default initialization"
  (let* ((agent-id (uuid->string (make-uuid)))
         (now (time->seconds (current-time))))
    (make-agent
     id: agent-id
     name: name
     version: version
     state: #f              ; Will be initialized by agent/state.ss
     memory: #f             ; Will be initialized by agent/memory.ss
     tools: (hash)          ; Empty tools registry
     config: config
     metadata: metadata
     lifecycle: ':initializing
     created-at: now
     updated-at: now
     checkpoint-id: #f)))

;;; ============================================================================
;;; Lifecycle Transitions
;;; ============================================================================

(def (agent-transition! agent new-lifecycle)
  "Transition agent to a new lifecycle state"
  (unless (valid-lifecycle-state? new-lifecycle)
    (error "Invalid lifecycle state" new-lifecycle))

  (let ((old-lifecycle (agent-lifecycle agent)))
    ;; Validate transition
    (unless (valid-transition? old-lifecycle new-lifecycle)
      (error "Invalid lifecycle transition" old-lifecycle new-lifecycle))

    ;; Update agent
    (set! (agent-lifecycle agent) new-lifecycle)
    (set! (agent-updated-at agent) (time->seconds (current-time)))

    ;; Log transition
    (displayln (format "Agent ~a: ~a -> ~a"
                      (agent-name agent)
                      old-lifecycle
                      new-lifecycle))

    ;; Notify Elixir supervisor
    (elixir-send "lifecycle_change"
                 (hash ('agent_id (agent-id agent))
                       ('old_state (symbol->string old-lifecycle))
                       ('new_state (symbol->string new-lifecycle))
                       ('timestamp (agent-updated-at agent))))))

(def (valid-transition? from to)
  "Check if a lifecycle transition is valid"
  (case from
    ((:initializing) (member to '(:running :terminated)))
    ((:running) (member to '(:evolving :suspended :terminated)))
    ((:evolving) (member to '(:running :terminated)))
    ((:suspended) (member to '(:running :terminated)))
    ((:terminated) #f)  ; No transitions from terminated
    (else #f)))

;;; ============================================================================
;;; Agent Initialization
;;; ============================================================================

(def (agent-initialize! agent state-init memory-init)
  "Initialize agent with state and memory systems"
  (unless (eq? (agent-lifecycle agent) ':initializing)
    (error "Agent must be in :initializing state" (agent-lifecycle agent)))

  ;; Initialize state (from agent/state.ss)
  (set! (agent-state agent) state-init)

  ;; Initialize memory (from agent/memory.ss)
  (set! (agent-memory agent) memory-init)

  ;; Transition to running
  (agent-transition! agent ':running)

  ;; Create initial checkpoint
  (agent-checkpoint! agent)

  agent)

;;; ============================================================================
;;; Agent Checkpointing
;;; ============================================================================

;;; Helper function to convert association list to hash table
(def (list->hash lst)
  "Convert association list to hash table"
  (let ((h (hash)))
    (for-each (lambda (pair)
                (hash-put! h (car pair) (cdr pair)))
              lst)
    h))

(def (agent-checkpoint! agent)
  "Create a checkpoint of the agent's current state"
  (let* ((checkpoint-data (serialize-agent agent))
         (checkpoint-id (elixir-checkpoint! checkpoint-data)))
    (set! (agent-checkpoint-id agent) checkpoint-id)
    (set! (agent-updated-at agent) (time->seconds (current-time)))
    checkpoint-id))

(def (serialize-agent agent)
  "Serialize agent to a hash for checkpointing"
  (hash
   ('id (agent-id agent))
   ('name (agent-name agent))
   ('version (agent-version agent))
   ('state (if (agent-state agent)
               (serialize-state (agent-state agent))
               #f))
   ('memory (if (agent-memory agent)
                (serialize-memory (agent-memory agent))
                #f))
   ('tools (hash->list (agent-tools agent)))
   ('config (hash->list (agent-config agent)))
   ('metadata (hash->list (agent-metadata agent)))
   ('lifecycle (symbol->string (agent-lifecycle agent)))
   ('created-at (agent-created-at agent))
   ('updated-at (agent-updated-at agent))
   ('checkpoint-id (agent-checkpoint-id agent))))

(def (deserialize-agent data)
  "Deserialize agent from checkpoint data"
  (make-agent
   id: (hash-ref data 'id)
   name: (hash-ref data 'name)
   version: (hash-ref data 'version)
   state: (let ((state-data (hash-ref data 'state)))
            (if state-data
                (deserialize-state state-data)
                #f))
   memory: (let ((memory-data (hash-ref data 'memory)))
             (if memory-data
                 (deserialize-memory memory-data)
                 #f))
   tools: (list->hash (hash-ref data 'tools))
   config: (list->hash (hash-ref data 'config))
   metadata: (list->hash (hash-ref data 'metadata))
   lifecycle: (string->symbol (hash-ref data 'lifecycle))
   created-at: (hash-ref data 'created-at)
   updated-at: (hash-ref data 'updated-at)
   checkpoint-id: (hash-ref data 'checkpoint-id)))

;;; Placeholder functions (will be implemented in respective modules)
(def (serialize-state state)
  "Placeholder for state serialization"
  (if state (hash ('placeholder #t)) #f))

(def (deserialize-state data)
  "Placeholder for state deserialization"
  data)

(def (serialize-memory memory)
  "Placeholder for memory serialization"
  (if memory (hash ('placeholder #t)) #f))

(def (deserialize-memory data)
  "Placeholder for memory deserialization"
  data)

;;; ============================================================================
;;; Agent Restoration
;;; ============================================================================

(def (agent-restore! checkpoint-id)
  "Restore agent from a checkpoint"
  (displayln (format "Restoring agent from checkpoint: ~a" checkpoint-id))

  ;; Request checkpoint from Elixir
  (elixir-send "restore_checkpoint" (hash ('checkpoint_id checkpoint-id)))

  ;; Wait for response
  (let ((response (wait-for-message "checkpoint_data" timeout: 30)))
    (if response
        (let ((checkpoint-data (hash-ref response 'data)))
          (deserialize-agent checkpoint-data))
        (error "Failed to restore checkpoint" checkpoint-id))))

;;; ============================================================================
;;; Agent Shutdown
;;; ============================================================================

(def (agent-shutdown! agent)
  "Gracefully shutdown the agent"
  (displayln (format "Shutting down agent: ~a" (agent-name agent)))

  ;; Create final checkpoint
  (when (member (agent-lifecycle agent) '(:running :suspended))
    (agent-checkpoint! agent))

  ;; Transition to terminated
  (agent-transition! agent ':terminated)

  ;; Notify Elixir
  (elixir-send "agent_terminated"
               (hash ('agent_id (agent-id agent))
                     ('timestamp (time->seconds (current-time))))))

;;; ============================================================================
;;; Agent Evolution Support
;;; ============================================================================

(def (agent-begin-evolution! agent)
  "Begin evolution process (transition to :evolving state)"
  (unless (eq? (agent-lifecycle agent) ':running)
    (error "Agent must be running to begin evolution" (agent-lifecycle agent)))

  ;; Create checkpoint before evolution
  (agent-checkpoint! agent)

  ;; Transition to evolving
  (agent-transition! agent ':evolving)

  ;; Notify Elixir to start shadow testing
  (elixir-send "begin_evolution"
               (hash ('agent_id (agent-id agent))
                     ('checkpoint_id (agent-checkpoint-id agent))
                     ('timestamp (time->seconds (current-time))))))

(def (agent-end-evolution! agent success?)
  "End evolution process (transition back to :running)"
  (unless (eq? (agent-lifecycle agent) ':evolving)
    (error "Agent must be evolving to end evolution" (agent-lifecycle agent)))

  ;; Transition back to running
  (agent-transition! agent ':running)

  ;; Notify Elixir
  (elixir-send "end_evolution"
               (hash ('agent_id (agent-id agent))
                     ('success success?)
                     ('timestamp (time->seconds (current-time))))))

;;; ============================================================================
;;; Agent Suspension/Resumption
;;; ============================================================================

(def (agent-suspend! agent)
  "Suspend agent execution"
  (unless (eq? (agent-lifecycle agent) ':running)
    (error "Only running agents can be suspended" (agent-lifecycle agent)))

  ;; Create checkpoint
  (agent-checkpoint! agent)

  ;; Transition to suspended
  (agent-transition! agent ':suspended))

(def (agent-resume! agent)
  "Resume suspended agent"
  (unless (eq? (agent-lifecycle agent) ':suspended)
    (error "Only suspended agents can be resumed" (agent-lifecycle agent)))

  ;; Transition to running
  (agent-transition! agent ':running))

;;; ============================================================================
;;; Agent Metadata Management
;;; ============================================================================

(def (agent-set-metadata! agent key value)
  "Set a metadata key-value pair"
  (hash-put! (agent-metadata agent) key value)
  (set! (agent-updated-at agent) (time->seconds (current-time))))

(def (agent-get-metadata agent key (default #f))
  "Get a metadata value"
  (hash-ref (agent-metadata agent) key default))

;;; ============================================================================
;;; Agent Configuration
;;; ============================================================================

(def (agent-set-config! agent key value)
  "Set a configuration key-value pair"
  (hash-put! (agent-config agent) key value)
  (set! (agent-updated-at agent) (time->seconds (current-time))))

(def (agent-get-config agent key (default #f))
  "Get a configuration value"
  (hash-ref (agent-config agent) key default))

;;; ============================================================================
;;; Agent Status
;;; ============================================================================

(def (agent-status agent)
  "Get current agent status as a hash"
  (hash
   ('id (agent-id agent))
   ('name (agent-name agent))
   ('version (agent-version agent))
   ('lifecycle (agent-lifecycle agent))
   ('created_at (agent-created-at agent))
   ('updated_at (agent-updated-at agent))
   ('checkpoint_id (agent-checkpoint-id agent))
   ('uptime (- (time->seconds (current-time)) (agent-created-at agent)))))

(def (agent-is-running? agent)
  "Check if agent is in running state"
  (eq? (agent-lifecycle agent) ':running))

(def (agent-is-evolving? agent)
  "Check if agent is in evolving state"
  (eq? (agent-lifecycle agent) ':evolving))

(def (agent-is-terminated? agent)
  "Check if agent is terminated"
  (eq? (agent-lifecycle agent) ':terminated))
