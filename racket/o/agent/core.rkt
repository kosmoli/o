#lang racket

;;; agent/core.rkt - Core Agent Structure and Lifecycle Management
;;;
;;; Defines the fundamental agent structure, lifecycle management,
;;; and state transitions for self-evolving AI agents.

(provide (struct-out agent)
         agent-lifecycle-states
         valid-lifecycle-state?
         make-agent-instance
         agent-transition!
         valid-transition?
         agent-initialize!
         agent-checkpoint!
         serialize-agent
         deserialize-agent
         agent-restore!
         agent-shutdown!
         agent-begin-evolution!
         agent-end-evolution!
         agent-suspend!
         agent-resume!
         agent-set-metadata!
         agent-get-metadata
         agent-set-config!
         agent-get-config
         agent-status
         agent-running?
         agent-evolving?
         agent-terminated?)

(require racket/hash
         racket/format
         racket/list
         "types.rkt"
         "../message/types.rkt"
         "../database/client.rkt")

;;; ============================================================================
;;; Agent Structure
;;; ============================================================================

(struct agent
  (id            ; Unique agent identifier (UUID)
   name          ; Human-readable name
   version       ; Current version number
   state         ; Current agent state
   memory        ; Memory system
   tools         ; Available tools registry
   config        ; Configuration parameters
   metadata      ; Additional metadata
   lifecycle     ; Lifecycle state
   created-at    ; Creation timestamp
   updated-at    ; Last update timestamp
   checkpoint-id) ; Last checkpoint ID
  #:transparent
  #:mutable)

;;; ============================================================================
;;; Agent Lifecycle States
;;; ============================================================================

(define agent-lifecycle-states
  '(initializing running evolving suspended terminated))

(define (valid-lifecycle-state? state)
  "Check if a lifecycle state is valid"
  (if (member state agent-lifecycle-states) #t #f))

;;; ============================================================================
;;; Agent Creation
;;; ============================================================================

(define (make-agent-instance
         #:name [name "Agent"]
         #:version [version "0.1.0"]
         #:config [config (hash)]
         #:metadata [metadata (hash)])
  "Create a new agent instance with default initialization"
  (define agent-id (uuid-generate))
  (define now (current-seconds))
  (agent
   agent-id
   name
   version
   #f              ; Will be initialized by agent/state.ss
   #f             ; Will be initialized by agent/memory.ss
   (hash)          ; Empty tools registry
   config
   metadata
   'initializing
   now
   now
   #f))

;;; ============================================================================
;;; Lifecycle Transitions
;;; ============================================================================

(define (agent-transition! agent new-lifecycle)
  "Transition agent to a new lifecycle state"
  (unless (valid-lifecycle-state? new-lifecycle)
    (error 'agent-transition! "Invalid lifecycle state" new-lifecycle))

  (define old-lifecycle (agent-lifecycle agent))

  ;; Validate transition
  (unless (valid-transition? old-lifecycle new-lifecycle)
    (error 'agent-transition! "Invalid lifecycle transition" old-lifecycle new-lifecycle))

  ;; Update agent
  (set-agent-lifecycle! agent new-lifecycle)
  (set-agent-updated-at! agent (current-seconds))

  ;; Log transition
  (displayln (format "Agent ~a: ~a -> ~a"
                     (agent-name agent)
                     old-lifecycle
                     new-lifecycle))

  ;; Notify database supervisor
  (db-create-message
   (hash 'agent_id (agent-id agent)
         'type 'lifecycle_change
         'content (format "Lifecycle: ~a -> ~a" old-lifecycle new-lifecycle)
         'old_state (symbol->string old-lifecycle)
         'new_state (symbol->string new-lifecycle)
         'timestamp (agent-updated-at agent))))

(define (valid-transition? from to)
  "Check if a lifecycle transition is valid"
  (case from
    [(initializing) (member to '(running terminated))]
    [(running) (member to '(evolving suspended terminated))]
    [(evolving) (member to '(running terminated))]
    [(suspended) (member to '(running terminated))]
    [(terminated) #f]  ; No transitions from terminated
    [else #f]))

;;; ============================================================================
;;; Agent Initialization
;;; ============================================================================

(define (agent-initialize! agent state-init memory-init)
  "Initialize agent with state and memory systems"
  (unless (eq? (agent-lifecycle agent) 'initializing)
    (error 'agent-initialize! "Agent must be in initializing state"))

  ;; Initialize state (from agent/state.ss)
  (set-agent-state! agent state-init)

  ;; Initialize memory (from agent/memory.ss)
  (set-agent-memory! agent memory-init)

  ;; Transition to running
  (agent-transition! agent 'running)

  ;; Create initial checkpoint
  (agent-checkpoint! agent)

  agent)

;;; ============================================================================
;;; Agent Checkpointing
;;; ============================================================================

(define (agent-checkpoint! agent)
  "Create a checkpoint of the agent's current state"
  (define checkpoint-data (serialize-agent agent))
  ;; Store checkpoint in database
  (define checkpoint-id
    (format "ckpt_~a_~a" (agent-id agent) (current-seconds)))
  (db-create-message
   (hash 'agent_id (agent-id agent)
         'type 'checkpoint
         'checkpoint_id checkpoint-id
         'data checkpoint-data))
  (set-agent-checkpoint-id! agent checkpoint-id)
  (set-agent-updated-at! agent (current-seconds))
  checkpoint-id)

(define (serialize-agent agent)
  "Serialize agent to a hash for checkpointing"
  (hash
   'id (agent-id agent)
   'name (agent-name agent)
   'version (agent-version agent)
   'state (if (agent-state agent)
              (hash 'placeholder #t)
              #f)
   'memory (if (agent-memory agent)
               (hash 'placeholder #t)
               #f)
   'tools (hash->list (agent-tools agent))
   'config (hash->list (agent-config agent))
   'metadata (hash->list (agent-metadata agent))
   'lifecycle (symbol->string (agent-lifecycle agent))
   'created-at (agent-created-at agent)
   'updated-at (agent-updated-at agent)
   'checkpoint-id (agent-checkpoint-id agent)))

(define (deserialize-agent data)
  "Deserialize agent from checkpoint data"

  (define (list->hash-helper lst)
    (for/hash ([pair lst])
      (values (car pair) (cdr pair))))

  (agent
   (hash-ref data 'id)
   (hash-ref data 'name)
   (hash-ref data 'version)
   #f  ; state
   #f  ; memory
   (list->hash-helper (hash-ref data 'tools))
   (list->hash-helper (hash-ref data 'config))
   (list->hash-helper (hash-ref data 'metadata))
   (string->symbol (hash-ref data 'lifecycle))
   (hash-ref data 'created-at)
   (hash-ref data 'updated-at)
   (hash-ref data 'checkpoint-id)))

;;; ============================================================================
;;; Agent Restoration
;;; ============================================================================

(define (agent-restore! checkpoint-id)
  "Restore agent from a checkpoint"
  (displayln (format "Restoring agent from checkpoint: ~a" checkpoint-id))

  ;; Request checkpoint from database
  (define messages (db-get-messages checkpoint-id #:limit 1))

  (if (null? messages)
      (error 'agent-restore! "Failed to restore checkpoint" checkpoint-id)
      (let ([checkpoint-data (message-content (first messages))])
        (deserialize-agent checkpoint-data))))

;;; ============================================================================
;;; Agent Shutdown
;;; ============================================================================

(define (agent-shutdown! agent)
  "Gracefully shutdown the agent"
  (displayln (format "Shutting down agent: ~a" (agent-name agent)))

  ;; Create final checkpoint
  (when (member (agent-lifecycle agent) '(running suspended))
    (agent-checkpoint! agent))

  ;; Transition to terminated
  (agent-transition! agent 'terminated)

  ;; Notify database
  (db-create-message
   (hash 'agent_id (agent-id agent)
         'type 'terminated
         'timestamp (current-seconds))))

;;; ============================================================================
;;; Agent Evolution Support
;;; ============================================================================

(define (agent-begin-evolution! agent)
  "Begin evolution process (transition to evolving state)"
  (unless (eq? (agent-lifecycle agent) 'running)
    (error 'agent-begin-evolution! "Agent must be running to begin evolution"))

  ;; Create checkpoint before evolution
  (agent-checkpoint! agent)

  ;; Transition to evolving
  (agent-transition! agent 'evolving)

  ;; Notify database
  (db-create-message
   (hash 'agent_id (agent-id agent)
         'type 'evolution_begin
         'checkpoint_id (agent-checkpoint-id agent)
         'timestamp (current-seconds))))

(define (agent-end-evolution! agent success?)
  "End evolution process (transition back to running)"
  (unless (eq? (agent-lifecycle agent) 'evolving)
    (error 'agent-end-evolution! "Agent must be evolving to end evolution"))

  ;; Transition back to running
  (agent-transition! agent 'running)

  ;; Notify database
  (db-create-message
   (hash 'agent_id (agent-id agent)
         'type 'evolution_end
         'success success?
         'timestamp (current-seconds))))

;;; ============================================================================
;;; Agent Suspension/Resumption
;;; ============================================================================

(define (agent-suspend! agent)
  "Suspend agent execution"
  (unless (eq? (agent-lifecycle agent) 'running)
    (error 'agent-suspend! "Only running agents can be suspended"))

  ;; Create checkpoint
  (agent-checkpoint! agent)

  ;; Transition to suspended
  (agent-transition! agent 'suspended))

(define (agent-resume! agent)
  "Resume suspended agent"
  (unless (eq? (agent-lifecycle agent) 'suspended)
    (error 'agent-resume! "Only suspended agents can be resumed"))

  ;; Transition to running
  (agent-transition! agent 'running))

;;; ============================================================================
;;; Agent Metadata Management
;;; ============================================================================

(define (agent-set-metadata! agent key value)
  "Set a metadata key-value pair"
  (hash-set! (agent-metadata agent) key value)
  (set-agent-updated-at! agent (current-seconds)))

(define (agent-get-metadata agent key #:default [default #f])
  "Get a metadata value"
  (hash-ref (agent-metadata agent) key default))

;;; ============================================================================
;;; Agent Configuration
;;; ============================================================================

(define (agent-set-config! agent key value)
  "Set a configuration key-value pair"
  (hash-set! (agent-config agent) key value)
  (set-agent-updated-at! agent (current-seconds)))

(define (agent-get-config agent key #:default [default #f])
  "Get a configuration value"
  (hash-ref (agent-config agent) key default))

;;; ============================================================================
;;; Agent Status
;;; ============================================================================

(define (agent-status agent)
  "Get current agent status as a hash"
  (hash
   'id (agent-id agent)
   'name (agent-name agent)
   'version (agent-version agent)
   'lifecycle (agent-lifecycle agent)
   'created_at (agent-created-at agent)
   'updated_at (agent-updated-at agent)
   'checkpoint_id (agent-checkpoint-id agent)
   'uptime (- (current-seconds) (agent-created-at agent))))

(define (agent-running? agent)
  "Check if agent is in running state"
  (eq? (agent-lifecycle agent) 'running))

(define (agent-evolving? agent)
  "Check if agent is in evolving state"
  (eq? (agent-lifecycle agent) 'evolving))

(define (agent-terminated? agent)
  "Check if agent is terminated"
  (eq? (agent-lifecycle agent) 'terminated))
