#lang racket

;;; database/agents.rkt - Agent Database Operations
;;;
;;; High-level agent database operations built on top of the database client.

(provide agent-create!
         agent-get
         agent-update!
         agent-delete!
         agent-list
         agent-get-config
         agent-update-config!
         agent-get-memory
         agent-update-memory!
         agent-get-stats
         agent-increment-stat!
         agent-exists?
         agent-validate-config)

(require racket/hash
        racket/format
        racket/list
        "./client.rkt")

;;; ============================================================================
;;; Agent Management
;;; ============================================================================

(define (agent-create! name
                       #:llm-provider [llm-provider "openai"]
                       #:llm-model [llm-model "gpt-4"]
                       #:llm-temperature [llm-temperature 0.7]
                       #:llm-max-tokens [llm-max-tokens 4096]
                       #:system-prompt [system-prompt "You are a helpful assistant."]
                       #:persona [persona "You are a helpful AI assistant."]
                       #:human [human "User information will be stored here."])
  "Create a new agent with default memory blocks"

  (define agent (db-create-agent
                 (hash 'name name
                       'llm_provider llm-provider
                       'llm_model llm-model
                       'llm_temperature llm-temperature
                       'llm_max_tokens llm-max-tokens
                       'system_prompt system-prompt)))

  (define agent-id (hash-ref agent 'id))

  (when (or persona human)
    (db-initialize-agent-memory agent-id persona human))

  (displayln (format "Agent created: ~a (~a)" name agent-id))
  agent)

(define (agent-get agent-id)
  "Get agent by ID"
  (db-get-agent agent-id))

(define (agent-update! agent-id updates)
  "Update agent configuration"
  (db-update-agent agent-id updates))

(define (agent-delete! agent-id)
  "Delete agent (soft delete)"
  (db-delete-agent agent-id)
  (displayln (format "Agent deleted: ~a" agent-id)))

(define (agent-list #:limit [limit 10] #:offset [offset 0])
  "List agents"
  (db-list-agents #:limit limit #:offset offset))

;;; ============================================================================
;;; Agent Configuration
;;; ============================================================================

(define (agent-get-config agent-id)
  "Get agent configuration"
  (define agent (agent-get agent-id))
  (hash 'llm_config (hash 'provider (hash-ref agent 'llm_provider)
                           'model (hash-ref agent 'llm_model)
                           'temperature (hash-ref agent 'llm_temperature)
                           'max_tokens (hash-ref agent 'llm_max_tokens))
        'system_prompt (hash-ref agent 'system_prompt)
        'memory_config (hash 'core_memory_enabled (hash-ref agent 'core_memory_enabled)
                            'archival_memory_enabled (hash-ref agent 'archival_memory_enabled)
                            'max_archival_entries (hash-ref agent 'max_archival_entries))))

(define (agent-update-config! agent-id config)
  "Update agent configuration"
  (define updates (make-hash))

  (when (hash-has-key? config 'llm_config)
    (define llm-config (hash-ref config 'llm_config))
    (when (hash-has-key? llm-config 'provider)
      (hash-set! updates 'llm_provider (hash-ref llm-config 'provider)))
    (when (hash-has-key? llm-config 'model)
      (hash-set! updates 'llm_model (hash-ref llm-config 'model)))
    (when (hash-has-key? llm-config 'temperature)
      (hash-set! updates 'llm_temperature (hash-ref llm-config 'temperature)))
    (when (hash-has-key? llm-config 'max_tokens)
      (hash-set! updates 'llm_max_tokens (hash-ref llm-config 'max_tokens))))

  (when (hash-has-key? config 'system_prompt)
    (hash-set! updates 'system_prompt (hash-ref config 'system_prompt)))

  (when (hash-has-key? config 'memory_config)
    (define mem-config (hash-ref config 'memory_config))
    (when (hash-has-key? mem-config 'core_memory_enabled)
      (hash-set! updates 'core_memory_enabled (hash-ref mem-config 'core_memory_enabled)))
    (when (hash-has-key? mem-config 'archival_memory_enabled)
      (hash-set! updates 'archival_memory_enabled (hash-ref mem-config 'archival_memory_enabled)))
    (when (hash-has-key? mem-config 'max_archival_entries)
      (hash-set! updates 'max_archival_entries (hash-ref mem-config 'max_archival_entries))))

  (agent-update! agent-id updates))

;;; ============================================================================
;;; Agent Memory
;;; ============================================================================

(define (agent-get-memory agent-id)
  "Get agent memory (core memory blocks)"
  (define blocks (db-get-memory-blocks agent-id))
  (define core-memory
    (for/hash ([block (in-list blocks)])
      (values (string->symbol (hash-ref block 'label))
              (hash-ref block 'value))))
  (hash 'core_memory core-memory
        'archival_memory_count 0
        'recall_memory_count 0))

(define (agent-update-memory! agent-id memory)
  "Update agent memory"
  (when (hash-has-key? memory 'core_memory)
    (define core-memory (hash-ref memory 'core_memory))
    (for ([(label value) (in-hash core-memory)])
      (db-update-memory-block agent-id (symbol->string label) value))))

;;; ============================================================================
;;; Agent Statistics
;;; ============================================================================

(define (agent-get-stats agent-id)
  "Get agent statistics"
  (db-get-agent-statistics agent-id))

(define (agent-increment-stat! agent-id stat-name #:amount [amount 1])
  "Increment a statistic"
  (define updates (make-hash))
  (hash-set! updates stat-name amount)
  (db-update-statistics agent-id updates))

;;; ============================================================================
;;; Agent Validation
;;; ============================================================================

(define (agent-exists? agent-id)
  "Check if agent exists"
  (db-agent-exists? agent-id))

(define (agent-validate-config config)
  "Validate agent configuration"
  (define errors '())

  (define errors1
    (if (hash-has-key? config 'name)
        errors
        (cons "Name is required" errors)))

  (define errors2
    (if (hash-has-key? config 'llm_provider)
        (let ([provider (hash-ref config 'llm_provider)])
          (if (member provider '("openai" "anthropic" "groq" "ollama"))
              errors1
              (cons "Invalid LLM provider" errors1)))
        errors1))

  (define errors3
    (if (hash-has-key? config 'llm_temperature)
        (let ([temp (hash-ref config 'llm_temperature)])
          (if (and (number? temp) (>= temp 0.0) (<= temp 2.0))
              errors2
              (cons "Temperature must be between 0.0 and 2.0" errors2)))
        errors2))

  (if (null? errors3)
      #t
      (cons #f errors3)))
