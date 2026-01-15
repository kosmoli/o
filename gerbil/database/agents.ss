;;; database/agents.ss - Agent Database Operations
;;;
;;; High-level agent database operations built on top of the database client.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  ./client)

;;; ============================================================================
;;; Agent Management
;;; ============================================================================

(def (agent-create! name
                    #!key
                    (llm-provider "openai")
                    (llm-model "gpt-4")
                    (llm-temperature 0.7)
                    (llm-max-tokens 4096)
                    (system-prompt "You are a helpful assistant.")
                    (persona "You are a helpful AI assistant.")
                    (human "User information will be stored here."))
  "Create a new agent with default memory blocks"

  ;; Create agent
  (let* ((agent (db-create-agent
                 (hash 'name name
                       'llm_provider llm-provider
                       'llm_model llm-model
                       'llm_temperature llm-temperature
                       'llm_max_tokens llm-max-tokens
                       'system_prompt system-prompt)))
         (agent-id (hash-ref agent 'id)))

    ;; Initialize memory blocks (already done by Elixir, but can override)
    (when (or persona human)
      (db-initialize-agent-memory agent-id persona human))

    (displayln (format "Agent created: ~a (~a)" name agent-id))
    agent))

(def (agent-get agent-id)
  "Get agent by ID"
  (db-get-agent agent-id))

(def (agent-update! agent-id updates)
  "Update agent configuration"
  (db-update-agent agent-id updates))

(def (agent-delete! agent-id)
  "Delete agent (soft delete)"
  (db-delete-agent agent-id)
  (displayln (format "Agent deleted: ~a" agent-id)))

(def (agent-list #!key (limit 10) (offset 0))
  "List agents"
  (db-list-agents limit: limit offset: offset))

;;; ============================================================================
;;; Agent Configuration
;;; ============================================================================

(def (agent-get-config agent-id)
  "Get agent configuration"
  (let ((agent (agent-get agent-id)))
    (hash
     'llm_config (hash
                  'provider (hash-ref agent 'llm_provider)
                  'model (hash-ref agent 'llm_model)
                  'temperature (hash-ref agent 'llm_temperature)
                  'max_tokens (hash-ref agent 'llm_max_tokens))
     'system_prompt (hash-ref agent 'system_prompt)
     'memory_config (hash
                     'core_memory_enabled (hash-ref agent 'core_memory_enabled)
                     'archival_memory_enabled (hash-ref agent 'archival_memory_enabled)
                     'max_archival_entries (hash-ref agent 'max_archival_entries)))))

(def (agent-update-config! agent-id config)
  "Update agent configuration"
  (let ((updates (hash)))

    ;; Update LLM config
    (when (hash-key? config 'llm_config)
      (let ((llm-config (hash-ref config 'llm_config)))
        (when (hash-key? llm-config 'provider)
          (hash-put! updates 'llm_provider (hash-ref llm-config 'provider)))
        (when (hash-key? llm-config 'model)
          (hash-put! updates 'llm_model (hash-ref llm-config 'model)))
        (when (hash-key? llm-config 'temperature)
          (hash-put! updates 'llm_temperature (hash-ref llm-config 'temperature)))
        (when (hash-key? llm-config 'max_tokens)
          (hash-put! updates 'llm_max_tokens (hash-ref llm-config 'max_tokens)))))

    ;; Update system prompt
    (when (hash-key? config 'system_prompt)
      (hash-put! updates 'system_prompt (hash-ref config 'system_prompt)))

    ;; Update memory config
    (when (hash-key? config 'memory_config)
      (let ((mem-config (hash-ref config 'memory_config)))
        (when (hash-key? mem-config 'core_memory_enabled)
          (hash-put! updates 'core_memory_enabled (hash-ref mem-config 'core_memory_enabled)))
        (when (hash-key? mem-config 'archival_memory_enabled)
          (hash-put! updates 'archival_memory_enabled (hash-ref mem-config 'archival_memory_enabled)))
        (when (hash-key? mem-config 'max_archival_entries)
          (hash-put! updates 'max_archival_entries (hash-ref mem-config 'max_archival_entries)))))

    (agent-update! agent-id updates)))

;;; ============================================================================
;;; Agent Memory
;;; ============================================================================

(def (agent-get-memory agent-id)
  "Get agent memory (core memory blocks)"
  (let ((blocks (db-get-memory-blocks agent-id)))
    (hash
     'core_memory (list->hash
                   (map (lambda (block)
                          (cons (string->symbol (hash-ref block 'label))
                                (hash-ref block 'value)))
                        blocks))
     'archival_memory_count 0  ;; TODO: Get actual count
     'recall_memory_count 0)))

(def (agent-update-memory! agent-id memory)
  "Update agent memory"
  (when (hash-key? memory 'core_memory)
    (let ((core-memory (hash-ref memory 'core_memory)))
      (hash-for-each
       (lambda (label value)
         (db-update-memory-block agent-id (symbol->string label) value))
       core-memory))))

;;; ============================================================================
;;; Agent Statistics
;;; ============================================================================

(def (agent-get-stats agent-id)
  "Get agent statistics"
  (db-get-agent-statistics agent-id))

(def (agent-increment-stat! agent-id stat-name #!optional (amount 1))
  "Increment a statistic"
  (let ((updates (hash stat-name amount)))
    (db-update-statistics agent-id updates)))

;;; ============================================================================
;;; Agent Validation
;;; ============================================================================

(def (agent-exists? agent-id)
  "Check if agent exists"
  (db-agent-exists? agent-id))

(def (agent-validate-config config)
  "Validate agent configuration"
  (let ((errors '()))

    ;; Validate name
    (unless (hash-key? config 'name)
      (set! errors (cons "Name is required" errors)))

    ;; Validate LLM provider
    (when (hash-key? config 'llm_provider)
      (let ((provider (hash-ref config 'llm_provider)))
        (unless (member provider '("openai" "anthropic" "groq" "ollama"))
          (set! errors (cons "Invalid LLM provider" errors)))))

    ;; Validate temperature
    (when (hash-key? config 'llm_temperature)
      (let ((temp (hash-ref config 'llm_temperature)))
        (unless (and (number? temp) (>= temp 0.0) (<= temp 2.0))
          (set! errors (cons "Temperature must be between 0.0 and 2.0" errors)))))

    ;; Return validation result
    (if (null? errors)
        #t
        (cons #f errors))))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Connect to database
(db-connect!)

;; Create agent
(def agent (agent-create! "MyAgent"
                          llm-provider: "openai"
                          llm-model: "gpt-4"
                          system-prompt: "You are a helpful assistant."
                          persona: "You are a helpful AI assistant."
                          human: "User prefers concise responses."))

(def agent-id (hash-ref agent 'id))

;; Get agent
(def agent (agent-get agent-id))

;; Update agent config
(agent-update-config! agent-id
                      (hash 'llm_config (hash 'temperature 0.8)
                            'system_prompt "You are a very helpful assistant."))

;; Get agent memory
(def memory (agent-get-memory agent-id))

;; Update agent memory
(agent-update-memory! agent-id
                      (hash 'core_memory
                            (hash 'persona "You are a very helpful AI assistant."
                                  'human "User prefers detailed explanations.")))

;; Get agent statistics
(def stats (agent-get-stats agent-id))

;; List agents
(def agents (agent-list limit: 10))

;; Delete agent
(agent-delete! agent-id)
|#
