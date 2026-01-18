;;; server/routes/agents.ss - Agent Management Routes
;;;
;;; This module provides REST API endpoints for agent management.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :o/server/http
  :o/server/router)

;;; ============================================================================
;;; Agent CRUD Operations
;;; ============================================================================

(def (create-agent-handler req)
  "POST /v1/agents - Create a new agent"
  (if (not (http-request-json req))
      (make-error-response "JSON body required" status: 400)

      (let ((data (http-request-json req)))
        ;; Validate required fields
        (cond
         ((not (hash-key? data 'name))
          (make-error-response "Field 'name' is required" status: 400))

         (else
          ;; TODO: Actually create agent in database
          (let ((agent-id (generate-agent-id))
                (name (hash-ref data 'name))
                (llm-config (let ((ht (make-hash-table)))
  (hash-put! ht 'llm_config (hash))
  ht))
                (system-prompt (hash-ref data 'system_prompt "You are a helpful assistant."))
                (memory-config (let ((ht (make-hash-table)))
  (hash-put! ht 'memory_config (hash))
  ht)))

            (make-json-response
             (let ((ht (make-hash-table)))
  (hash-put! ht 'id agent-id)
  (hash-put! ht 'name name)
  (hash-put! ht 'llm_config llm-config)
  (hash-put! ht 'system_prompt system-prompt)
  (hash-put! ht 'memory_config memory-config)
  (hash-put! ht 'created_at (current-seconds))
  ht)
             status: 201)))))))

(def (get-agent-handler req)
  "GET /v1/agents/'id - Get agent by ID"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        ;; TODO: Actually fetch agent from database
        (make-json-response
         (let ((ht (make-hash-table)))
  (hash-put! ht 'id agent-id)
  (hash-put! ht 'name (format "Agent ~a" agent-id))
  ht)))))

(def (update-agent-handler req)
  "PATCH /v1/agents/'id - Update agent"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        (if (not (http-request-json req))
            (make-error-response "JSON body required" status: 400)

            (let ((data (http-request-json req)))
              ;; TODO: Actually update agent in database
              (make-json-response
               (let ((ht (make-hash-table)))
  (hash-put! ht 'id agent-id)
  (hash-put! ht 'name (hash-ref data 'name (format "Agent ~a" agent-id)))
  ht)))))))

(def (delete-agent-handler req)
  "DELETE /v1/agents/'id - Delete agent"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        ;; TODO: Actually delete agent from database
        (make-json-response
         (let ((ht (make-hash-table)))
  (hash-put! ht 'deleted #t)
  (hash-put! ht 'id agent-id)
  ht)
         status: 200))))

(def (list-agents-handler req)
  "GET /v1/agents - List all agents"
  (let ((limit (or (get-query-param req "limit") "10"))
        (offset (or (get-query-param req "offset") "0")))

    ;; TODO: Actually fetch agents from database
    (make-json-response
     (let ((ht (make-hash-table)))
  (hash-put! ht 'agents (list
               (hash
                'id "agent-1"
                'name "Agent 1"
                'created_at (current-seconds)))
               (hash ('id "agent-2") ('name "Agent 2") ('created_at (current-seconds))))
  ht))))

;;; ============================================================================
;;; Agent Configuration
;;; ============================================================================

(def (get-agent-config-handler req)
  "GET /v1/agents/:id/config - Get agent configuration"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        ;; TODO: Actually fetch config from database
        (make-json-response
         (let ((ht (make-hash-table)))
  (hash-put! ht 'llm_config (hash
                       'provider "openai"
                       'model "gpt-4"
                       'temperature 0.7
                       'max_tokens 4096))
  ht)))))

(def (update-agent-config-handler req)
  "PATCH /v1/agents/:id/config - Update agent configuration"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        (if (not (http-request-json req))
            (make-error-response "JSON body required" status: 400)

            (let ((data (http-request-json req)))
              ;; TODO: Actually update config in database
              (make-json-response
               (let ((ht (make-hash-table)))
  (hash-put! ht 'updated #t)
  (hash-put! ht 'config data)
  ht)))))))

;;; ============================================================================
;;; Agent Memory
;;; ============================================================================

(def (get-agent-memory-handler req)
  "GET /v1/agents/:id/memory - Get agent memory"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        ;; TODO: Actually fetch memory from database
        (make-json-response
         (let ((ht (make-hash-table)))
  (hash-put! ht 'core_memory (hash
                        'persona "You are a helpful AI assistant."
                        'human "User prefers concise responses."))
  ht)))))

(def (update-agent-memory-handler req)
  "PATCH /v1/agents/:id/memory - Update agent memory"
  (let ((agent-id (get-path-param req 'id)))
    (if (not agent-id)
        (make-error-response "Agent ID required" status: 400)

        (if (not (http-request-json req))
            (make-error-response "JSON body required" status: 400)

            (let ((data (http-request-json req)))
              ;; TODO: Actually update memory in database
              (make-json-response
               (let ((ht (make-hash-table)))
  (hash-put! ht 'updated #t)
  (hash-put! ht 'memory data)
  ht)))))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (generate-agent-id)
  "Generate unique agent ID"
  (string-append
   "agent-"
   (number->string (current-seconds))
   "-"
   (number->string (random-integer 1000000))))

;;; ============================================================================
;;; Route Registration
;;; ============================================================================

(def (register-agent-routes! router)
  "Register all agent routes"

  ;; Agent CRUD
  (post! router "/v1/agents" create-agent-handler)
  (get! router "/v1/agents" list-agents-handler)
  (get! router "/v1/agents/:id" get-agent-handler)
  (patch! router "/v1/agents/:id" update-agent-handler)
  (delete! router "/v1/agents/:id" delete-agent-handler)

  ;; Agent configuration
  (get! router "/v1/agents/:id/config" get-agent-config-handler)
  (patch! router "/v1/agents/:id/config" update-agent-config-handler)

  ;; Agent memory
  (get! router "/v1/agents/:id/memory" get-agent-memory-handler)
  (patch! router "/v1/agents/:id/memory" update-agent-memory-handler)

  router)

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create router and register agent routes
(def router (make-router-instance))
(register-agent-routes! router)

;; Test with curl:

# Create agent
curl -X POST http://localhost:8283/v1/agents \
  -H "Content-Type: application/json" \
  -d '{
    "name": "MyAgent",
    "llm_config": {
      "provider": "openai",
      "model": "gpt-4"
    },
    "system_prompt": "You are a helpful assistant."
  }'

# List agents
curl http://localhost:8283/v1/agents

# Get agent
curl http://localhost:8283/v1/agents/agent-123

# Update agent
curl -X PATCH http://localhost:8283/v1/agents/agent-123 \
  -H "Content-Type: application/json" \
  -d '{
    "name": "UpdatedAgent",
    "system_prompt": "You are a very helpful assistant."
  }'

# Delete agent
curl -X DELETE http://localhost:8283/v1/agents/agent-123

# Get agent config
curl http://localhost:8283/v1/agents/agent-123/config

# Get agent memory
curl http://localhost:8283/v1/agents/agent-123/memory
|#
