#lang racket

;;; server/routes/agents.rkt - Agent Management Routes
;;;
;;; This module provides REST API endpoints for agent management.

(provide create-agent-handler
         get-agent-handler
         update-agent-handler
         delete-agent-handler
         list-agents-handler
         get-agent-config-handler
         update-agent-config-handler
         get-agent-memory-handler
         update-agent-memory-handler
         register-agent-routes!)

(require racket/hash
        racket/format
        "../http.rkt"
        "../router.rkt"
        "../../database/agents.rkt")

;;; ============================================================================
;;; Agent CRUD Operations
;;; ============================================================================

(define (create-agent-handler req)
  "POST /v1/agents - Create a new agent"
  (if (not (http-request-json req))
      (make-error-response "JSON body required" #:status 400)
      (let ([data (http-request-json req)])
        ;; Validate required fields
        (cond
         [(not (hash-has-key? data 'name))
          (make-error-response "Field 'name' is required" #:status 400)]
         [else
          ;; Create agent using database layer
          (define name (hash-ref data 'name))
          (define llm-provider (hash-ref (hash-ref data 'llm_config (hash)) 'provider "openai"))
          (define llm-model (hash-ref (hash-ref data 'llm_config (hash)) 'model "gpt-4"))
          (define llm-temperature (hash-ref (hash-ref data 'llm_config (hash)) 'temperature 0.7))
          (define llm-max-tokens (hash-ref (hash-ref data 'llm_config (hash)) 'max_tokens 4096))
          (define system-prompt (hash-ref data 'system_prompt "You are a helpful assistant."))
          (define persona (hash-ref (hash-ref data 'memory_config (hash)) 'persona #f))
          (define human (hash-ref (hash-ref data 'memory_config (hash)) 'human #f))

          (define agent (agent-create! name
                                       #:llm-provider llm-provider
                                       #:llm-model llm-model
                                       #:llm-temperature llm-temperature
                                       #:llm-max-tokens llm-max-tokens
                                       #:system-prompt system-prompt
                                       #:persona persona
                                       #:human human))

          (make-json-response
           (hash 'id (hash-ref agent 'id)
                 'name name
                 'llm_config (hash 'provider llm-provider
                                   'model llm-model
                                   'temperature llm-temperature
                                   'max_tokens llm-max-tokens)
                 'system_prompt system-prompt
                 'memory_config (hash 'persona persona
                                     'human human)
                 'created_at (hash-ref agent 'created_at)
                 'updated_at (hash-ref agent 'updated_at))
           #:status 201)]))))

(define (get-agent-handler req)
  "GET /v1/agents/:id - Get agent by ID"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      (let ([agent (agent-get agent-id)])
        (if agent
            (make-json-response agent)
            (make-error-response "Agent not found" #:status 404)))))

(define (update-agent-handler req)
  "PATCH /v1/agents/:id - Update agent"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      (if (not (http-request-json req))
          (make-error-response "JSON body required" #:status 400)
          (let ([data (http-request-json req)])
            (define updated (agent-update! agent-id data))
            (if updated
                (make-json-response updated)
                (make-error-response "Failed to update agent" #:status 500))))))

(define (delete-agent-handler req)
  "DELETE /v1/agents/:id - Delete agent"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      (let ([result (agent-delete! agent-id)])
        (if result
            (make-json-response (hash 'deleted #t
                                       'id agent-id))
            (make-error-response "Failed to delete agent" #:status 500)))))

(define (list-agents-handler req)
  "GET /v1/agents - List all agents"
  (define limit-str (hash-ref (http-request-query-params req) "limit" "10"))
  (define offset-str (hash-ref (http-request-query-params req) "offset" "0"))
  (define limit (string->number limit-str))
  (define offset (string->number offset-str))

  (define agents (agent-list #:limit limit #:offset offset))
  (make-json-response agents))

;;; ============================================================================
;;; Agent Configuration
;;; ============================================================================

(define (get-agent-config-handler req)
  "GET /v1/agents/:id/config - Get agent configuration"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      (let ([agent (agent-get agent-id)])
        (if agent
            (make-json-response
             (hash 'llm_config (hash-ref agent 'llm_config (hash))
                   'memory_config (hash-ref agent 'memory_config (hash))
                   'tool_config (hash-ref agent 'tool_config (hash))))
            (make-error-response "Agent not found" #:status 404)))))

(define (update-agent-config-handler req)
  "PATCH /v1/agents/:id/config - Update agent configuration"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      (if (not (http-request-json req))
          (make-error-response "JSON body required" #:status 400)
          (let ([data (http-request-json req)])
            ;; Update configuration
            (define updated (agent-update! agent-id data))
            (if updated
                (make-json-response (hash 'updated #t
                                            'config data))
                (make-error-response "Failed to update config" #:status 500))))))

;;; ============================================================================
;;; Agent Memory
;;; ============================================================================

(define (get-agent-memory-handler req)
  "GET /v1/agents/:id/memory - Get agent memory"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      ;; TODO: Fetch memory from database
      (make-json-response
       (hash 'core_memory (hash 'persona "You are a helpful AI assistant."
                                'human "User information.")
             'archival_memory_count 0
             'recall_memory_count 0))))

(define (update-agent-memory-handler req)
  "PATCH /v1/agents/:id/memory - Update agent memory"
  (define agent-id (get-path-param req 'id))
  (if (not agent-id)
      (make-error-response "Agent ID required" #:status 400)
      (if (not (http-request-json req))
          (make-error-response "JSON body required" #:status 400)
          (let ([data (http-request-json req)])
            ;; TODO: Update memory in database
            (make-json-response (hash 'updated #t
                                        'memory data))))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (generate-agent-id)
  "Generate unique agent ID"
  (format "agent-~a-~a"
          (current-seconds)
          (random 1000000)))

;;; ============================================================================
;;; Route Registration
;;; ============================================================================

(define (register-agent-routes! router)
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
