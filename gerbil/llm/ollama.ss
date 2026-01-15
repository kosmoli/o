;;; llm/ollama.ss - Ollama API Client
;;;
;;; This module provides a client for Ollama's local LLM API.
;;; Ollama runs models locally and provides a simple HTTP API.

(export #t)

(import
  :std/net/request
  :std/text/json
  :std/sugar
  :std/misc/hash
  ./types)

;;; ============================================================================
;;; Ollama Chat Completion
;;; ============================================================================

(def (ollama-chat-completion
      messages
      #!key
      (model "llama3.2:latest")
      (endpoint "http://localhost:11434")
      (temperature 0.7)
      (max-tokens #f)
      (tools #f))
  "Call Ollama chat API

   Parameters:
   - messages: list of llm-message structures or hashes
   - model: model name (default: llama3.2:latest)
   - endpoint: Ollama server endpoint (default: http://localhost:11434)
   - temperature: sampling temperature (0-2)
   - max-tokens: maximum tokens to generate
   - tools: list of tool-definition structures (if supported by model)

   Returns: llm-response structure"

  (let* ((headers '(("Content-Type" . "application/json")))

         ;; Convert messages to API format
         (api-messages
          (map (lambda (msg)
                 (if (llm-message? msg)
                     (message->hash msg)
                     msg))  ;; Already a hash
               messages))

         ;; Build request body
         (body-data (hash
                     'model model
                     'messages api-messages
                     'stream #f
                     'options (hash 'temperature temperature)))

         ;; Add optional parameters
         (_ (begin
              (when max-tokens
                (hash-put! (hash-ref body-data 'options)
                          'num_predict max-tokens))
              (when tools
                (hash-put! body-data 'tools
                          (map tool-definition->hash tools)))))

         (body-string (json-object->string body-data))
         (chat-endpoint (string-append endpoint "/api/chat")))

    ;; Make API request
    (let ((response (http-post chat-endpoint headers: headers data: body-string)))
      (if (= (request-status response) 200)
          (parse-ollama-response response)
          (make-llm-error
           type: :api-error
           message: "Ollama API request failed"
           status: (request-status response)
           provider: :ollama
           details: (request-text response))))))

;;; ============================================================================
;;; Ollama Generate (Simple API)
;;; ============================================================================

(def (ollama-generate
      prompt
      #!key
      (model "llama3.2:latest")
      (endpoint "http://localhost:11434")
      (temperature 0.7)
      (max-tokens #f)
      (system-prompt #f))
  "Call Ollama generate API (simpler interface)

   Parameters:
   - prompt: user prompt string
   - model: model name (default: llama3.2:latest)
   - endpoint: Ollama server endpoint
   - temperature: sampling temperature
   - max-tokens: maximum tokens to generate
   - system-prompt: optional system prompt

   Returns: llm-response structure"

  (let* ((headers '(("Content-Type" . "application/json")))

         ;; Build request body
         (body-data (hash
                     'model model
                     'prompt prompt
                     'stream #f
                     'options (hash 'temperature temperature)))

         ;; Add optional parameters
         (_ (begin
              (when system-prompt
                (hash-put! body-data 'system system-prompt))
              (when max-tokens
                (hash-put! (hash-ref body-data 'options)
                          'num_predict max-tokens))))

         (body-string (json-object->string body-data))
         (generate-endpoint (string-append endpoint "/api/generate")))

    ;; Make API request
    (let ((response (http-post generate-endpoint headers: headers data: body-string)))
      (if (= (request-status response) 200)
          (parse-ollama-generate-response response)
          (make-llm-error
           type: :api-error
           message: "Ollama API request failed"
           status: (request-status response)
           provider: :ollama
           details: (request-text response))))))

;;; ============================================================================
;;; Response Parsing
;;; ============================================================================

(def (parse-ollama-response response)
  "Parse Ollama chat API response into llm-response structure"
  (let* ((response-json (request-json response))
         (model (hash-ref response-json 'model))
         (created-at (hash-ref response-json 'created_at))
         (message-hash (hash-ref response-json 'message))
         (done (hash-ref response-json 'done))

         ;; Extract usage stats if available
         (prompt-eval-count (hash-ref response-json 'prompt_eval_count 0))
         (eval-count (hash-ref response-json 'eval_count 0)))

    ;; Build llm-response
    (make-llm-response
     id: (string-append "ollama-" (number->string (current-seconds)))
     model: model
     message: (hash->message message-hash)
     finish-reason: (if done :stop :unknown)
     usage: (make-usage-stats
             prompt-tokens: prompt-eval-count
             completion-tokens: eval-count
             total-tokens: (+ prompt-eval-count eval-count))
     created: (current-seconds)
     provider: :ollama)))

(def (parse-ollama-generate-response response)
  "Parse Ollama generate API response into llm-response structure"
  (let* ((response-json (request-json response))
         (model (hash-ref response-json 'model))
         (response-text (hash-ref response-json 'response))
         (done (hash-ref response-json 'done))

         ;; Extract usage stats if available
         (prompt-eval-count (hash-ref response-json 'prompt_eval_count 0))
         (eval-count (hash-ref response-json 'eval_count 0)))

    ;; Build llm-response
    (make-llm-response
     id: (string-append "ollama-" (number->string (current-seconds)))
     model: model
     message: (make-assistant-message response-text)
     finish-reason: (if done :stop :unknown)
     usage: (make-usage-stats
             prompt-tokens: prompt-eval-count
             completion-tokens: eval-count
             total-tokens: (+ prompt-eval-count eval-count))
     created: (current-seconds)
     provider: :ollama)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(def (ollama-simple prompt
                    #!key
                    (model "llama3.2:latest")
                    (system-prompt "You are a helpful assistant.")
                    (endpoint "http://localhost:11434"))
  "Simple Ollama chat with just a prompt

   Returns: string content of response"

  (let* ((messages (list
                    (make-system-message system-prompt)
                    (make-user-message prompt)))
         (response (ollama-chat-completion
                    messages
                    model: model
                    endpoint: endpoint)))

    (if (llm-response? response)
        (extract-text-content (llm-response-message response))
        (error "Ollama request failed" response))))

(def (ollama-with-tools prompt tools
                        #!key
                        (model "llama3.2:latest")
                        (system-prompt "You are a helpful assistant.")
                        (endpoint "http://localhost:11434"))
  "Ollama chat with tool support (if model supports it)

   Returns: llm-response structure (check for tool calls)"

  (let ((messages (list
                   (make-system-message system-prompt)
                   (make-user-message prompt))))

    (ollama-chat-completion
     messages
     model: model
     tools: tools
     endpoint: endpoint)))

;;; ============================================================================
;;; Model Management
;;; ============================================================================

(def (ollama-list-models #!key (endpoint "http://localhost:11434"))
  "List available Ollama models

   Returns: list of model names"

  (let* ((list-endpoint (string-append endpoint "/api/tags"))
         (response (http-get list-endpoint)))

    (if (= (request-status response) 200)
        (let* ((response-json (request-json response))
               (models (hash-ref response-json 'models)))
          (map (lambda (model) (hash-ref model 'name)) models))
        (error "Failed to list Ollama models"
               status: (request-status response)))))

(def (ollama-pull-model model #!key (endpoint "http://localhost:11434"))
  "Pull/download an Ollama model

   Parameters:
   - model: model name to pull (e.g., 'llama3.2:latest')

   Returns: #t on success"

  (let* ((headers '(("Content-Type" . "application/json")))
         (body-data (hash 'name model 'stream #f))
         (body-string (json-object->string body-data))
         (pull-endpoint (string-append endpoint "/api/pull")))

    (let ((response (http-post pull-endpoint headers: headers data: body-string)))
      (if (= (request-status response) 200)
          #t
          (error "Failed to pull Ollama model"
                 model: model
                 status: (request-status response))))))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Simple usage
(def response (ollama-simple "Why is the sky blue? Be concise."))
(displayln response)

;; Using generate API
(def response (ollama-generate "Why is the sky blue? Be concise."
                               system-prompt: "You are a helpful assistant."))
(displayln (extract-text-content (llm-response-message response)))

;; List available models
(def models (ollama-list-models))
(displayln "Available models:")
(for-each displayln models)

;; Pull a model
(ollama-pull-model "llama3.2:latest")
|#
