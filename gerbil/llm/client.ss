;;; llm/client.ss - Unified LLM Client Interface
;;;
;;; This module provides a unified interface for all LLM providers,
;;; allowing easy switching between OpenAI, Anthropic, Groq, and Ollama.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :o/llm/types
  :o/llm/openai
  :o/llm/anthropic
  :o/llm/groq
  :o/llm/ollama)

;;; ============================================================================
;;; Provider Registry
;;; ============================================================================

(def *default-models*
  (let ((ht (make-hash-table)))
    (hash-put! ht 'openai "gpt-4")
    (hash-put! ht 'anthropic "claude-3-5-sonnet-20241022")
    (hash-put! ht 'groq "llama-3.3-70b-versatile")
    (hash-put! ht 'ollama "llama3.2:latest")
    ht))

(def (get-default-model provider)
  "Get default model for a provider"
  (hash-ref *default-models* provider))

;;; ============================================================================
;;; Unified Chat Completion Interface
;;; ============================================================================

(def (llm-chat-completion
      provider
      messages . rest
      (model #f)
      (api-key #f)
      (temperature 0.7)
      (max-tokens #f)
      (system #f)
      (tools #f)
      (tool-choice #f)
      (endpoint #f))
  "Unified chat completion interface for all providers

   Parameters:
   - provider: 'openai 'anthropic 'groq 'ollama
   - messages: list of llm-message structures or hashes
   - model: model name (uses default if not specified)
   - api-key: API key (uses env var if not specified)
   - temperature: sampling temperature
   - max-tokens: maximum tokens to generate
   - system: system prompt (for Anthropic)
   - tools: list of tool-definition structures
   - tool-choice: tool choice strategy
   - endpoint: custom endpoint (for Ollama)

   Returns: llm-response structure"

  (let ((model-name (or model (get-default-model provider))))

    (case provider
      ;; OpenAI
      (('openai)
       (let ((msgs (if system
                       (cons (make-system-message system) messages)
                       messages)))
         (openai-chat-completion
          msgs
          model: model-name
          api-key: api-key
          temperature: temperature
          max-tokens: max-tokens
          tools: tools
          tool-choice: tool-choice)))

      ;; Anthropic
      (('anthropic)
       (anthropic-messages
        messages
        model: model-name
        api-key: api-key
        max-tokens: (or max-tokens 4096)  ;; Anthropic requires max-tokens
        temperature: temperature
        system: system
        tools: tools))

      ;; Groq
      (('groq)
       (let ((msgs (if system
                       (cons (make-system-message system) messages)
                       messages)))
         (groq-chat-completion
          msgs
          model: model-name
          api-key: api-key
          temperature: temperature
          max-tokens: max-tokens
          tools: tools
          tool-choice: tool-choice)))

      ;; Ollama
      (('ollama)
       (let ((msgs (if system
                       (cons (make-system-message system) messages)
                       messages)))
         (ollama-chat-completion
          msgs
          model: model-name
          endpoint: (or endpoint "http://localhost:11434")
          temperature: temperature
          max-tokens: max-tokens
          tools: tools)))

      ;; Unknown provider
      (else
       (error "Unknown LLM provider" provider)))))

;;; ============================================================================
;;; Simple Chat Interface
;;; ============================================================================

(def (llm-simple
      provider
      prompt . rest
      (model #f)
      (system-prompt "You are a helpful assistant.")
      (api-key #f)
      (endpoint #f))
  "Simple chat interface for all providers

   Parameters:
   - provider: 'openai 'anthropic 'groq 'ollama
   - prompt: user prompt string
   - model: model name (uses default if not specified)
   - system-prompt: system prompt
   - api-key: API key (uses env var if not specified)
   - endpoint: custom endpoint (for Ollama)

   Returns: string content of response"

  (let ((messages (list (make-user-message prompt))))

    (case provider
      (('openai)
       (openai-simple prompt
                      model: (or model (get-default-model 'openai))
                      system-prompt: system-prompt
                      api-key: api-key))

      (('anthropic)
       (anthropic-simple prompt
                         model: (or model (get-default-model 'anthropic))
                         system-prompt: system-prompt
                         max-tokens: 4096
                         api-key: api-key))

      (('groq)
       (groq-simple prompt
                    model: (or model (get-default-model 'groq))
                    system-prompt: system-prompt
                    api-key: api-key))

      (('ollama)
       (ollama-simple prompt
                      model: (or model (get-default-model 'ollama))
                      system-prompt: system-prompt
                      endpoint: (or endpoint "http://localhost:11434")))

      (else
       (error "Unknown LLM provider" provider)))))

;;; ============================================================================
;;; Tool-enabled Chat Interface
;;; ============================================================================

(def (llm-with-tools
      provider
      prompt
      tools . rest
      (model #f)
      (system-prompt "You are a helpful assistant.")
      (api-key #f)
      (endpoint #f))
  "Tool-enabled chat interface for all providers

   Parameters:
   - provider: 'openai 'anthropic 'groq 'ollama
   - prompt: user prompt string
   - tools: list of tool-definition structures
   - model: model name (uses default if not specified)
   - system-prompt: system prompt
   - api-key: API key (uses env var if not specified)
   - endpoint: custom endpoint (for Ollama)

   Returns: llm-response structure (check for tool calls)"

  (case provider
    (('openai)
     (openai-with-tools prompt tools
                        model: (or model (get-default-model 'openai))
                        system-prompt: system-prompt
                        api-key: api-key))

    (('anthropic)
     (anthropic-with-tools prompt tools
                           model: (or model (get-default-model 'anthropic))
                           system-prompt: system-prompt
                           max-tokens: 4096
                           api-key: api-key))

    (('groq)
     (groq-with-tools prompt tools
                      model: (or model (get-default-model 'groq))
                      system-prompt: system-prompt
                      api-key: api-key))

    (('ollama)
     (ollama-with-tools prompt tools
                        model: (or model (get-default-model 'ollama))
                        system-prompt: system-prompt
                        endpoint: (or endpoint "http://localhost:11434")))

    (else
     (error "Unknown LLM provider" provider))))

;;; ============================================================================
;;; Provider Configuration
;;; ============================================================================

(defstruct llm-config
  (provider      ; 'openai 'anthropic 'groq 'ollama
   model         ; model name
   api-key       ; API key (optional)
   temperature   ; sampling temperature
   max-tokens    ; max tokens to generate
   endpoint)     ; custom endpoint (for Ollama)
  transparent: #t)

(def (make-llm-config-instance
      provider . rest
      (model #f)
      (api-key #f)
      (temperature 0.7)
      (max-tokens #f)
      (endpoint #f))
  "Create an LLM configuration"
  (make-llm-config
   provider: provider
   model: (or model (get-default-model provider))
   api-key: api-key
   temperature: temperature
   max-tokens: max-tokens
   endpoint: endpoint))

(def (llm-chat-with-config
      config
      messages . rest
      (system #f)
      (tools #f)
      (tool-choice #f))
  "Chat completion using a configuration object

   Parameters:
   - config: llm-config structure
   - messages: list of llm-message structures
   - system: system prompt (optional)
   - tools: list of tool-definition structures (optional)
   - tool-choice: tool choice strategy (optional)

   Returns: llm-response structure"

  (llm-chat-completion
   (llm-config-provider config)
   messages
   model: (llm-config-model config)
   api-key: (llm-config-api-key config)
   temperature: (llm-config-temperature config)
   max-tokens: (llm-config-max-tokens config)
   system: system
   tools: tools
   tool-choice: tool-choice
   endpoint: (llm-config-endpoint config)))

;;; ============================================================================
;;; Provider Utilities
;;; ============================================================================

(def (provider-supports-tools? provider)
  "Check if provider supports tool calling"
  (member provider '('openai 'anthropic 'groq 'ollama)))

(def (provider-requires-max-tokens? provider)
  "Check if provider requires max-tokens parameter"
  (eq? provider 'anthropic))

(def (provider-supports-streaming? provider)
  "Check if provider supports streaming responses"
  (member provider '('openai 'anthropic 'groq 'ollama)))

(def (validate-provider provider)
  "Validate provider name"
  (unless (member provider '('openai 'anthropic 'groq 'ollama))
    (error "Invalid provider" provider)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Simple usage with different providers
(def response1 (llm-simple 'openai "Why is the sky blue?"))
(def response2 (llm-simple 'anthropic "Why is the sky blue?"))
(def response3 (llm-simple 'groq "Why is the sky blue?"))
(def response4 (llm-simple 'ollama "Why is the sky blue?"))

;; Using configuration object
(def config (make-llm-config-instance
             'openai
             model: "gpt-4"
             temperature: 0.5))

(def messages (list (make-user-message "Hello!")))
(def response (llm-chat-with-config config messages))

;; With tools
(def calculator-tool
  (make-tool-definition-instance
   "calculate"
   "Perform a mathematical calculation"
   (let ((ht (make-hash-table)))
  (hash-put! ht 'type "object")
  (hash-put! ht 'properties (hash
                 'expression (hash
                              'type "string"
                              'description "Mathematical expression to evaluate")))
  ht)))

(def response (llm-with-tools
               'openai
               "What is 25 * 4?"
               (list calculator-tool)))

(when (has-tool-calls? (llm-response-message response))
  (displayln "Tool calls requested:")
  (for-each
   (lambda (tc)
     (displayln (format "  - ~a(~a)"
                       (function-call-name (tool-call-function tc))
                       (function-call-arguments (tool-call-function tc)))))
   (llm-message-tool-calls (llm-response-message response))))
|#
