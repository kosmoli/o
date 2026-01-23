#lang racket

;;; llm/ollama.rkt - Ollama API Client
;;;
;;; This module provides a client for Ollama's local inference API.

(provide ollama-chat-completion
         ollama-simple
         ollama-with-tools
         ollama-list-models
         ollama-show-model
         ollama-embed
         ollama-message->hash
         ollama-parse-response)

(require net/http-easy
         json
         racket/hash
         racket/format
         racket/match
         "./types.rkt")

;;; ============================================================================
;;; Ollama Chat Completion
;;; ============================================================================

(define (ollama-chat-completion messages
                                #:model [model "llama3.2"]
                                #:base-url [base-url "http://localhost:11434"]
                                #:temperature [temperature 0.7]
                                #:max-tokens [max-tokens 4096]
                                #:tools [tools #f]
                                #:stream [stream #f])
  "Call Ollama chat completions API

   Parameters:
   - messages: list of llm-message structures or hashes
   - model: model name (default: llama3.2)
   - base-url: Ollama server URL (default: http://localhost:11434)
   - temperature: sampling temperature (0-2)
   - max-tokens: maximum tokens to generate
   - tools: list of tool-definition structures (limited support)
   - stream: whether to stream response

   Returns: llm-response structure"

  (define endpoint (string-append base-url "/api/chat"))

  ;; Convert messages to API format
  (define api-messages
    (map (lambda (msg)
           (if (llm-message? msg)
               (message->ollama-format msg)
               msg))
         messages))

  ;; Build request body
  (define body-data
    (hash 'model model
          'messages api-messages
          'stream stream
          'options (hash
                   'temperature temperature
                   'num_predict max-tokens)))

  ;; Add tools if provided (limited support in Ollama)
  (define body-data-final
    (if tools
        (hash-set body-data 'tools
                  (map tool-definition->ollama-format tools))
        body-data))

  ;; Make API request
  (define response
    (post endpoint
         #:headers (hash 'content-type "application/json")
         #:data (jsexpr->string body-data-final)))

  (define response-body (response-body response))

  (if (= (response-status-code response) 200)
      (parse-ollama-response (string->jsexpr response-body))
      (llm-error 'api-error
                "Ollama API request failed"
                (response-status-code response)
                'ollama
                response-body)))

;;; ============================================================================
;;; Format Conversion
;;; ============================================================================

(define (message->ollama-format msg)
  "Convert llm-message to Ollama API format"
  (define role (llm-message-role msg))
  (define content (llm-message-content msg))
  (define tool-calls (llm-message-tool-calls msg))

  (cond
   [(eq? role 'system)
    ;; Ollama treats system as user with specific prefix
    (hash 'role "user"
          'content (format "System: ~a" content))]

   [(eq? role 'user)
    (hash 'role "user"
          'content content)]

   [(eq? role 'assistant)
    (hash 'role "assistant"
          'content content)]

   [(eq? role 'tool)
    ;; Tool results as user messages
    (hash 'role "user"
          'content (format "Tool result: ~a" content))]

   [else
    (error 'message->ollama-format "Unsupported message role" role)]))

(define (tool-definition->ollama-format td)
  "Convert tool-definition to Ollama format (native function calling)"
  (define func (tool-definition-function td))
  (hash 'type "function"
        'function (hash 'name (function-definition-name func)
                        'description (function-definition-description func)
                        'parameters (function-definition-parameters func))))

;;; ============================================================================
;;; Response Parsing
;;; ============================================================================

(define (parse-ollama-response response-data)
  "Parse Ollama API response into llm-response structure"
  (define model (hash-ref response-data 'model ""))
  (define created (hash-ref response-data 'created_at 0))
  (define message (hash-ref response-data 'message (hash)))
  (define done (hash-ref response-data 'done #t))

  ;; Extract message content
  (define role-str (hash-ref message 'role "assistant"))
  (define content (hash-ref message 'content ""))

  ;; Build llm-response
  (llm-response
   (format "ollama-~a" (substring (number->string (current-milliseconds)) 0 8))
   model
   (llm-message (string->symbol role-str)
                content
                #f
                #f  ; Ollama tool calls limited support
                #f)
   (if done 'stop 'unknown)
   (usage-stats (hash-ref response-data 'prompt_eval_count 0)
                (hash-ref response-data 'eval_count 0)
                (+ (hash-ref response-data 'prompt_eval_count 0)
                   (hash-ref response-data 'eval_count 0)))
   created
   'ollama))

;;; ============================================================================
;;; Model Management
;;; ============================================================================

(define (ollama-list-models #:base-url [base-url "http://localhost:11434"])
  "List available Ollama models

   Returns: list of model hashes"
  (define endpoint (string-append base-url "/api/tags"))
  (define response
    (get endpoint
        #:headers (hash 'content-type "application/json")))

  (if (= (response-status-code response) 200)
      (hash-ref (response-json response) 'models '())
      (error 'ollama-list-models "Failed to list models")))

(define (ollama-show-model model-name
                           #:base-url [base-url "http://localhost:11434"])
  "Show details of a specific model

   Returns: model details hash"
  (define endpoint (string-append base-url "/api/show"))
  (define response
    (post endpoint
         #:headers (hash 'content-type "application/json")
         #:data (jsexpr->string (hash 'name model-name))))

  (if (= (response-status-code response) 200)
      (response-json response)
      (error 'ollama-show-model "Failed to show model")))

;;; ============================================================================
;;; Embeddings
;;; ============================================================================

(define (ollama-embed text
                      #:model [model "nomic-embed-text"]
                      #:base-url [base-url "http://localhost:11434"])
  "Get embeddings for text using Ollama

   Returns: list of embedding values"
  (define endpoint (string-append base-url "/api/embeddings"))

  (define response
    (post endpoint
         #:headers (hash 'content-type "application/json")
         #:data (jsexpr->string (hash 'model model
                                       'input text))))

  (if (= (response-status-code response) 200)
      (hash-ref (response-json response) 'embedding '())
      (error 'ollama-embed "Failed to get embeddings")))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(define (ollama-simple prompt
                       #:model [model "llama3.2"]
                       #:system-prompt [system-prompt "You are a helpful assistant."]
                       #:base-url [base-url "http://localhost:11434"])
  "Simple Ollama chat with just a prompt

   Returns: string content of response"

  (define messages
    (list (make-system-message system-prompt)
          (make-user-message prompt)))

  (define response
    (ollama-chat-completion messages
                            #:model model
                            #:base-url base-url))

  (if (llm-response? response)
      (extract-text-content (llm-response-message response))
      (error 'ollama-simple "Request failed" response)))

(define (ollama-with-tools prompt tools
                           #:model [model "llama3.2"]
                           #:system-prompt [system-prompt "You are a helpful assistant."]
                           #:base-url [base-url "http://localhost:11434"])
  "Ollama chat with tool support (limited)

   Returns: llm-response structure"

  (define messages
    (list (make-system-message system-prompt)
          (make-user-message prompt)))

  (ollama-chat-completion messages
                          #:model model
                          #:tools tools
                          #:base-url base-url))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (ollama-message->hash msg)
  "Convert llm-message to Ollama-compatible hash"
  (message->ollama-format msg))

(define (ollama-parse-response response-data)
  "Parse Ollama API response"
  (parse-ollama-response response-data))
