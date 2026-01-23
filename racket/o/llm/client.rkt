#lang racket

;;; llm/client.rkt - LLM Client following Racket AI Book best practices
;;;
;;; Provides a unified interface for multiple LLM providers using net/http-easy

(provide (struct-out llm-config)
         (struct-out llm-response)
         make-llm-config
         llm-chat-completion
         llm-embeddings
         llm-chat->messages
         llm-response->content
         llm-response->usage)

(require net/http-easy
         json
         racket/hash
         racket/format
         racket/string
         racket/port)

;;; ============================================================================
;;; Configuration Types
;;; ============================================================================

(struct llm-config
  (provider           ; 'openai, 'anthropic, 'ollama, 'groq
   model              ; Model name
   api-key            ; API key
   base-url           ; Base URL
   temperature        ; Temperature
   max-tokens         ; Max tokens
   timeout)           ; Timeout
  #:transparent)

(define (make-llm-config
         #:provider [provider 'openai]
         #:model [model "gpt-4o-mini"]
         #:api-key [api-key (getenv "OPENAI_API_KEY")]
         #:base-url [base-url #f]
         #:temperature [temperature 0.7]
         #:max-tokens [max-tokens 4096]
         #:timeout [timeout 120])
  "Create LLM configuration"
  (llm-config provider model api-key base-url temperature max-tokens timeout))

;;; ============================================================================
;;; Response Types
;;; ============================================================================

(struct llm-response
  (content            ; Response content
   tool-calls         ; Tool calls
   usage-prompt       ; Prompt tokens
   usage-completion   ; Completion tokens
   usage-total        ; Total tokens
   model              ; Model used
   raw)               ; Raw response
  #:transparent)

;;; ============================================================================
;;; Provider Endpoints
;;; ============================================================================

(define provider-endpoints
  (hash 'openai (hash 'chat "https://api.openai.com/v1/chat/completions"
                       'embeddings "https://api.openai.com/v1/embeddings")
        'anthropic (hash 'chat "https://api.anthropic.com/v1/messages"
                         'embeddings #f)
        'ollama (hash 'chat "http://localhost:11434/api/chat"
                      'embeddings "http://localhost:11434/api/embeddings")
        'groq (hash 'chat "https://api.groq.com/openai/v1/chat/completions"
                    'embeddings #f)))

(define (get-endpoint config endpoint-type)
  "Get endpoint URL for config"
  (define provider (llm-config-provider config))
  (define base-url (llm-config-base-url config))
  (if base-url
      (string-append base-url
                     (if (string=? endpoint-type "chat")
                         "/chat/completions"
                         "/embeddings"))
      (hash-ref (hash-ref provider-endpoints provider)
                (string->symbol endpoint-type)
                #f)))

;;; ============================================================================
;;; Authentication Headers
;;; ============================================================================

(define (get-auth-headers config)
  "Get authentication headers"
  (case (llm-config-provider config)
    [(openai groq)
     (hash 'authorization (format "Bearer ~a" (llm-config-api-key config))
           'content-type "application/json")]
    [(anthropic)
     (hash 'x-api-key (llm-config-api-key config)
           'anthropic-version "2023-06-01"
           'content-type "application/json")]
    [(ollama)
     (hash 'content-type "application/json")]
    [else
     (error 'get-auth-headers "Unknown provider")]))

;;; ============================================================================
;;; Message Conversion
;;; ============================================================================

(define (llm-chat->messages messages)
  "Convert various message formats to standard format"
  (map (lambda (msg)
         (cond
           [(hash? msg)
            (hash 'role (hash-ref msg 'role "user")
                  'content (hash-ref msg 'content ""))]
           [(and (pair? msg) (string? (car msg)))
            (hash 'role (car msg) 'content (cadr msg))]
           [else
            (hash 'role "user" 'content (format "~a" msg))]))
       messages))

;;; ============================================================================
;;; Chat Completion
;;; ============================================================================

(define (llm-chat-completion config messages
                             #:tools [tools #f]
                             #:system [system #f])
  "Send chat completion request to LLM"

  (define normalized-messages (llm-chat->messages messages))

  (define messages-with-system
    (if system
        (cons (hash 'role "system" 'content system) normalized-messages)
        normalized-messages))

  (define payload
    (hash 'model (llm-config-model config)
          'messages messages-with-system
          'temperature (llm-config-temperature config)
          'max_tokens (llm-config-max-tokens config)
          'stream #f))

  (define payload-with-tools
    (if tools
        (hash-set payload 'tools tools)
        payload))

  (define auth
    (lambda (uri headers params)
      (values (hash-union headers (get-auth-headers config))
              params)))

  (define endpoint (get-endpoint config "chat"))

  (unless endpoint
    (error 'llm-chat-completion "Provider not supported"))

  (define response
    (post endpoint
         #:auth auth
         #:data (jsexpr->string payload-with-tools)
         #:timeout (llm-config-timeout config)))

  (unless (= (response-status-code response) 200)
    (error 'llm-chat-completion
           (format "Request failed: ~a" (response-status-code response))))

  (parse-chat-response config (response-json response)))

;;; ============================================================================
;;; Response Parsing
;;; ============================================================================

(define (parse-chat-response config response-data)
  "Parse chat response based on provider"

  (case (llm-config-provider config)
    [(openai groq)
     (define choice (first (hash-ref response-data 'choices)))
     (define message (hash-ref choice 'message))
     (llm-response
      (hash-ref message 'content "")
      (hash-ref message 'tool_calls #f)
      (hash-ref (hash-ref response-data 'usage) 'prompt_tokens 0)
      (hash-ref (hash-ref response-data 'usage) 'completion_tokens 0)
      (hash-ref (hash-ref response-data 'usage) 'total_tokens 0)
      (hash-ref response-data 'model "")
      response-data)]

    [(anthropic)
     (define content-blocks (hash-ref response-data 'content))
     (define text-content
       (apply string-append
              (map (lambda (block)
                     (if (string=? (hash-ref block 'type) "text")
                         (hash-ref block 'text "")
                         ""))
                   (filter (lambda (b) (string=? (hash-ref b 'type) "text"))
                           content-blocks))))
     (define tool-calls
       (filter-map
        (lambda (block)
          (when (string=? (hash-ref block 'type) "tool_use")
            (hash 'id (hash-ref block 'id)
                  'name (hash-ref block 'name)
                  'input (hash-ref block 'input))))
        (filter (lambda (b) (string=? (hash-ref b 'type) "tool_use"))
                content-blocks)))
     (llm-response
      text-content
      (and (not (null? tool-calls)) tool-calls)
      (hash-ref (hash-ref response-data 'usage) 'input_tokens 0)
      (hash-ref (hash-ref response-data 'usage) 'output_tokens 0)
      (+ (hash-ref (hash-ref response-data 'usage) 'input_tokens 0)
         (hash-ref (hash-ref response-data 'usage) 'output_tokens 0))
      (hash-ref response-data 'model "")
      response-data)]

    [(ollama)
     (define message (hash-ref response-data 'message))
     (llm-response
      (hash-ref message 'content "")
      #f
      (hash-ref response-data 'prompt_eval_count 0)
      (hash-ref response-data 'eval_count 0)
      (+ (hash-ref response-data 'prompt_eval_count 0)
         (hash-ref response-data 'eval_count 0))
      (hash-ref response-data 'model "")
      response-data)]

    [else
     (error 'parse-chat-response "Unknown provider")]))

;;; ============================================================================
;;; Embeddings
;;; ============================================================================

(define (llm-embeddings config text)
  "Get embeddings for text"

  (define endpoint (get-endpoint config "embeddings"))

  (unless endpoint
    (error 'llm-embeddings "Provider does not support embeddings"))

  (define payload
    (hash 'input text
          'model (llm-config-model config)))

  (define auth
    (lambda (uri headers params)
      (values (hash-union headers (get-auth-headers config))
              params)))

  (define response
    (post endpoint
         #:auth auth
         #:data (jsexpr->string payload)
         #:timeout (llm-config-timeout config)))

  (unless (= (response-status-code response) 200)
    (error 'llm-embeddings "Embeddings request failed"))

  (define response-data (response-json response))
  (define data (first (hash-ref response-data 'data)))
  (hash-ref data 'embedding))

;;; ============================================================================
;;; Response Helpers
;;; ============================================================================

(define (llm-response->content response)
  "Extract content from llm-response"
  (llm-response-content response))

(define (llm-response->usage response)
  "Get usage info as hash"
  (hash 'prompt_tokens (llm-response-usage-prompt response)
        'completion_tokens (llm-response-usage-completion response)
        'total_tokens (llm-response-usage-total response)))
