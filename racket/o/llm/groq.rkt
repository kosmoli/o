#lang racket

;;; llm/groq.rkt - Groq API Client
;;;
;;; This module provides a client for Groq's API,
;;; which offers fast inference with open-source models.

(provide groq-chat-completion
         groq-simple
         groq-with-tools
         groq-message->hash
         groq-tools->hash
         groq-parse-response)

(require net/http-easy
         json
         racket/hash
         racket/format
         "./types.rkt")

;;; ============================================================================
;;; Groq Chat Completion
;;; ============================================================================

(define (groq-chat-completion messages
                              #:model [model "llama-3.3-70b-versatile"]
                              #:api-key [api-key #f]
                              #:temperature [temperature 0.7]
                              #:max-tokens [max-tokens 4096]
                              #:tools [tools #f]
                              #:tool-choice [tool-choice #f])
  "Call Groq chat completions API

   Parameters:
   - messages: list of llm-message structures or hashes
   - model: model name (default: llama-3.3-70b-versatile)
   - api-key: Groq API key (defaults to GROQ_API_KEY env var)
   - temperature: sampling temperature (0-2)
   - max-tokens: maximum tokens to generate
   - tools: list of tool-definition structures
   - tool-choice: 'auto, 'none, or specific tool

   Returns: llm-response structure"

  (define key (or api-key (getenv "GROQ_API_KEY")))
  (unless key
    (error 'groq-chat-completion "GROQ_API_KEY not set"))

  (define headers
    (hash 'content-type "application/json"
          'authorization (format "Bearer ~a" key)))

  ;; Convert messages to API format
  (define api-messages
    (map (lambda (msg)
           (if (llm-message? msg)
               (message->hash msg)
               msg))
         messages))

  ;; Build request body
  (define body-data
    (hash 'model model
          'messages api-messages
          'temperature temperature
          'max_tokens max-tokens))

  ;; Add optional parameters
  (define body-data-with-tools
    (if tools
        (hash-set body-data 'tools
                  (map tool-definition->hash tools))
        body-data))

  (define body-data-final
    (if tool-choice
        (hash-set body-data-with-tools 'tool_choice tool-choice)
        body-data-with-tools))

  (define endpoint "https://api.groq.com/openai/v1/chat/completions")

  ;; Make API request
  (define response
    (post endpoint
         #:headers headers
         #:data (jsexpr->string body-data-final)))

  (if (= (response-status-code response) 200)
      (parse-groq-response (response-json response))
      (llm-error 'api-error
                "Groq API request failed"
                (response-status-code response)
                'groq
                (response-body response))))

;;; ============================================================================
;;; Response Parsing
;;; ============================================================================

(define (parse-groq-response response-json)
  "Parse Groq API response into llm-response structure"
  (define id (hash-ref response-json 'id ""))
  (define model (hash-ref response-json 'model ""))
  (define created (hash-ref response-json 'created 0))
  (define choices (hash-ref response-json 'choices '()))
  (define usage (hash-ref response-json 'usage (hash)))

  ;; Extract first choice
  (define first-choice (and (pair? choices) (first choices)))
  (define message-hash (hash-ref first-choice 'message (hash)))
  (define finish-reason-str (hash-ref first-choice 'finish_reason "stop"))

  ;; Build llm-response
  (llm-response
   id
   model
   (hash->message message-hash)
   (finish-reason->symbol finish-reason-str)
   (usage-stats (hash-ref usage 'prompt_tokens 0)
                (hash-ref usage 'completion_tokens 0)
                (hash-ref usage 'total_tokens 0))
   created
   'groq))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(define (groq-simple prompt
                     #:model [model "llama-3.3-70b-versatile"]
                     #:system-prompt [system-prompt "You are a helpful assistant."]
                     #:api-key [api-key #f])
  "Simple Groq chat completion with just a prompt

   Returns: string content of response"

  (define messages
    (list (make-system-message system-prompt)
          (make-user-message prompt)))

  (define response
    (groq-chat-completion messages
                          #:model model
                          #:api-key api-key))

  (if (llm-response? response)
      (extract-text-content (llm-response-message response))
      (error 'groq-simple "Request failed" response)))

(define (groq-with-tools prompt tools
                         #:model [model "llama-3.3-70b-versatile"]
                         #:system-prompt [system-prompt "You are a helpful assistant."]
                         #:api-key [api-key #f])
  "Groq chat completion with tool support

   Returns: llm-response structure (check for tool calls)"

  (define messages
    (list (make-system-message system-prompt)
          (make-user-message prompt)))

  (groq-chat-completion messages
                        #:model model
                        #:tools tools
                        #:api-key api-key))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (groq-message->hash msg)
  "Convert llm-message to Groq-compatible hash"
  (message->hash msg))

(define (groq-tools->hash tools)
  "Convert list of tool-definitions to Groq format"
  (map tool-definition->hash tools))

(define (groq-parse-response response-json)
  "Parse Groq API response"
  (parse-groq-response response-json))
