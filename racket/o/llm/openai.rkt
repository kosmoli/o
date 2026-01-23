#lang racket

;;; llm/openai.rkt - OpenAI API Client
;;;
;;; This module provides a client for OpenAI's chat completions API,
;;; supporting both basic chat and tool calling.

(provide openai-chat-completion
         openai-simple
         openai-with-tools
         openai-stream
         openai-message->hash
         openai-tools->hash
         openai-parse-response)

(require net/http-easy
         json
         racket/hash
         racket/format
         racket/match
         "./types.rkt")

;;; ============================================================================
;;; OpenAI Chat Completion
;;; ============================================================================

(define (openai-chat-completion messages
                                #:model [model "gpt-4"]
                                #:api-key [api-key #f]
                                #:temperature [temperature 0.7]
                                #:max-tokens [max-tokens #f]
                                #:tools [tools #f]
                                #:tool-choice [tool-choice #f])
  "Call OpenAI chat completions API

   Parameters:
   - messages: list of llm-message structures or hashes
   - model: model name (default: gpt-4)
   - api-key: OpenAI API key (defaults to OPENAI_API_KEY env var)
   - temperature: sampling temperature (0-2)
   - max-tokens: maximum tokens to generate
   - tools: list of tool-definition structures
   - tool-choice: 'auto, 'none, or specific tool

   Returns: llm-response structure"

  (define key (or api-key (getenv "OPENAI_API_KEY")))
  (unless key
    (error 'openai-chat-completion "OPENAI_API_KEY not set"))

  (define headers
    (hash 'content-type "application/json"
          'authorization (format "Bearer ~a" key)))

  ;; Convert messages to API format
  (define api-messages
    (map (lambda (msg)
           (if (llm-message? msg)
               (message->hash msg)
               msg))  ; Already a hash
         messages))

  ;; Build request body
  (define body-data
    (hash 'model model
          'messages api-messages
          'temperature temperature))

  ;; Add optional parameters
  (define body-data-with-max
    (if max-tokens
        (hash-set body-data 'max_tokens max-tokens)
        body-data))

  (define body-data-with-tools
    (if tools
        (hash-set body-data-with-max 'tools
                  (map tool-definition->hash tools))
        body-data-with-max))

  (define body-data-final
    (if tool-choice
        (hash-set body-data-with-tools 'tool_choice tool-choice)
        body-data-with-tools))

  (define endpoint "https://api.openai.com/v1/chat/completions")

  ;; Make API request
  (define response
    (post endpoint
         #:headers headers
         #:data (jsexpr->string body-data-final)))

  (if (= (response-status-code response) 200)
      (parse-openai-response (response-json response))
      (llm-error 'api-error
                "OpenAI API request failed"
                (response-status-code response)
                'openai
                (response-body response))))

;;; ============================================================================
;;; Response Parsing
;;; ============================================================================

(define (parse-openai-response response-json)
  "Parse OpenAI API response into llm-response structure"

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
   'openai))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(define (openai-simple prompt
                       #:model [model "gpt-4"]
                       #:system-prompt [system-prompt "You are a helpful assistant."]
                       #:api-key [api-key #f])
  "Simple OpenAI chat completion with just a prompt

   Returns: string content of response"

  (define messages
    (list (make-system-message system-prompt)
          (make-user-message prompt)))

  (define response
    (openai-chat-completion messages
                            #:model model
                            #:api-key api-key))

  (if (llm-response? response)
      (extract-text-content (llm-response-message response))
      (error 'openai-simple "Request failed" response)))

(define (openai-with-tools prompt tools
                           #:model [model "gpt-4"]
                           #:system-prompt [system-prompt "You are a helpful assistant."]
                           #:api-key [api-key #f])
  "OpenAI chat completion with tool support

   Returns: llm-response structure (check for tool calls)"

  (define messages
    (list (make-system-message system-prompt)
          (make-user-message prompt)))

  (openai-chat-completion messages
                          #:model model
                          #:tools tools
                          #:api-key api-key))

;;; ============================================================================
;;; Streaming Support (Placeholder)
;;; ============================================================================

;; TODO: Implement streaming support
(define (openai-stream messages
                       #:model [model "gpt-4"]
                       #:api-key [api-key #f]
                       #:temperature [temperature 0.7]
                       #:max-tokens [max-tokens #f]
                       #:tools [tools #f]
                       #:tool-choice [tool-choice #f]
                       #:on-chunk [on-chunk void])
  "Stream OpenAI response

   Parameters:
   - on-chunk: callback function for each chunk (chunk -> void)

   Returns: llm-response structure"
  (error 'openai-stream "Streaming not yet implemented"))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (openai-message->hash msg)
  "Convert llm-message to OpenAI-compatible hash"
  (message->hash msg))

(define (openai-tools->hash tools)
  "Convert list of tool-definitions to OpenAI format"
  (map tool-definition->hash tools))

(define (openai-parse-response response-json)
  "Parse OpenAI API response"
  (parse-openai-response response-json))
