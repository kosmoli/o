#lang racket

;;; llm/anthropic.rkt - Anthropic Claude API Client
;;;
;;; This module provides a client for Anthropic's Messages API.

(provide anthropic-messages
         anthropic-simple
         anthropic-with-tools
         anthropic-stream
         anthropic-message->hash
         anthropic-tools->hash
         anthropic-parse-content
         anthropic-stop-reason->symbol)

(require net/http-easy
         json
         racket/hash
         racket/format
         racket/match
         "./types.rkt")

;;; ============================================================================
;;; Anthropic Messages API
;;; ============================================================================

(define (anthropic-messages messages
                            #:model [model "claude-3-5-sonnet-20241022"]
                            #:api-key [api-key #f]
                            #:max-tokens [max-tokens 4096]
                            #:temperature [temperature 1.0]
                            #:system [system #f]
                            #:tools [tools #f])
  "Call Anthropic Messages API"

  (define key (or api-key (getenv "ANTHROPIC_API_KEY")))
  (unless key
    (error 'anthropic-messages "ANTHROPIC_API_KEY not set"))

  (define headers
    (hash 'content-type "application/json"
          'x-api-key key
          'anthropic-version "2023-06-01"))

  ;; Filter out system messages and convert to API format
  (define api-messages
    (filter-map
     (lambda (msg)
       (define m (if (llm-message? msg) msg (hash->message msg)))
       (if (eq? (llm-message-role m) 'system)
           #f
           (message->anthropic-format m)))
     messages))

  ;; Build request body
  (define body-data
    (hash 'model model
          'messages api-messages
          'max_tokens max-tokens
          'temperature temperature))

  (define body-data-with-system
    (if system
        (hash-set body-data 'system system)
        body-data))

  (define body-data-with-tools
    (if tools
        (hash-set body-data-with-system 'tools
                  (map tool-definition->anthropic-format tools))
        body-data-with-system))

  (define endpoint "https://api.anthropic.com/v1/messages")

  (define response
    (post endpoint
         #:headers headers
         #:data (jsexpr->string body-data-with-tools)))

  (if (= (response-status-code response) 200)
      (parse-anthropic-response (response-json response))
      (llm-error 'api-error
                "Anthropic API request failed"
                (response-status-code response)
                'anthropic
                (response-body response))))

;;; ============================================================================
;;; Format Conversion
;;; ============================================================================

(define (message->anthropic-format msg)
  "Convert llm-message to Anthropic API format"
  (define role (llm-message-role msg))
  (define content (llm-message-content msg))
  (define tool-calls (llm-message-tool-calls msg))

  (cond
   [(eq? role 'user)
    (hash 'role "user"
          'content content)]

   [(and (eq? role 'assistant) tool-calls)
    (hash 'role "assistant"
          'content (map tool-call->anthropic-format tool-calls))]

   [(eq? role 'assistant)
    (hash 'role "assistant"
          'content content)]

   [(eq? role 'tool)
    (hash 'role "user"
          'content (list
                   (hash 'type "tool_result"
                        'tool_use_id (llm-message-tool-call-id msg)
                        'content content)))]

   [else
    (error 'message->anthropic-format "Unsupported message role" role)]))

(define (tool-call->anthropic-format tc)
  "Convert tool-call to Anthropic tool_use format"
  (hash 'type "tool_use"
        'id (tool-call-id tc)
        'name (function-call-name (tool-call-function tc))
        'input (string->jsexpr (function-call-arguments (tool-call-function tc)))))

(define (tool-definition->anthropic-format td)
  "Convert tool-definition to Anthropic format"
  (define func (tool-definition-function td))
  (hash 'name (function-definition-name func)
        'description (function-definition-description func)
        'input_schema (function-definition-parameters func)))

;;; ============================================================================
;;; Response Parsing
;;; ============================================================================

(define (parse-anthropic-response response-json)
  "Parse Anthropic API response into llm-response structure"
  (define id (hash-ref response-json 'id ""))
  (define model (hash-ref response-json 'model ""))
  (define role (hash-ref response-json 'role "assistant"))
  (define content-blocks (hash-ref response-json 'content '()))
  (define stop-reason (hash-ref response-json 'stop_reason "end_turn"))
  (define usage (hash-ref response-json 'usage (hash)))

  (define parsed-content (parse-anthropic-content content-blocks))

  (llm-response
   id
   model
   (llm-message (string->symbol role)
                (car parsed-content)
                #f
                (cdr parsed-content)
                #f)
   (anthropic-stop-reason->symbol stop-reason)
   (usage-stats (hash-ref usage 'input_tokens 0)
                (hash-ref usage 'output_tokens 0)
                (+ (hash-ref usage 'input_tokens 0)
                   (hash-ref usage 'output_tokens 0)))
   (current-seconds)
   'anthropic))

(define (parse-anthropic-content content-blocks)
  "Parse Anthropic content blocks into (text . tool-calls)"

  (for/fold ([text-parts '()]
             [tool-calls '()]
             #:result (cons (string-join (reverse text-parts) "\n")
                            (reverse tool-calls)))
            ([block (in-list content-blocks)])
    (define block-type (hash-ref block 'type ""))
    (cond
     [(string=? block-type "text")
      (values (cons (hash-ref block 'text "") text-parts)
              tool-calls)]

     [(string=? block-type "tool_use")
      (define tc (make-tool-call-instance
                  (hash-ref block 'id "")
                  (hash-ref block 'name "")
                  (jsexpr->string (hash-ref block 'input (hash)))))
      (values text-parts (cons tc tool-calls))]

     [else
      (values text-parts tool-calls)])))

(define (anthropic-stop-reason->symbol reason-str)
  "Convert Anthropic stop reason to standard symbol"
  (match reason-str
    ["end_turn" 'stop]
    ["tool_use" 'tool-calls]
    ["max_tokens" 'length]
    ["stop_sequence" 'stop]
    [_ 'unknown]))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(define (anthropic-simple prompt
                          #:model [model "claude-3-5-sonnet-20241022"]
                          #:system-prompt [system-prompt "You are a helpful assistant."]
                          #:max-tokens [max-tokens 4096]
                          #:api-key [api-key #f])
  "Simple Anthropic chat with just a prompt"

  (define messages (list (make-user-message prompt)))
  (define response
    (anthropic-messages messages
                        #:model model
                        #:system system-prompt
                        #:max-tokens max-tokens
                        #:api-key api-key))

  (if (llm-response? response)
      (extract-text-content (llm-response-message response))
      (error 'anthropic-simple "Request failed" response)))

(define (anthropic-with-tools prompt tools
                              #:model [model "claude-3-5-sonnet-20241022"]
                              #:system-prompt [system-prompt "You are a helpful assistant."]
                              #:max-tokens [max-tokens 4096]
                              #:api-key [api-key #f])
  "Anthropic chat with tool support"

  (define messages (list (make-user-message prompt)))
  (anthropic-messages messages
                      #:model model
                      #:system system-prompt
                      #:tools tools
                      #:max-tokens max-tokens
                      #:api-key api-key))

;;; ============================================================================
;;; Streaming Support (Placeholder)
;;; ============================================================================

(define (anthropic-stream messages
                          #:model [model "claude-3-5-sonnet-20241022"]
                          #:api-key [api-key #f]
                          #:max-tokens [max-tokens 4096]
                          #:temperature [temperature 1.0]
                          #:system [system #f]
                          #:tools [tools #f]
                          #:on-chunk [on-chunk void])
  "Stream Anthropic response"
  (error 'anthropic-stream "Streaming not yet implemented"))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (anthropic-message->hash msg)
  "Convert llm-message to Anthropic-compatible hash"
  (message->anthropic-format msg))

(define (anthropic-tools->hash tools)
  "Convert list of tool-definitions to Anthropic format"
  (map tool-definition->anthropic-format tools))

(define (anthropic-parse-content content-blocks)
  "Parse Anthropic content blocks"
  (parse-anthropic-content content-blocks))
