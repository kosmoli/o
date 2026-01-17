;;; llm/openai.ss - OpenAI API Client
;;;
;;; This module provides a client for OpenAI's chat completions API,
;;; supporting both basic chat and tool calling.

(export #t)

(import
  :std/net/request
  :std/text/json
  :std/sugar
  :std/misc/hash
  :o/llm/types)

;;; ============================================================================
;;; OpenAI Chat Completion
;;; ============================================================================

(def (openai-chat-completion
      messages
      #!key
      (model "gpt-4")
      (api-key #f)
      (temperature 0.7)
      (max-tokens #f)
      (tools #f)
      (tool-choice #f))
  "Call OpenAI chat completions API

   Parameters:
   - messages: list of llm-message structures or hashes
   - model: model name (default: gpt-4)
   - api-key: OpenAI API key (defaults to OPENAI_API_KEY env var)
   - temperature: sampling temperature (0-2)
   - max-tokens: maximum tokens to generate
   - tools: list of tool-definition structures
   - tool-choice: 'auto', 'none', or specific tool

   Returns: llm-response structure"

  (let ((key (or api-key (get-environment-variable "OPENAI_API_KEY"))))
    (unless key
      (error "OpenAI API key not provided and OPENAI_API_KEY not set"))

    (let* ((headers `(("Content-Type" . "application/json")
                      ("Authorization" . ,(string-append "Bearer " key))))

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
                       'temperature temperature))

           ;; Add optional parameters
           (_ (begin
                (when max-tokens
                  (hash-put! body-data 'max_tokens max-tokens))
                (when tools
                  (hash-put! body-data 'tools
                             (map tool-definition->hash tools)))
                (when tool-choice
                  (hash-put! body-data 'tool_choice tool-choice))))

           (body-string (json-object->string body-data))
           (endpoint "https://api.openai.com/v1/chat/completions"))

      ;; Make API request
      (let ((response (http-post endpoint headers: headers data: body-string)))
        (if (= (request-status response) 200)
            (parse-openai-response response)
            (make-llm-error
             type: :api-error
             message: "OpenAI API request failed"
             status: (request-status response)
             provider: :openai
             details: (request-text response)))))))

;;; ============================================================================
;;; Response Parsing
;;; ============================================================================

(def (parse-openai-response response)
  "Parse OpenAI API response into llm-response structure"
  (let* ((response-json (request-json response))
         (id (hash-ref response-json 'id))
         (model (hash-ref response-json 'model))
         (created (hash-ref response-json 'created))
         (choices (hash-ref response-json 'choices))
         (usage (hash-ref response-json 'usage))

         ;; Extract first choice
         (first-choice (and (pair? choices) (car choices)))
         (message-hash (hash-ref first-choice 'message))
         (finish-reason-str (hash-ref first-choice 'finish_reason)))

    ;; Build llm-response
    (make-llm-response
     id: id
     model: model
     message: (hash->message message-hash)
     finish-reason: (finish-reason->symbol finish-reason-str)
     usage: (make-usage-stats
             prompt-tokens: (hash-ref usage 'prompt_tokens)
             completion-tokens: (hash-ref usage 'completion_tokens)
             total-tokens: (hash-ref usage 'total_tokens))
     created: created
     provider: :openai)))

;;; ============================================================================
;;; Streaming Support (Future)
;;; ============================================================================

;; TODO: Implement streaming support
;; (def (openai-chat-completion-stream messages #!key ...)
;;   ...)

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(def (openai-simple prompt
                     #!key
                     (model "gpt-4")
                     (system-prompt "You are a helpful assistant.")
                     (api-key #f))
  "Simple OpenAI chat completion with just a prompt

   Returns: string content of response"

  (let* ((messages (list
                    (make-system-message system-prompt)
                    (make-user-message prompt)))
         (response (openai-chat-completion
                    messages
                    model: model
                    api-key: api-key)))

    (if (llm-response? response)
        (extract-text-content (llm-response-message response))
        (error "OpenAI request failed" response))))

(def (openai-with-tools prompt tools
                        #!key
                        (model "gpt-4")
                        (system-prompt "You are a helpful assistant.")
                        (api-key #f))
  "OpenAI chat completion with tool support

   Returns: llm-response structure (check for tool calls)"

  (let ((messages (list
                   (make-system-message system-prompt)
                   (make-user-message prompt))))

    (openai-chat-completion
     messages
     model: model
     tools: tools
     api-key: api-key)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Simple usage
(def response (openai-simple "Why is the sky blue? Be concise."))
(displayln response)

;; With tools
(def calculator-tool
  (make-tool-definition-instance
   "calculate"
   "Perform a mathematical calculation"
   (hash
    'type "object"
    'properties (hash
                 'expression (hash
                              'type "string"
                              'description "Mathematical expression to evaluate"))
    'required '("expression"))))

(def response (openai-with-tools
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
