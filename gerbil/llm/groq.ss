;;; llm/groq.ss - Groq API Client
;;;
;;; This module provides a client for Groq's chat completions API.
;;; Groq uses an OpenAI-compatible API, so we reuse most of the OpenAI logic.

(export #t)

(import
  :std/net/request
  :std/text/json
  :std/sugar
  :std/misc/hash
  :o/llm/types)

;;; ============================================================================
;;; Groq Chat Completion
;;; ============================================================================

(def (groq-chat-completion
      messages
      #!key
      (model "llama-3.3-70b-versatile")
      (api-key #f)
      (temperature 0.7)
      (max-tokens #f)
      (tools #f)
      (tool-choice #f))
  "Call Groq chat completions API (OpenAI-compatible)

   Parameters:
   - messages: list of llm-message structures or hashes
   - model: model name (default: llama-3.3-70b-versatile)
   - api-key: Groq API key (defaults to GROQ_API_KEY env var)
   - temperature: sampling temperature (0-2)
   - max-tokens: maximum tokens to generate
   - tools: list of tool-definition structures
   - tool-choice: 'auto', 'none', or specific tool

   Returns: llm-response structure"

  (let ((key (or api-key (get-environment-variable "GROQ_API_KEY"))))
    (unless key
      (error "Groq API key not provided and GROQ_API_KEY not set"))

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
           (endpoint "https://api.groq.com/openai/v1/chat/completions"))

      ;; Make API request
      (let ((response (http-post endpoint headers: headers data: body-string)))
        (if (= (request-status response) 200)
            (parse-groq-response response)
            (make-llm-error
             type: :api-error
             message: "Groq API request failed"
             status: (request-status response)
             provider: :groq
             details: (request-text response)))))))

;;; ============================================================================
;;; Response Parsing
;;; ============================================================================

(def (parse-groq-response response)
  "Parse Groq API response into llm-response structure"
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
     provider: :groq)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(def (groq-simple prompt
                  #!key
                  (model "llama-3.3-70b-versatile")
                  (system-prompt "You are a helpful assistant.")
                  (api-key #f))
  "Simple Groq chat completion with just a prompt

   Returns: string content of response"

  (let* ((messages (list
                    (make-system-message system-prompt)
                    (make-user-message prompt)))
         (response (groq-chat-completion
                    messages
                    model: model
                    api-key: api-key)))

    (if (llm-response? response)
        (extract-text-content (llm-response-message response))
        (error "Groq request failed" response))))

(def (groq-with-tools prompt tools
                      #!key
                      (model "llama-3.3-70b-versatile")
                      (system-prompt "You are a helpful assistant.")
                      (api-key #f))
  "Groq chat completion with tool support

   Returns: llm-response structure (check for tool calls)"

  (let ((messages (list
                   (make-system-message system-prompt)
                   (make-user-message prompt))))

    (groq-chat-completion
     messages
     model: model
     tools: tools
     api-key: api-key)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Simple usage
(def response (groq-simple "Why is the sky blue? Be concise."))
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

(def response (groq-with-tools
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
