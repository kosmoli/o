;;; llm/anthropic.ss - Anthropic API Client
;;;
;;; This module provides a client for Anthropic's Messages API (Claude),
;;; supporting both basic chat and tool use.

(export #t)

(import
  :std/net/request
  :std/text/json
  :std/sugar
  :std/misc/hash
  ./types)

;;; ============================================================================
;;; Anthropic Messages API
;;; ============================================================================

(def (anthropic-messages
      messages
      #!key
      (model "claude-3-5-sonnet-20241022")
      (api-key #f)
      (max-tokens 4096)
      (temperature 1.0)
      (system #f)
      (tools #f))
  "Call Anthropic Messages API

   Parameters:
   - messages: list of llm-message structures or hashes (no system messages)
   - model: model name (default: claude-3-5-sonnet-20241022)
   - api-key: Anthropic API key (defaults to ANTHROPIC_API_KEY env var)
   - max-tokens: maximum tokens to generate (required by Anthropic)
   - temperature: sampling temperature (0-1)
   - system: system prompt string (separate from messages)
   - tools: list of tool-definition structures

   Returns: llm-response structure"

  (let ((key (or api-key (get-environment-variable "ANTHROPIC_API_KEY"))))
    (unless key
      (error "Anthropic API key not provided and ANTHROPIC_API_KEY not set"))

    (let* ((headers `(("Content-Type" . "application/json")
                      ("x-api-key" . ,key)
                      ("anthropic-version" . "2023-06-01")))

           ;; Filter out system messages and convert to API format
           (api-messages
            (filter-map
             (lambda (msg)
               (let ((m (if (llm-message? msg) msg (hash->message msg))))
                 (if (eq? (llm-message-role m) :system)
                     #f  ;; Skip system messages
                     (message->anthropic-format m))))
             messages))

           ;; Build request body
           (body-data (hash
                       'model model
                       'messages api-messages
                       'max_tokens max-tokens
                       'temperature temperature))

           ;; Add optional parameters
           (_ (begin
                (when system
                  (hash-put! body-data 'system system))
                (when tools
                  (hash-put! body-data 'tools
                             (map tool-definition->anthropic-format tools)))))

           (body-string (json-object->string body-data))
           (endpoint "https://api.anthropic.com/v1/messages"))

      ;; Make API request
      (let ((response (http-post endpoint headers: headers data: body-string)))
        (if (= (request-status response) 200)
            (parse-anthropic-response response)
            (make-llm-error
             type: :api-error
             message: "Anthropic API request failed"
             status: (request-status response)
             provider: :anthropic
             details: (request-text response)))))))

;;; ============================================================================
;;; Format Conversion
;;; ============================================================================

(def (message->anthropic-format msg)
  "Convert llm-message to Anthropic API format"
  (let ((role (llm-message-role msg))
        (content (llm-message-content msg))
        (tool-calls (llm-message-tool-calls msg)))

    (cond
     ;; User message
     ((eq? role :user)
      (hash 'role "user"
            'content content))

     ;; Assistant message with tool calls
     ((and (eq? role :assistant) tool-calls)
      (hash 'role "assistant"
            'content (map tool-call->anthropic-format tool-calls)))

     ;; Assistant message without tool calls
     ((eq? role :assistant)
      (hash 'role "assistant"
            'content content))

     ;; Tool result message
     ((eq? role :tool)
      (hash 'role "user"
            'content (list
                      (hash
                       'type "tool_result"
                       'tool_use_id (llm-message-tool-call-id msg)
                       'content content))))

     (else
      (error "Unsupported message role for Anthropic" role)))))

(def (tool-call->anthropic-format tc)
  "Convert tool-call to Anthropic tool_use format"
  (hash
   'type "tool_use"
   'id (tool-call-id tc)
   'name (function-call-name (tool-call-function tc))
   'input (string->json-object (function-call-arguments (tool-call-function tc)))))

(def (tool-definition->anthropic-format td)
  "Convert tool-definition to Anthropic format"
  (let ((func (tool-definition-function td)))
    (hash
     'name (function-definition-name func)
     'description (function-definition-description func)
     'input_schema (function-definition-parameters func))))

;;; ============================================================================
;;; Response Parsing
;;; ============================================================================

(def (parse-anthropic-response response)
  "Parse Anthropic API response into llm-response structure"
  (let* ((response-json (request-json response))
         (id (hash-ref response-json 'id))
         (model (hash-ref response-json 'model))
         (role (hash-ref response-json 'role))
         (content (hash-ref response-json 'content))
         (stop-reason (hash-ref response-json 'stop_reason))
         (usage (hash-ref response-json 'usage))

         ;; Parse content blocks
         (parsed-content (parse-anthropic-content content)))

    ;; Build llm-response
    (make-llm-response
     id: id
     model: model
     message: (make-llm-message
               role: (string->symbol role)
               content: (car parsed-content)  ;; text content
               name: #f
               tool-calls: (cdr parsed-content)  ;; tool calls
               tool-call-id: #f)
     finish-reason: (anthropic-stop-reason->symbol stop-reason)
     usage: (make-usage-stats
             prompt-tokens: (hash-ref usage 'input_tokens)
             completion-tokens: (hash-ref usage 'output_tokens)
             total-tokens: (+ (hash-ref usage 'input_tokens)
                             (hash-ref usage 'output_tokens)))
     created: (current-seconds)  ;; Anthropic doesn't provide timestamp
     provider: :anthropic)))

(def (parse-anthropic-content content-blocks)
  "Parse Anthropic content blocks into (text . tool-calls)

   Returns: cons of (text-content . list-of-tool-calls)"

  (let loop ((blocks content-blocks)
             (text-parts '())
             (tool-calls '()))

    (if (null? blocks)
        ;; Return (text . tool-calls)
        (cons (string-join (reverse text-parts) "\n")
              (reverse tool-calls))

        (let* ((block (car blocks))
               (block-type (hash-ref block 'type)))

          (cond
           ;; Text block
           ((equal? block-type "text")
            (loop (cdr blocks)
                  (cons (hash-ref block 'text) text-parts)
                  tool-calls))

           ;; Tool use block
           ((equal? block-type "tool_use")
            (let ((tc (make-tool-call-instance
                       (hash-ref block 'id)
                       (hash-ref block 'name)
                       (json-object->string (hash-ref block 'input)))))
              (loop (cdr blocks)
                    text-parts
                    (cons tc tool-calls))))

           ;; Unknown block type
           (else
            (loop (cdr blocks) text-parts tool-calls)))))))

(def (anthropic-stop-reason->symbol reason-str)
  "Convert Anthropic stop reason to standard symbol"
  (cond
   ((equal? reason-str "end_turn") :stop)
   ((equal? reason-str "tool_use") :tool-calls)
   ((equal? reason-str "max_tokens") :length)
   ((equal? reason-str "stop_sequence") :stop)
   (else :unknown)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(def (anthropic-simple prompt
                      #!key
                      (model "claude-3-5-sonnet-20241022")
                      (system-prompt "You are a helpful assistant.")
                      (max-tokens 4096)
                      (api-key #f))
  "Simple Anthropic chat with just a prompt

   Returns: string content of response"

  (let* ((messages (list (make-user-message prompt)))
         (response (anthropic-messages
                    messages
                    model: model
                    system: system-prompt
                    max-tokens: max-tokens
                    api-key: api-key)))

    (if (llm-response? response)
        (extract-text-content (llm-response-message response))
        (error "Anthropic request failed" response))))

(def (anthropic-with-tools prompt tools
                           #!key
                           (model "claude-3-5-sonnet-20241022")
                           (system-prompt "You are a helpful assistant.")
                           (max-tokens 4096)
                           (api-key #f))
  "Anthropic chat with tool support

   Returns: llm-response structure (check for tool calls)"

  (let ((messages (list (make-user-message prompt))))

    (anthropic-messages
     messages
     model: model
     system: system-prompt
     tools: tools
     max-tokens: max-tokens
     api-key: api-key)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (string->json-object str)
  "Parse JSON string to hash"
  (json-object<-string str))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Simple usage
(def response (anthropic-simple "Why is the sky blue? Be concise."))
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

(def response (anthropic-with-tools
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
