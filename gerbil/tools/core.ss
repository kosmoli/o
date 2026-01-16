;;; tools/core.ss - Core Tool Implementations
;;;
;;; Core tools for agent operations: send_message and conversation_search.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/text/json
  :std/sort
  ../database/client
  ../message/manager
  ./types)

;;; ============================================================================
;;; Tool Registry
;;; ============================================================================

(def (make-tool-registry)
  "Create empty tool registry

   Returns:
     Tool registry structure"

  (make-tool-registry
   tools: (hash)
   categories: (hash)))

(def (registry-register-tool! registry tool-def)
  "Register tool in registry

   Args:
     registry: Tool registry
     tool-def: Tool definition

   Returns:
     #t on success"

  ;; Validate tool definition
  (let ((result (validate-tool-definition tool-def)))
    (unless (car result)
      (error "Invalid tool definition" errors: (cdr result))))

  (let ((name (tool-definition-name tool-def))
        (category (tool-definition-category tool-def)))

    ;; Add to tools hash
    (hash-put! (tool-registry-tools registry) name tool-def)

    ;; Add to category
    (let ((category-tools (hash-ref (tool-registry-categories registry) category '())))
      (hash-put! (tool-registry-categories registry)
                 category
                 (cons name category-tools)))

    (displayln (format "Registered tool: ~a (category: ~a)" name category))
    #t))

(def (registry-get-tool registry name)
  "Get tool definition by name

   Args:
     registry: Tool registry
     name: Tool name

   Returns:
     Tool definition or #f if not found"

  (hash-ref (tool-registry-tools registry) name #f))

(def (registry-list-tools registry #!key (category #f))
  "List all tools or tools in category

   Args:
     registry: Tool registry
     category: Optional category filter

   Returns:
     List of tool names"

  (if category
      (hash-ref (tool-registry-categories registry) category '())
      (hash-keys (tool-registry-tools registry))))

(def (registry-has-tool? registry name)
  "Check if tool exists in registry

   Args:
     registry: Tool registry
     name: Tool name

   Returns:
     #t if exists, #f otherwise"

  (hash-key? (tool-registry-tools registry) name))

;;; ============================================================================
;;; Tool Execution
;;; ============================================================================

(def (execute-tool registry tool-name arguments context)
  "Execute tool with given arguments

   Args:
     registry: Tool registry
     tool-name: Tool name
     arguments: Tool arguments (hash)
     context: Execution context

   Returns:
     Tool result structure"

  ;; Get tool definition
  (let ((tool-def (registry-get-tool registry tool-name)))
    (if (not tool-def)
        (make-error-result (format "Tool not found: ~a" tool-name))

        ;; Validate arguments
        (let ((validation-result (validate-tool-arguments tool-def arguments)))
          (if (not (car validation-result))
              (make-error-result (format "Invalid arguments: ~a" (cdr validation-result)))

              ;; Execute tool handler
              (try
               (let ((handler (tool-definition-handler tool-def)))
                 (handler arguments context))
               (catch (e)
                 (make-error-result (format "Tool execution error: ~a" (error-message e))))))))))

;;; ============================================================================
;;; Core Tool: send_message
;;; ============================================================================

(def (send-message-handler arguments context)
  "Handler for send_message tool

   Args:
     arguments: Hash with 'message' key
     context: Execution context

   Returns:
     Tool result with message ID"

  (let ((message-text (hash-ref arguments 'message))
        (agent-id (tool-execution-context-agent-id context)))

    ;; Create message manager
    (let ((manager (make-message-manager agent-id)))

      ;; Create assistant message
      (let ((message (message-create! manager
                                     :assistant
                                     message-text)))

        (make-success-result
         (hash 'message_id (hash-ref message 'id)
               'content message-text
               'timestamp (hash-ref message 'created_at))
         metadata: (hash 'tool "send_message"))))))

(def send-message-tool
  (make-tool-definition
   name: "send_message"
   description: "Send a message to the user. Use this tool to communicate with the user, respond to their queries, or provide information."
   parameters: (hash
                'message (hash 'type :string
                              'description "The message content to send to the user"
                              'required #t))
   handler: send-message-handler
   category: :core
   requires-approval: #f
   metadata: (hash 'version "1.0")))

;;; ============================================================================
;;; Core Tool: conversation_search
;;; ============================================================================

(def (conversation-search-handler arguments context)
  "Handler for conversation_search tool

   Args:
     arguments: Hash with 'query', 'limit', 'page' keys
     context: Execution context

   Returns:
     Tool result with matching messages"

  (let ((query (hash-ref arguments 'query))
        (limit (hash-ref arguments 'limit 10))
        (page (hash-ref arguments 'page 0))
        (agent-id (tool-execution-context-agent-id context)))

    ;; Create message manager
    (let ((manager (make-message-manager agent-id)))

      ;; Search messages
      (let ((results (message-search manager query
                                    limit: limit
                                    offset: (* page limit))))

        ;; Format results
        (let ((formatted-results
               (map (lambda (msg)
                     (hash 'id (hash-ref msg 'id)
                           'role (symbol->string (hash-ref msg 'role))
                           'content (hash-ref msg 'content)
                           'timestamp (hash-ref msg 'created_at)))
                   results)))

          (make-success-result
           (hash 'results formatted-results
                 'count (length formatted-results)
                 'query query
                 'page page)
           metadata: (hash 'tool "conversation_search")))))))

(def conversation-search-tool
  (make-tool-definition
   name: "conversation_search"
   description: "Search through conversation history. Use this tool to find previous messages, recall past discussions, or retrieve specific information from the conversation."
   parameters: (hash
                'query (hash 'type :string
                            'description "Search query to find matching messages"
                            'required #t)
                'limit (hash 'type :integer
                            'description "Maximum number of results to return"
                            'required #f
                            'default 10)
                'page (hash 'type :integer
                           'description "Page number for pagination (0-indexed)"
                           'required #f
                           'default 0))
   handler: conversation-search-handler
   category: :core
   requires-approval: #f
   metadata: (hash 'version "1.0")))

;;; ============================================================================
;;; Tool Dispatcher
;;; ============================================================================

(defstruct tool-dispatcher
  (registry          ; Tool registry
   call-history      ; List of tool calls
   approval-queue)   ; Queue of calls awaiting approval
  transparent: #t)

(def (make-tool-dispatcher)
  "Create tool dispatcher

   Returns:
     Tool dispatcher structure"

  (make-tool-dispatcher
   registry: (make-tool-registry)
   call-history: '()
   approval-queue: '()))

(def (dispatcher-register-tool! dispatcher tool-def)
  "Register tool with dispatcher

   Args:
     dispatcher: Tool dispatcher
     tool-def: Tool definition

   Returns:
     #t on success"

  (registry-register-tool! (tool-dispatcher-registry dispatcher) tool-def))

(def (dispatcher-call-tool dispatcher tool-name arguments agent-id)
  "Call tool through dispatcher

   Args:
     dispatcher: Tool dispatcher
     tool-name: Tool name
     arguments: Tool arguments
     agent-id: Agent ID

   Returns:
     Tool call structure"

  ;; Create tool call
  (let ((call (make-tool-call
               id: (uuid-generate)
               tool-name: tool-name
               arguments: arguments
               timestamp: (current-seconds)
               agent-id: agent-id
               status: :pending
               result: #f
               error: #f)))

    ;; Check if tool requires approval
    (let ((tool-def (registry-get-tool (tool-dispatcher-registry dispatcher) tool-name)))
      (if (not tool-def)
          (begin
            (tool-call-status-set! call :failed)
            (tool-call-error-set! call (format "Tool not found: ~a" tool-name))
            call)

          (if (tool-definition-requires-approval tool-def)
              ;; Add to approval queue
              (begin
                (set! (tool-dispatcher-approval-queue dispatcher)
                      (cons call (tool-dispatcher-approval-queue dispatcher)))
                (displayln (format "Tool call ~a awaiting approval" (tool-call-id call)))
                call)

              ;; Execute immediately
              (dispatcher-execute-call! dispatcher call))))))

(def (dispatcher-execute-call! dispatcher call)
  "Execute tool call

   Args:
     dispatcher: Tool dispatcher
     call: Tool call structure

   Returns:
     Updated tool call structure"

  ;; Update status to executing
  (tool-call-status-set! call :executing)

  ;; Create execution context
  (let ((context (make-tool-execution-context
                  agent-id: (tool-call-agent-id call)
                  call-id: (tool-call-id call)
                  arguments: (tool-call-arguments call)
                  timestamp: (current-seconds)
                  metadata: (hash))))

    ;; Execute tool
    (let ((result (execute-tool (tool-dispatcher-registry dispatcher)
                                (tool-call-tool-name call)
                                (tool-call-arguments call)
                                context)))

      ;; Update call with result
      (if (tool-result-success result)
          (begin
            (tool-call-status-set! call :completed)
            (tool-call-result-set! call (tool-result-value result)))
          (begin
            (tool-call-status-set! call :failed)
            (tool-call-error-set! call (tool-result-error result))))

      ;; Add to history
      (set! (tool-dispatcher-call-history dispatcher)
            (cons call (tool-dispatcher-call-history dispatcher)))

      (displayln (format "Tool call ~a ~a"
                        (tool-call-id call)
                        (if (tool-result-success result) "completed" "failed")))
      call)))

(def (dispatcher-approve-call! dispatcher call-id)
  "Approve pending tool call

   Args:
     dispatcher: Tool dispatcher
     call-id: Tool call ID

   Returns:
     Updated tool call or #f if not found"

  (let ((call (find (lambda (c) (equal? (tool-call-id c) call-id))
                   (tool-dispatcher-approval-queue dispatcher))))
    (if (not call)
        #f
        (begin
          ;; Remove from approval queue
          (set! (tool-dispatcher-approval-queue dispatcher)
                (filter (lambda (c) (not (equal? (tool-call-id c) call-id)))
                       (tool-dispatcher-approval-queue dispatcher)))

          ;; Update status and execute
          (tool-call-status-set! call :approved)
          (dispatcher-execute-call! dispatcher call)))))

(def (dispatcher-reject-call! dispatcher call-id reason)
  "Reject pending tool call

   Args:
     dispatcher: Tool dispatcher
     call-id: Tool call ID
     reason: Rejection reason

   Returns:
     Updated tool call or #f if not found"

  (let ((call (find (lambda (c) (equal? (tool-call-id c) call-id))
                   (tool-dispatcher-approval-queue dispatcher))))
    (if (not call)
        #f
        (begin
          ;; Remove from approval queue
          (set! (tool-dispatcher-approval-queue dispatcher)
                (filter (lambda (c) (not (equal? (tool-call-id c) call-id)))
                       (tool-dispatcher-approval-queue dispatcher)))

          ;; Update status
          (tool-call-status-set! call :rejected)
          (tool-call-error-set! call reason)

          ;; Add to history
          (set! (tool-dispatcher-call-history dispatcher)
                (cons call (tool-dispatcher-call-history dispatcher)))

          (displayln (format "Tool call ~a rejected: ~a" call-id reason))
          call))))

(def (dispatcher-get-call dispatcher call-id)
  "Get tool call by ID

   Args:
     dispatcher: Tool dispatcher
     call-id: Tool call ID

   Returns:
     Tool call or #f if not found"

  (or (find (lambda (c) (equal? (tool-call-id c) call-id))
           (tool-dispatcher-call-history dispatcher))
      (find (lambda (c) (equal? (tool-call-id c) call-id))
           (tool-dispatcher-approval-queue dispatcher))))

(def (dispatcher-get-history dispatcher #!key (limit 10))
  "Get tool call history

   Args:
     dispatcher: Tool dispatcher
     limit: Maximum number of calls to return

   Returns:
     List of tool calls"

  (take (tool-dispatcher-call-history dispatcher)
        (min limit (length (tool-dispatcher-call-history dispatcher)))))

(def (dispatcher-get-pending dispatcher)
  "Get pending tool calls

   Args:
     dispatcher: Tool dispatcher

   Returns:
     List of pending tool calls"

  (tool-dispatcher-approval-queue dispatcher))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (uuid-generate)
  "Generate UUID (simplified version)

   Returns:
     UUID string"

  (let ((timestamp (current-seconds))
        (random (random-integer 1000000)))
    (format "~a-~a" timestamp random)))

;;; ============================================================================
;;; Initialize Core Tools
;;; ============================================================================

(def (register-core-tools! dispatcher)
  "Register all core tools with dispatcher

   Args:
     dispatcher: Tool dispatcher

   Returns:
     #t on success"

  (dispatcher-register-tool! dispatcher send-message-tool)
  (dispatcher-register-tool! dispatcher conversation-search-tool)
  (displayln "Core tools registered")
  #t)

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create dispatcher
(def dispatcher (make-tool-dispatcher))

;; Register core tools
(register-core-tools! dispatcher)

;; Call send_message tool
(def call (dispatcher-call-tool dispatcher
                               "send_message"
                               (hash 'message "Hello, user!")
                               agent-id))

;; Check result
(displayln (format "Call status: ~a" (tool-call-status call)))
(displayln (format "Call result: ~a" (tool-call-result call)))

;; Search conversation
(def search-call (dispatcher-call-tool dispatcher
                                      "conversation_search"
                                      (hash 'query "hello" 'limit 5)
                                      agent-id))

;; Get call history
(def history (dispatcher-get-history dispatcher limit: 10))
(displayln (format "Call history: ~a calls" (length history)))
|#
