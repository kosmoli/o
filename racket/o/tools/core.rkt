#lang racket

;;; tools/core.rkt - Core Tool Implementations
;;;
;;; Core tools for agent operations: send_message and conversation_search.

(provide make-tool-registry
         registry-register-tool!
         registry-get-tool
         registry-list-tools
         registry-has-tool?
         execute-tool
         send-message-tool
         send-message-handler
         conversation-search-tool
         conversation-search-handler
         make-tool-dispatcher
         dispatcher-register-tool!
         dispatcher-call-tool
         dispatcher-execute-call!
         dispatcher-approve-call!
         dispatcher-reject-call!
         dispatcher-get-call
         dispatcher-get-history
         dispatcher-get-pending
         register-core-tools!)

(require racket/hash
         racket/format
         "../database/client.rkt"
         "../message/types.rkt"
         "types.rkt")

;;; ============================================================================
;;; Tool Registry
;;; ============================================================================

(define (make-tool-registry)
  "Create empty tool registry

   Returns:
     Tool registry structure"
  (tool-registry (hash) (hash)))

(define (registry-register-tool! registry tool-def)
  "Register tool in registry

   Args:
     registry: Tool registry
     tool-def: Tool definition

   Returns:
     #t on success"

  ;; Validate tool definition
  (define result (validate-tool-definition-internal tool-def))
  (unless (car result)
    (error 'registry-register-tool!
           "Invalid tool definition"
           (cdr result)))

  (define name (tool-definition-internal-name tool-def))
  (define category (tool-definition-internal-category tool-def))

  ;; Add to tools hash
  (hash-set! (tool-registry-tools registry) name tool-def)

  ;; Add to category
  (define category-tools (hash-ref (tool-registry-categories registry) category '()))
  (hash-set! (tool-registry-categories registry)
             category
             (cons name category-tools))

  (displayln (format "Registered tool: ~a (category: ~a)" name category))
  #t)

(define (registry-get-tool registry name)
  "Get tool definition by name

   Args:
     registry: Tool registry
     name: Tool name

   Returns:
     Tool definition or #f if not found"

  (hash-ref (tool-registry-tools registry) name #f))

(define (registry-list-tools registry #:category [category #f])
  "List all tools or tools in category

   Args:
     registry: Tool registry
     category: Optional category filter

   Returns:
     List of tool names"

  (if category
      (hash-ref (tool-registry-categories registry) category '())
      (hash-keys (tool-registry-tools registry))))

(define (registry-has-tool? registry name)
  "Check if tool exists in registry

   Args:
     registry: Tool registry
     name: Tool name

   Returns:
     #t if exists, #f otherwise"

  (hash-has-key? (tool-registry-tools registry) name))

;;; ============================================================================
;;; Tool Execution
;;; ============================================================================

(define (execute-tool registry tool-name arguments context)
  "Execute tool with given arguments

   Args:
     registry: Tool registry
     tool-name: Tool name
     arguments: Tool arguments (hash)
     context: Execution context

   Returns:
     Tool result structure"

  ;; Get tool definition
  (define tool-def (registry-get-tool registry tool-name))
  (if (not tool-def)
      (make-error-result (format "Tool not found: ~a" tool-name))

      ;; Validate arguments
      (let ([validation-result (validate-tool-arguments tool-def arguments)])
        (if (not (car validation-result))
            (make-error-result (format "Invalid arguments: ~a" (cdr validation-result)))

            ;; Execute tool handler
            (with-handlers ([exn:fail?
                             (lambda (e)
                               (make-error-result
                                (format "Tool execution error: ~a" (exn-message e))))])
              (define handler (tool-definition-internal-handler tool-def))
              (handler arguments context))))))

;;; ============================================================================
;;; Core Tool: send_message
;;; ============================================================================

(define (send-message-handler arguments context)
  "Handler for send_message tool

   Args:
     arguments: Hash with 'message' key
     context: Execution context

   Returns:
     Tool result with message ID"

  (define message-text (hash-ref arguments 'message))
  (define agent-id (tool-execution-context-agent-id context))

  ;; Create message
  (define message (make-assistant-message agent-id message-text))
  (define msg-id (format "~a-~a" (current-seconds) (random 1000000)))

  (make-success-result
   (hash 'message_id msg-id
         'content message-text
         'timestamp (message-timestamp message))
   #:metadata (hash 'tool "send_message")))

(define send-message-tool
  (tool-definition-internal
   "send_message"
   "Send a message to the user. Use this tool to communicate with the user, respond to their queries, or provide information."
   (hash 'message (hash 'type 'string
                         'description "The message content to send to the user"
                         'required #t))
   send-message-handler
   'core
   #f
   (hash 'version "1.0")))

;;; ============================================================================
;;; Core Tool: conversation_search
;;; ============================================================================

(define (conversation-search-handler arguments context)
  "Handler for conversation_search tool

   Args:
     arguments: Hash with 'query', 'limit', 'page' keys
     context: Execution context

   Returns:
     Tool result with matching messages"

  (define query (hash-ref arguments 'query))
  (define limit (hash-ref arguments 'limit 10))
  (define page (hash-ref arguments 'page 0))

  ;; Placeholder: return empty results
  (make-success-result
   (hash 'results '()
         'count 0
         'query query
         'page page)
   #:metadata (hash 'tool "conversation_search")))

(define conversation-search-tool
  (tool-definition-internal
   "conversation_search"
   "Search through conversation history. Use this tool to find previous messages, recall past discussions, or retrieve specific information from the conversation."
   (hash 'query (hash 'type 'string
                       'description "Search query to find matching messages"
                       'required #t)
         'limit (hash 'type 'integer
                      'description "Maximum number of results to return"
                      'required #f
                      'default 10)
         'page (hash 'type 'integer
                     'description "Page number for pagination (0-indexed)"
                     'required #f
                     'default 0))
   conversation-search-handler
   'core
   #f
   (hash 'version "1.0")))

;;; ============================================================================
;;; Tool Dispatcher
;;; ============================================================================

(define (make-tool-dispatcher)
  "Create tool dispatcher

   Returns:
     Tool dispatcher structure"

  (values (make-tool-registry) '() '()))

(define (dispatcher-register-tool! dispatcher tool-def)
  "Register tool with dispatcher

   Args:
     dispatcher: Tool dispatcher (values: registry call-history approval-queue)
     tool-def: Tool definition

   Returns:
     #t on success"

  (define registry (vector-ref dispatcher 0))
  (registry-register-tool! registry tool-def))

(define (dispatcher-call-tool dispatcher tool-name arguments agent-id)
  "Call tool through dispatcher

   Args:
     dispatcher: Tool dispatcher (vector: registry call-history approval-queue)
     tool-name: Tool name
     arguments: Tool arguments
     agent-id: Agent ID

   Returns:
     Tool execution structure"

  (define registry (vector-ref dispatcher 0))
  (define call-history (vector-ref dispatcher 1))
  (define approval-queue (vector-ref dispatcher 2))

  ;; Create tool call
  (define call (tool-execution
                (format "~a-~a" (current-seconds) (random 1000000))
                tool-name
                arguments
                (current-seconds)
                agent-id
                'pending
                #f
                #f))

  ;; Check if tool requires approval
  (define tool-def (registry-get-tool registry tool-name))
  (cond
   [(not tool-def)
    (set-tool-execution-status! call 'failed)
    (set-tool-execution-error! call (format "Tool not found: ~a" tool-name))
    call]

   [(tool-definition-internal-requires-approval tool-def)
    ;; Add to approval queue
    (vector-set! dispatcher 2 (cons call approval-queue))
    (displayln (format "Tool call ~a awaiting approval" (tool-execution-id call)))
    call]

   [else
    ;; Execute immediately
    (dispatcher-execute-call! dispatcher call)]))

(define (dispatcher-execute-call! dispatcher call)
  "Execute tool call

   Args:
     dispatcher: Tool dispatcher (vector)
     call: Tool execution structure

   Returns:
     Updated tool execution structure"

  (define registry (vector-ref dispatcher 0))

  ;; Update status to executing
  (set-tool-execution-status! call 'executing)

  ;; Create execution context
  (define context (tool-execution-context
                   (tool-execution-agent-id call)
                   (tool-execution-id call)
                   (tool-execution-arguments call)
                   (current-seconds)
                   (hash)))

  ;; Execute tool
  (define result (execute-tool registry
                              (tool-execution-tool-name call)
                              (tool-execution-arguments call)
                              context))

  ;; Update call with result
  (if (tool-result-success result)
      (begin
        (set-tool-execution-status! call 'completed)
        (set-tool-execution-result! call (tool-result-value result)))
      (begin
        (set-tool-execution-status! call 'failed)
        (set-tool-execution-error! call (tool-result-error result))))

  ;; Add to history
  (define call-history (vector-ref dispatcher 1))
  (vector-set! dispatcher 1 (cons call call-history))

  (displayln (format "Tool call ~a ~a"
                     (tool-execution-id call)
                     (if (tool-result-success result) "completed" "failed")))
  call)

(define (dispatcher-approve-call! dispatcher call-id)
  "Approve pending tool call

   Args:
     dispatcher: Tool dispatcher (vector)
     call-id: Tool call ID

   Returns:
     Updated tool call or #f if not found"

  (define approval-queue (vector-ref dispatcher 2))

  (define call
    (for/first ([c (in-list approval-queue)]
                #:when (equal? (tool-execution-id c) call-id))
      c))

  (if (not call)
      #f
      (begin
        ;; Remove from approval queue
        (vector-set! dispatcher 2
                     (filter (lambda (c)
                               (not (equal? (tool-execution-id c) call-id)))
                             approval-queue))

        ;; Update status and execute
        (set-tool-execution-status! call 'approved)
        (dispatcher-execute-call! dispatcher call))))

(define (dispatcher-reject-call! dispatcher call-id reason)
  "Reject pending tool call

   Args:
     dispatcher: Tool dispatcher (vector)
     call-id: Tool call ID
     reason: Rejection reason

   Returns:
     Updated tool call or #f if not found"

  (define approval-queue (vector-ref dispatcher 2))
  (define call-history (vector-ref dispatcher 1))

  (define call
    (for/first ([c (in-list approval-queue)]
                #:when (equal? (tool-execution-id c) call-id))
      c))

  (if (not call)
      #f
      (begin
        ;; Remove from approval queue
        (vector-set! dispatcher 2
                     (filter (lambda (c)
                               (not (equal? (tool-execution-id c) call-id)))
                             approval-queue))

        ;; Update status
        (set-tool-execution-status! call 'rejected)
        (set-tool-execution-error! call reason)

        ;; Add to history
        (vector-set! dispatcher 1 (cons call call-history))

        (displayln (format "Tool call ~a rejected: ~a" call-id reason))
        call)))

(define (dispatcher-get-call dispatcher call-id)
  "Get tool call by ID

   Args:
     dispatcher: Tool dispatcher (vector)
     call-id: Tool call ID

   Returns:
     Tool call or #f if not found"

  (define call-history (vector-ref dispatcher 1))
  (define approval-queue (vector-ref dispatcher 2))

  (or (for/first ([c (in-list call-history)]
                  #:when (equal? (tool-execution-id c) call-id))
        c)
      (for/first ([c (in-list approval-queue)]
                  #:when (equal? (tool-execution-id c) call-id))
        c)))

(define (dispatcher-get-history dispatcher #:limit [limit 10])
  "Get tool call history

   Args:
     dispatcher: Tool dispatcher (vector)
     limit: Maximum number of calls to return

   Returns:
     List of tool calls"

  (define call-history (vector-ref dispatcher 1))
  (take call-history (min limit (length call-history))))

(define (dispatcher-get-pending dispatcher)
  "Get pending tool calls

   Args:
     dispatcher: Tool dispatcher (vector)

   Returns:
     List of pending tool calls"

  (vector-ref dispatcher 2))

;;; ============================================================================
;;; Initialize Core Tools
;;; ============================================================================

(define (register-core-tools! dispatcher)
  "Register all core tools with dispatcher

   Args:
     dispatcher: Tool dispatcher (vector)

   Returns:
     #t on success"

  (dispatcher-register-tool! dispatcher send-message-tool)
  (dispatcher-register-tool! dispatcher conversation-search-tool)
  (displayln "Core tools registered")
  #t)
