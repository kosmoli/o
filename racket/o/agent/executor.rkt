#lang racket

;;; agent/executor.rkt - Agent Step Executor
;;;
;;; Step-based execution engine for agent operations.

(provide (struct-out step-executor)
         make-step-executor
         execute-step
         execute-agent
         make-execution-step-record
         (struct-out execution-context)
         (struct-out execution-step)
         execution-context-agent-id
         execution-context-agent-config
         execution-context-agent-state
         execution-context-step-history
         execution-context-conversation-history
         execution-context-memory-blocks
         set-execution-context-step-history!
         execution-step-type
         execution-step-input
         execution-step-output
         execution-step-status
         execution-step-error
         execution-step-duration
         execution-step-step-number
         execution-step-timestamp
         set-execution-step-status!
         set-execution-step-output!
         set-execution-step-error!
         set-execution-step-duration!
         execute-user-message-step
         execute-llm-inference-step
         execute-tool-call-step
         execute-memory-update-step
         execute-system-step
         build-llm-messages
         build-system-message
         get-available-tools
         determine-next-step
         string-replace
         string-contains
         step-failed?)

(require racket/hash
        racket/format
        racket/string
        racket/match
        racket/function
        racket/list
        racket/contract
        racket/bool
        "./types.rkt"
        (prefix-in llmt: "../llm/types.rkt")
        "../llm/client.rkt"
        "../tools/core.rkt"
        "../tools/types.rkt"
        "../memory/blocks.rkt"
        "../memory/core.rkt"
        "../message/manager.rkt"
        (prefix-in msg: "../message/types.rkt"))

;;; ============================================================================
;;; Step Executor
;;; ============================================================================

;;; Step executor - coordinates LLM calls, tool execution, and memory management

(struct step-executor
  (llm-client            ; LLM client for inference
   tool-dispatcher       ; Tool dispatcher for tool calls
   message-manager       ; Message manager for conversation
   memory-manager        ; Memory block manager
   max-retries           ; Maximum retries for failed steps
   metadata)             ; Additional metadata
  #:transparent)

(define (make-step-executor llm-client tool-dispatcher message-manager memory-manager
                            #:max-retries [max-retries 3])
  "Create step executor

   Args:
     llm-client: LLM client for inference
     tool-dispatcher: Tool dispatcher for tool calls
     message-manager: Message manager for conversation
     memory-manager: Memory block manager
     max-retries: Maximum retries for failed steps (default: 3)

   Returns:
     Step executor instance

   Example:
     (make-step-executor llm-client tool-dispatcher
                         message-manager memory-manager
                         #:max-retries 5)"
  (step-executor
   llm-client
   tool-dispatcher
   message-manager
   memory-manager
   max-retries
   (hash)))

;;; ============================================================================
;;; Step Execution
;;; ============================================================================

(define (execute-step executor context step)
  "Execute a single step

   Args:
     executor: Step executor
     context: Execution context
     step: Execution step to execute

   Returns:
     Updated execution step with result, status, and duration

   The step status is updated to 'running during execution,
     then 'completed or 'failed based on the result.

   Step types:
   - 'user-message: Add user message to conversation
   - 'llm-inference: Call LLM for response generation
   - 'tool-call: Execute a tool call
   - 'memory-update: Update a memory block
   - 'system: Internal system operation"

  (define start-time (current-seconds))
  (set-execution-step-status! step 'running)

  (with-handlers* ([exn:fail?
                    (lambda (e)
                      ;; Step execution failed
                      (set-execution-step-status! step 'failed)
                      (set-execution-step-error! step (exn-message e))
                      (set-execution-step-duration! step (- (current-seconds) start-time))
                      step)])

    ;; Execute based on step type
    (define result
      (case (execution-step-type step)
        [(user-message)
         (execute-user-message-step executor context step)]
        [(llm-inference)
         (execute-llm-inference-step executor context step)]
        [(tool-call)
         (execute-tool-call-step executor context step)]
        [(memory-update)
         (execute-memory-update-step executor context step)]
        [(system)
         (execute-system-step executor context step)]
        [else
         (error 'execute-step "Unknown step type: ~a" (execution-step-type step))]))

    ;; Update step with result
    (set-execution-step-output! step result)
    (set-execution-step-status! step 'completed)
    (set-execution-step-duration! step (- (current-seconds) start-time))
    step))

(define (execute-user-message-step executor context step)
  "Execute user message step

   Args:
     executor: Step executor
     context: Execution context
     step: Execution step with user message in input

   Returns:
     Hash with message_id, content, and timestamp

   The user message is added to the conversation via message manager"
  (define input (execution-step-input step))
  (define message (manager-create-message!
                   (step-executor-message-manager executor)
                   'user
                   (hash-ref input 'content "")))
  (hash 'message_id (msg:message-id message)
        'content (msg:message-content message)
        'timestamp (msg:message-timestamp message)))

(define (execute-llm-inference-step executor context step)
  "Execute LLM inference step

   Args:
     executor: Step executor
     context: Execution context
     step: Execution step

   Returns:
     Hash with content, tool_calls, usage, and finish_reason

   The LLM is called with conversation history and available tools.
   Response includes generated content and any tool calls the LLM wants to make."
  (define config (execution-context-agent-config context))
  (define messages (build-llm-messages context))
  (define tools (get-available-tools executor config))
  (define llm-config (step-executor-llm-client executor))
  (define llm-params (agent-config-llm-config config))

  (define response
    (llm-chat-completion llm-config messages
                         #:tools tools
                         #:temperature (hash-ref llm-params 'temperature 0.7)
                         #:max-tokens (hash-ref llm-params 'max_tokens 4096)))

  (hash 'content (llm-response-content response)
        'tool_calls (llm-response-tool-calls response)
        'usage (hash 'prompt_tokens (llm-response-usage-prompt response)
                    'completion_tokens (llm-response-usage-completion response)
                    'total_tokens (llm-response-usage-total response))
        'finish_reason "stop"))

(define (execute-tool-call-step executor context step)
  "Execute tool call step

   Args:
     executor: Step executor
     context: Execution context
     step: Execution step with tool call in input

   Returns:
     Hash with tool_name, arguments, status, result, and error

   The tool is called through the dispatcher with its arguments.
   Status indicates if the call succeeded or failed."
  (define input (execution-step-input step))
  (define tool-name (hash-ref input 'tool_name))
  (define arguments (hash-ref input 'arguments (hash)))
  (define agent-id (execution-context-agent-id context))

  (define call (dispatcher-call-tool
                (step-executor-tool-dispatcher executor)
                tool-name
                arguments
                agent-id))

  (hash 'tool_name tool-name
        'arguments arguments
        'status (tool-execution-status call)
        'result (tool-execution-result call)
        'error (tool-execution-error call)))

(define (execute-memory-update-step executor context step)
  "Execute memory update step

   Args:
     executor: Step executor
     context: Execution context
     step: Execution step with memory operation in input

   Returns:
     Hash with operation details

   Supported operations:
   - 'append: Add content to end of memory block
   - 'replace: Replace old content with new content in block"
  (define input (execution-step-input step))
  (define operation (hash-ref input 'operation))
  (define block-name (hash-ref input 'block_name))
  (define content (hash-ref input 'content #f))
  (define manager (step-executor-memory-manager executor))
  (define agent-id (execution-context-agent-id context))

  (cond
   [(equal? operation "append")
    ;; Append to memory block
    (let* ([block (block-get manager block-name)]
           [current-value (if (hash? block) (hash-ref block 'value "") "")]
           [new-value (string-append current-value "\n" content)])
      (block-update! manager block-name new-value)
      (hash 'operation "append"
            'block_name block-name
            'content content))]

   [(equal? operation "replace")
    ;; Replace memory block content
    (let* ([old-content (hash-ref input 'old_content "")]
           [new-content (hash-ref input 'new_content "")]
           [block (block-get manager block-name)]
           [current-value (if (hash? block) (hash-ref block 'value "") "")]
           [updated (string-replace current-value old-content new-content)])
      (block-update! manager block-name updated)
      (hash 'operation "replace"
            'block_name block-name
            'old_content old-content
            'new_content new-content))]

   [else
    (error 'execute-memory-update-step "Unknown memory operation: ~a" operation)]))

(define (execute-system-step executor context step)
  "Execute system step

   Args:
     executor: Step executor
     context: Execution context
     step: Execution step

   Returns:
     Step input (passthrough for now)

   System steps are for internal operations that don't require
   external processing."
  (execution-step-input step))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (build-llm-messages context)
  "Build messages for LLM inference

   Args:
     context: Execution context

   Returns:
     List of LLM messages in chronological order

   The message list includes:
   1. System message with persona and memory blocks
   2. Conversation history messages"
  (define config (execution-context-agent-config context))
  (define memory-blocks (execution-context-memory-blocks context))
  (define history (execution-context-conversation-history context))

  (define messages '())

  ;; Add system message with persona and memory
  (set! messages
        (cons (llmt:make-system-message
               (build-system-message config memory-blocks))
              messages))

  ;; Add conversation history
  (for ([msg (in-list history)])
    (set! messages
          (cons (case (msg:message-role msg)
                  [(user) (llmt:make-user-message (msg:message-content msg))]
                  [(assistant) (llmt:make-assistant-message (msg:message-content msg))]
                  [(system) (llmt:make-system-message (msg:message-content msg))]
                  [(tool) (llmt:make-tool-message (msg:message-content msg) #f)]
                  [else (llmt:make-user-message (msg:message-content msg))])
                messages)))

  ;; Reverse to get chronological order
  (reverse messages))

(define (build-system-message config memory-blocks)
  "Build system message with persona and memory

   Args:
     config: Agent configuration
     memory-blocks: Memory blocks to include

   Returns:
     System message content as a string

   The system message includes:
   1. Agent persona
   2. Memory block contents (if provided)"
  (define parts '())

  ;; Add persona
  (set! parts (cons (agent-config-persona config) parts))

  ;; Add memory blocks
  (when memory-blocks
    (for ([block (in-list memory-blocks)])
      (set! parts
            (cons (format "~a: ~a"
                          (hash-ref block 'label "")
                          (hash-ref block 'value ""))
                  parts))))

  (string-join (reverse parts) "\n\n"))

(define (get-available-tools executor config)
  "Get available tools for agent

   Args:
     executor: Step executor
     config: Agent configuration

   Returns:
     List of tool definitions for enabled tools

   Only tools that are in the agent's enabled tools list
   and registered in the dispatcher are returned."
  (define enabled-tools (agent-config-tools-enabled config))
  (define dispatcher (step-executor-tool-dispatcher executor))
  (define registry (vector-ref dispatcher 0))

  (filter-map
   (lambda (tool-name)
     (registry-get-tool registry tool-name))
   enabled-tools))

(define (string-replace str old new)
  "Replace first occurrence of old with new in str

   Args:
     str: String to search
     old: String to find
     new: String to replace with

   Returns:
     Updated string with first occurrence replaced, or original if not found

   Example:
     (string-replace \"hello world\" \"world\" \"there\")
     ; => \"hello there\""
  (define pos (string-contains str old))
  (if pos
      (string-append
       (substring str 0 pos)
       new
       (substring str (+ pos (string-length old)) (string-length str)))
      str))

(define (string-contains str substr)
  "Check if str contains substr

   Args:
     str: String to search
     substr: Substring to find

   Returns:
     Position of substr in str or #f if not found

   Example:
     (string-contains \"hello world\" \"wor\")
     ; => 6"
  (define str-len (string-length str))
  (define sub-len (string-length substr))

  (define (loop i)
    (cond
     [(> (+ i sub-len) str-len) #f]
     [(string=? (substring str i (+ i sub-len)) substr) i]
     [else (loop (+ i 1))]))

  (loop 0))

;;; ============================================================================
;;; Execution Loop
;;; ============================================================================

(define (execute-agent executor context)
  "Execute agent with step-based execution

   Args:
     executor: Step executor
     context: Execution context

   Returns:
     Execution result with success status, steps executed, final state

   The execution loop continues until:
   - Maximum steps is reached
   - No more steps are available (determined by determine-next-step)
   - A step fails

   The agent state is updated throughout execution."
  (define start-time (current-seconds))
  (define config (execution-context-agent-config context))
  (define state (execution-context-agent-state context))
  (define max-steps (agent-config-max-steps config))

  ;; Update agent state to running
  (set-agent-state-status! state 'running)
  (set-agent-state-last-activity! state (current-seconds))

  (with-handlers* ([exn:fail?
                    (lambda (e)
                      ;; Execution error
                      (set-agent-state-status! state 'error)
                      (set-agent-state-error! state (exn-message e))
                      (make-execution-result
                       #f
                       (execution-context-agent-id context)
                       (agent-state-step-count state)
                       state
                       #f
                       (exn-message e)
                       (- (current-seconds) start-time)
                       (hash)))])

    ;; Execute steps until completion or max steps
    (define (loop step-num)
      (cond
       ;; Check if max steps reached
       [(>= step-num max-steps)
        (make-execution-result
         #t
         (execution-context-agent-id context)
         step-num
         state
         "Max steps reached"
         #f
         (- (current-seconds) start-time)
         (hash))]

       ;; Determine next step
       [else
        (define next-step (determine-next-step executor context))
        (if next-step
            ;; Execute step
            (let ((executed-step (execute-step executor context next-step)))
              ;; Add to step history
              (set-execution-context-step-history!
               context
               (cons executed-step (execution-context-step-history context)))

              ;; Update state
              (set-agent-state-step-count! state (+ (agent-state-step-count state) 1))
              (set-agent-state-last-activity! state (current-seconds))

              ;; Check if step failed
              (if (step-failed? executed-step)
                  ;; Step failed - return error result
                  (begin
                    (set-agent-state-status! state 'error)
                    (set-agent-state-error! state (execution-step-error executed-step))
                    (make-execution-result
                     #f
                     (execution-context-agent-id context)
                     (+ step-num 1)
                     state
                     #f
                     (execution-step-error executed-step)
                     (- (current-seconds) start-time)
                     (hash)))
                  ;; Step succeeded - continue
                  (loop (+ step-num 1))))

            ;; No more steps - execution complete
            (begin
              (set-agent-state-status! state 'completed)
              (make-execution-result
               #t
               (execution-context-agent-id context)
               step-num
               state
               "Execution complete"
               #f
               (- (current-seconds) start-time)
               (hash))))]))

    (loop 0)))

(define (determine-next-step executor context)
  "Determine next step to execute

   Args:
     executor: Step executor
     context: Execution context

   Returns:
     Next execution step or #f if no more steps

   Step determination logic:
   - If no history: Start with LLM inference
   - After user-message: LLM inference
   - After llm-inference: Tool call if available, or complete
   - After tool-call: LLM inference
   - After memory-update: Determine based on state
   - After system: Determine based on state

   This creates a conversation flow: User -> LLM -> Tools -> LLM -> ..."
  (define history (execution-context-step-history context))

  (cond
   [(null? history)
    ;; No steps yet - start with LLM inference
    (make-execution-step-record
     (execution-context-agent-id context)
     0
     'llm-inference
     (hash))]

   [else
    ;; Check last step to determine next action
    (define last-step (car history))
    (case (execution-step-type last-step)
      [(user-message)
       ;; After user message, do LLM inference
       (make-execution-step-record
        (execution-context-agent-id context)
        (+ (execution-step-step-number last-step) 1)
        'llm-inference
        (hash))]

      [(llm-inference)
       ;; After LLM inference, check if there are tool calls
       (define output (execution-step-output last-step))
       (define tool-calls (if output (hash-ref output 'tool_calls '()) '()))
       (if (and output (not (null? tool-calls)))
           ;; Execute first tool call
           (make-execution-step-record
            (execution-context-agent-id context)
            (+ (execution-step-step-number last-step) 1)
            'tool-call
            (car tool-calls))
           ;; No tool calls - execution complete
           #f)]

      [(tool-call)
       ;; After tool call, do another LLM inference
       (make-execution-step-record
        (execution-context-agent-id context)
        (+ (execution-step-step-number last-step) 1)
        'llm-inference
        (hash))]

      [(memory-update)
       ;; After memory update, do LLM inference
       (make-execution-step-record
        (execution-context-agent-id context)
        (+ (execution-step-step-number last-step) 1)
        'llm-inference
        (hash))]

      [(system)
       ;; After system step, do LLM inference
       (make-execution-step-record
        (execution-context-agent-id context)
        (+ (execution-step-step-number last-step) 1)
        'llm-inference
        (hash))]

      [else
       ;; Unknown step type - stop
       #f])]))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(define (step-failed? step)
  "Check if step execution failed

   Args:
     step: Execution step

   Returns:
     #t if step status is 'failed, #f otherwise"
  (eq? (execution-step-status step) 'failed))
