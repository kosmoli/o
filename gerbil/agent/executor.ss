;;; agent/executor.ss - Agent Step Executor
;;;
;;; Step-based execution engine for agent operations.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/error
  :o/llm/client
  :o/llm/types
  :o/tools/core
  :o/tools/types
  :o/memory/blocks
  :o/memory/core
  :o/message/manager
  :o/message/types
  :o/agent/types)

;;; ============================================================================
;;; Step Executor
;;; ============================================================================

(defstruct step-executor
  (llm-client            ; LLM client for inference
   tool-dispatcher       ; Tool dispatcher for tool calls
   message-manager       ; Message manager for conversation
   memory-manager        ; Memory block manager
   max-retries           ; Maximum retries for failed steps
   metadata)             ; Additional metadata
  transparent: #t)

(def (make-step-executor llm-client tool-dispatcher message-manager memory-manager
                        #!key (max-retries 3))
  "Create step executor

   Args:
     llm-client: LLM client
     tool-dispatcher: Tool dispatcher
     message-manager: Message manager
     memory-manager: Memory block manager
     max-retries: Maximum retries for failed steps

   Returns:
     Step executor"

  (make-step-executor
   llm-client: llm-client
   tool-dispatcher: tool-dispatcher
   message-manager: message-manager
   memory-manager: memory-manager
   max-retries: max-retries
   metadata: (hash)))

;;; ============================================================================
;;; Step Execution
;;; ============================================================================

(def (execute-step executor context step)
  "Execute a single step

   Args:
     executor: Step executor
     context: Execution context
     step: Execution step to execute

   Returns:
     Updated execution step"

  (let ((start-time (current-seconds)))
    ;; Update step status to running
    (execution-step-status-set! step step-status-running)

    (try
     ;; Execute based on step type
     (let ((result
            (case (execution-step-type step)
              ((:user-message)
               (execute-user-message-step executor context step))
              ((:llm-inference)
               (execute-llm-inference-step executor context step))
              ((:tool-call)
               (execute-tool-call-step executor context step))
              ((:memory-update)
               (execute-memory-update-step executor context step))
              ((:system)
               (execute-system-step executor context step))
              (else
               (error "Unknown step type" (execution-step-type step))))))

       ;; Update step with result
       (execution-step-output-set! step result)
       (execution-step-status-set! step step-status-completed)
       (execution-step-duration-set! step (- (current-seconds) start-time))
       step)

     (catch (e)
       ;; Step execution failed
       (execution-step-status-set! step step-status-failed)
       (execution-step-error-set! step (error-message e))
       (execution-step-duration-set! step (- (current-seconds) start-time))
       step))))

(def (execute-user-message-step executor context step)
  "Execute user message step

   Args:
     executor: Step executor
     context: Execution context
     step: Execution step

   Returns:
     Step output"

  (let ((input (execution-step-input step)))
    ;; Add user message to conversation
    (let ((message (message-manager-create-message
                    (step-executor-message-manager executor)
                    agent-id: (execution-context-agent-id context)
                    role: :user
                    content: (hash-ref input 'content)
                    metadata: (hash))))
      (hash 'message_id (message-id message)
            'content (message-content message)
            'timestamp (message-timestamp message)))))

(def (execute-llm-inference-step executor context step)
  "Execute LLM inference step

   Args:
     executor: Step executor
     context: Execution context
     step: Execution step

   Returns:
     Step output"

  (let* ((config (execution-context-agent-config context))
         (input (execution-step-input step))

         ;; Build messages for LLM
         (messages (build-llm-messages context))

         ;; Get available tools
         (tools (get-available-tools executor config))

         ;; Call LLM
         (response (llm-client-chat
                    (step-executor-llm-client executor)
                    messages: messages
                    tools: tools
                    temperature: (hash-ref (agent-config-llm-config config) 'temperature 0.7)
                    max-tokens: (hash-ref (agent-config-llm-config config) 'max_tokens 4096))))

    ;; Extract response content and tool calls
    (hash 'content (llm-response-content response)
          'tool_calls (llm-response-tool-calls response)
          'usage (llm-response-usage response)
          'finish_reason (llm-response-finish-reason response))))

(def (execute-tool-call-step executor context step)
  "Execute tool call step

   Args:
     executor: Step executor
     context: Execution context
     step: Execution step

   Returns:
     Step output"

  (let* ((input (execution-step-input step))
         (tool-name (hash-ref input 'tool_name))
         (arguments (hash-ref input 'arguments))
         (agent-id (execution-context-agent-id context)))

    ;; Call tool through dispatcher
    (let ((call (dispatcher-call-tool
                 (step-executor-tool-dispatcher executor)
                 tool-name
                 arguments
                 agent-id)))

      ;; Return tool call result
      (hash 'tool_name tool-name
            'arguments arguments
            'status (tool-call-status call)
            'result (tool-call-result call)
            'error (tool-call-error call)))))

(def (execute-memory-update-step executor context step)
  "Execute memory update step

   Args:
     executor: Step executor
     context: Execution context
     step: Execution step

   Returns:
     Step output"

  (let* ((input (execution-step-input step))
         (operation (hash-ref input 'operation))
         (block-name (hash-ref input 'block_name))
         (content (hash-ref input 'content #f))
         (manager (step-executor-memory-manager executor))
         (agent-id (execution-context-agent-id context)))

    (case operation
      ((:append)
       ;; Append to memory block
       (let ((block (memory-block-manager-get-block manager agent-id block-name)))
         (memory-block-manager-update-block! manager agent-id block-name
                                            (string-append (memory-block-value block) "\n" content))
         (hash 'operation "append"
               'block_name block-name
               'content content)))

      ((:replace)
       ;; Replace memory block content
       (let ((old-content (hash-ref input 'old_content))
             (new-content (hash-ref input 'new_content))
             (block (memory-block-manager-get-block manager agent-id block-name)))
         (let ((updated (string-replace (memory-block-value block) old-content new-content)))
           (memory-block-manager-update-block! manager agent-id block-name updated)
           (hash 'operation "replace"
                 'block_name block-name
                 'old_content old-content
                 'new_content new-content))))

      (else
       (error "Unknown memory operation" operation)))))

(def (execute-system-step executor context step)
  "Execute system step

   Args:
     executor: Step executor
     context: Execution context
     step: Execution step

   Returns:
     Step output"

  (let ((input (execution-step-input step)))
    ;; System steps are for internal operations
    ;; Return input as output for now
    input))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(def (build-llm-messages context)
  "Build messages for LLM inference

   Args:
     context: Execution context

   Returns:
     List of LLM messages"

  (let* ((config (execution-context-agent-config context))
         (memory-blocks (execution-context-memory-blocks context))
         (history (execution-context-conversation-history context))
         (messages '()))

    ;; Add system message with persona and memory
    (set! messages
          (cons (make-llm-message
                 role: :system
                 content: (build-system-message config memory-blocks))
                messages))

    ;; Add conversation history
    (for-each
     (lambda (msg)
       (set! messages
             (cons (make-llm-message
                    role: (message-role msg)
                    content: (message-content msg))
                   messages)))
     history)

    ;; Reverse to get chronological order
    (reverse messages)))

(def (build-system-message config memory-blocks)
  "Build system message with persona and memory

   Args:
     config: Agent configuration
     memory-blocks: Memory blocks

   Returns:
     System message content"

  (let ((parts '()))
    ;; Add persona
    (set! parts (cons (agent-config-persona config) parts))

    ;; Add memory blocks
    (when memory-blocks
      (for-each
       (lambda (block)
         (set! parts
               (cons (format "~a: ~a"
                            (memory-block-label block)
                            (memory-block-value block))
                     parts)))
       memory-blocks))

    ;; Join parts
    (string-join (reverse parts) "\n\n")))

(def (get-available-tools executor config)
  "Get available tools for agent

   Args:
     executor: Step executor
     config: Agent configuration

   Returns:
     List of tool definitions"

  (let ((enabled-tools (agent-config-tools-enabled config))
        (registry (tool-dispatcher-registry (step-executor-tool-dispatcher executor))))

    ;; Get tool definitions for enabled tools
    (filter-map
     (lambda (tool-name)
       (registry-get-tool registry tool-name))
     enabled-tools)))

(def (string-replace str old new)
  "Replace first occurrence of old with new in str

   Args:
     str: String to search
     old: String to find
     new: String to replace with

   Returns:
     Updated string"

  (let ((pos (string-contains str old)))
    (if pos
        (string-append
         (substring str 0 pos)
         new
         (substring str (+ pos (string-length old)) (string-length str)))
        str)))

;;; ============================================================================
;;; Execution Loop
;;; ============================================================================

(def (execute-agent executor context)
  "Execute agent with step-based execution

   Args:
     executor: Step executor
     context: Execution context

   Returns:
     Execution result"

  (let ((start-time (current-seconds))
        (config (execution-context-agent-config context))
        (state (execution-context-agent-state context))
        (max-steps (agent-config-max-steps config)))

    ;; Update agent state to running
    (agent-state-status-set! state agent-status-running)
    (agent-state-last-activity-set! state (current-seconds))

    (try
     ;; Execute steps until completion or max steps
     (let loop ((step-num 0))
       (cond
        ;; Check if max steps reached
        ((>= step-num max-steps)
         (make-execution-result
          success: #t
          agent-id: (execution-context-agent-id context)
          steps-executed: step-num
          final-state: state
          output: "Max steps reached"
          error: #f
          duration: (- (current-seconds) start-time)
          metadata: (hash)))

        ;; Continue execution
        (else
         ;; Determine next step
         (let ((next-step (determine-next-step executor context)))
           (if next-step
               (begin
                 ;; Execute step
                 (let ((executed-step (execute-step executor context next-step)))
                   ;; Add to step history
                   (execution-context-step-history-set!
                    context
                    (cons executed-step (execution-context-step-history context)))

                   ;; Update state
                   (agent-state-step-count-set! state (+ (agent-state-step-count state) 1))
                   (agent-state-last-activity-set! state (current-seconds))

                   ;; Check if step failed
                   (if (step-failed? executed-step)
                       ;; Step failed - return error result
                       (begin
                         (agent-state-status-set! state agent-status-error)
                         (agent-state-error-set! state (execution-step-error executed-step))
                         (make-execution-result
                          success: #f
                          agent-id: (execution-context-agent-id context)
                          steps-executed: (+ step-num 1)
                          final-state: state
                          output: #f
                          error: (execution-step-error executed-step)
                          duration: (- (current-seconds) start-time)
                          metadata: (hash)))
                       ;; Step succeeded - continue
                       (loop (+ step-num 1)))))

               ;; No more steps - execution complete
               (begin
                 (agent-state-status-set! state agent-status-completed)
                 (make-execution-result
                  success: #t
                  agent-id: (execution-context-agent-id context)
                  steps-executed: step-num
                  final-state: state
                  output: "Execution complete"
                  error: #f
                  duration: (- (current-seconds) start-time)
                  metadata: (hash))))))))

     (catch (e)
       ;; Execution error
       (agent-state-status-set! state agent-status-error)
       (agent-state-error-set! state (error-message e))
       (make-execution-result
        success: #f
        agent-id: (execution-context-agent-id context)
        steps-executed: (agent-state-step-count state)
        final-state: state
        output: #f
        error: (error-message e)
        duration: (- (current-seconds) start-time)
        metadata: (hash))))))

(def (determine-next-step executor context)
  "Determine next step to execute

   Args:
     executor: Step executor
     context: Execution context

   Returns:
     Next execution step or #f if no more steps"

  ;; Simple implementation: check if there are pending messages
  ;; In a full implementation, this would use more sophisticated logic

  (let ((history (execution-context-step-history context)))
    (if (null? history)
        ;; No steps yet - start with LLM inference
        (make-execution-step-record
         (execution-context-agent-id context)
         0
         step-type-llm-inference
         (hash))

        ;; Check last step
        (let ((last-step (car history)))
          (case (execution-step-type last-step)
            ((:user-message)
             ;; After user message, do LLM inference
             (make-execution-step-record
              (execution-context-agent-id context)
              (+ (execution-step-step-number last-step) 1)
              step-type-llm-inference
              (hash)))

            ((:llm-inference)
             ;; After LLM inference, check if there are tool calls
             (let ((output (execution-step-output last-step)))
               (if (and output (not (null? (hash-ref output 'tool_calls '()))))
                   ;; Execute first tool call
                   (let ((tool-call (car (hash-ref output 'tool_calls))))
                     (make-execution-step-record
                      (execution-context-agent-id context)
                      (+ (execution-step-step-number last-step) 1)
                      step-type-tool-call
                      tool-call))
                   ;; No tool calls - execution complete
                   #f)))

            ((:tool-call)
             ;; After tool call, do another LLM inference
             (make-execution-step-record
              (execution-context-agent-id context)
              (+ (execution-step-step-number last-step) 1)
              step-type-llm-inference
              (hash)))

            (else
             ;; Unknown step type - stop
             #f))))))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(def (string-contains str substr)
  "Check if str contains substr

   Args:
     str: String to search
     substr: Substring to find

   Returns:
     Position of substr in str or #f if not found"

  (let ((str-len (string-length str))
        (sub-len (string-length substr)))
    (let loop ((i 0))
      (cond
       ((> (+ i sub-len) str-len) #f)
       ((string=? (substring str i (+ i sub-len)) substr) i)
       (else (loop (+ i 1)))))))
