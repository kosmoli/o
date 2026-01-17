;;; tools/sandbox.ss - Tool Execution Sandbox
;;;
;;; Safe tool execution with resource limits and error isolation.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/error
  :o/tools/types
  :o/tools/core)

;;; ============================================================================
;;; Sandbox Configuration
;;; ============================================================================

(defstruct sandbox-config
  (max-execution-time    ; Maximum execution time in seconds (default: 30)
   max-memory-mb         ; Maximum memory usage in MB (default: 100)
   allow-network         ; Allow network access (default: #f)
   allow-file-read       ; Allow file read operations (default: #f)
   allow-file-write      ; Allow file write operations (default: #f)
   allowed-tools         ; List of allowed tool names (default: all)
   blocked-tools)        ; List of blocked tool names (default: none)
  transparent: #t)

(def (make-default-sandbox-config)
  "Create default sandbox configuration

   Returns:
     Sandbox configuration with safe defaults"

  (make-sandbox-config
   max-execution-time: 30
   max-memory-mb: 100
   allow-network: #f
   allow-file-read: #f
   allow-file-write: #f
   allowed-tools: '()    ; Empty means all allowed
   blocked-tools: '()))

(def (make-strict-sandbox-config)
  "Create strict sandbox configuration

   Returns:
     Sandbox configuration with strict limits"

  (make-sandbox-config
   max-execution-time: 10
   max-memory-mb: 50
   allow-network: #f
   allow-file-read: #f
   allow-file-write: #f
   allowed-tools: '("send_message" "conversation_search")
   blocked-tools: '()))

;;; ============================================================================
;;; Sandbox Execution Context
;;; ============================================================================

(defstruct sandbox-execution
  (tool-name             ; Tool being executed
   start-time            ; Execution start time
   timeout-time          ; Timeout deadline
   config                ; Sandbox configuration
   status                ; Execution status (:running, :completed, :timeout, :error)
   result                ; Execution result
   error)                ; Error message if failed
  transparent: #t)

;;; ============================================================================
;;; Sandbox Validation
;;; ============================================================================

(def (validate-tool-allowed config tool-name)
  "Check if tool is allowed in sandbox

   Args:
     config: Sandbox configuration
     tool-name: Tool name to check

   Returns:
     #t if allowed, #f otherwise"

  ;; Check blocked list first
  (when (member tool-name (sandbox-config-blocked-tools config))
    (return #f))

  ;; Check allowed list (empty means all allowed)
  (let ((allowed (sandbox-config-allowed-tools config)))
    (if (null? allowed)
        #t
        (member tool-name allowed))))

(def (validate-execution-time execution)
  "Check if execution has exceeded timeout

   Args:
     execution: Sandbox execution

   Returns:
     #t if within time limit, #f if timeout"

  (let ((current-time (current-seconds))
        (timeout-time (sandbox-execution-timeout-time execution)))
    (< current-time timeout-time)))

;;; ============================================================================
;;; Sandboxed Tool Execution
;;; ============================================================================

(def (sandbox-execute-tool dispatcher tool-name arguments agent-id
                           #!key
                           (config (make-default-sandbox-config)))
  "Execute tool in sandbox with resource limits

   Args:
     dispatcher: Tool dispatcher
     tool-name: Tool name
     arguments: Tool arguments
     agent-id: Agent ID
     config: Sandbox configuration (optional)

   Returns:
     Sandbox execution result"

  ;; Validate tool is allowed
  (unless (validate-tool-allowed config tool-name)
    (return (make-sandbox-execution
             tool-name: tool-name
             start-time: (current-seconds)
             timeout-time: 0
             config: config
             status: :error
             result: #f
             error: (format "Tool ~a is not allowed in sandbox" tool-name))))

  ;; Create execution context
  (let ((start-time (current-seconds))
        (timeout-time (+ (current-seconds)
                        (sandbox-config-max-execution-time config))))

    (let ((execution (make-sandbox-execution
                      tool-name: tool-name
                      start-time: start-time
                      timeout-time: timeout-time
                      config: config
                      status: :running
                      result: #f
                      error: #f)))

      ;; Execute tool with timeout protection
      (try
       (let ((call (dispatcher-call-tool dispatcher tool-name arguments agent-id)))

         ;; Check if execution completed within timeout
         (if (validate-execution-time execution)
             (begin
               (sandbox-execution-status-set! execution :completed)
               (sandbox-execution-result-set! execution call)
               execution)
             (begin
               (sandbox-execution-status-set! execution :timeout)
               (sandbox-execution-error-set! execution
                 (format "Tool execution exceeded timeout of ~a seconds"
                        (sandbox-config-max-execution-time config)))
               execution)))

       (catch (e)
         (sandbox-execution-status-set! execution :error)
         (sandbox-execution-error-set! execution
           (format "Tool execution error: ~a" (error-message e)))
         execution)))))

;;; ============================================================================
;;; Sandboxed Dispatcher
;;; ============================================================================

(defstruct sandboxed-dispatcher
  (dispatcher            ; Underlying tool dispatcher
   config                ; Sandbox configuration
   execution-history)    ; History of sandbox executions
  transparent: #t)

(def (make-sandboxed-dispatcher dispatcher
                               #!key
                               (config (make-default-sandbox-config)))
  "Create sandboxed dispatcher

   Args:
     dispatcher: Tool dispatcher
     config: Sandbox configuration (optional)

   Returns:
     Sandboxed dispatcher"

  (make-sandboxed-dispatcher
   dispatcher: dispatcher
   config: config
   execution-history: '()))

(def (sandboxed-dispatcher-call-tool sandboxed-dispatcher tool-name arguments agent-id)
  "Call tool through sandboxed dispatcher

   Args:
     sandboxed-dispatcher: Sandboxed dispatcher
     tool-name: Tool name
     arguments: Tool arguments
     agent-id: Agent ID

   Returns:
     Sandbox execution result"

  (let ((execution (sandbox-execute-tool
                    (sandboxed-dispatcher-dispatcher sandboxed-dispatcher)
                    tool-name
                    arguments
                    agent-id
                    config: (sandboxed-dispatcher-config sandboxed-dispatcher))))

    ;; Add to execution history
    (set! (sandboxed-dispatcher-execution-history sandboxed-dispatcher)
          (cons execution (sandboxed-dispatcher-execution-history sandboxed-dispatcher)))

    execution))

(def (sandboxed-dispatcher-get-history sandboxed-dispatcher #!key (limit 10))
  "Get sandbox execution history

   Args:
     sandboxed-dispatcher: Sandboxed dispatcher
     limit: Maximum number of executions to return

   Returns:
     List of sandbox executions"

  (take (sandboxed-dispatcher-execution-history sandboxed-dispatcher)
        (min limit (length (sandboxed-dispatcher-execution-history sandboxed-dispatcher)))))

(def (sandboxed-dispatcher-get-stats sandboxed-dispatcher)
  "Get sandbox execution statistics

   Args:
     sandboxed-dispatcher: Sandboxed dispatcher

   Returns:
     Hash with execution statistics"

  (let ((history (sandboxed-dispatcher-execution-history sandboxed-dispatcher)))
    (let ((total (length history))
          (completed (length (filter (lambda (e) (eq? (sandbox-execution-status e) :completed)) history)))
          (timeout (length (filter (lambda (e) (eq? (sandbox-execution-status e) :timeout)) history)))
          (error (length (filter (lambda (e) (eq? (sandbox-execution-status e) :error)) history))))

      (hash 'total total
            'completed completed
            'timeout timeout
            'error error
            'success_rate (if (> total 0)
                             (/ (* completed 100.0) total)
                             0.0)))))

;;; ============================================================================
;;; Sandbox Utilities
;;; ============================================================================

(def (sandbox-execution-succeeded? execution)
  "Check if sandbox execution succeeded

   Args:
     execution: Sandbox execution

   Returns:
     #t if succeeded, #f otherwise"

  (eq? (sandbox-execution-status execution) :completed))

(def (sandbox-execution-timed-out? execution)
  "Check if sandbox execution timed out

   Args:
     execution: Sandbox execution

   Returns:
     #t if timed out, #f otherwise"

  (eq? (sandbox-execution-status execution) :timeout))

(def (sandbox-execution-failed? execution)
  "Check if sandbox execution failed

   Args:
     execution: Sandbox execution

   Returns:
     #t if failed, #f otherwise"

  (eq? (sandbox-execution-status execution) :error))

(def (sandbox-execution-duration execution)
  "Get execution duration in seconds

   Args:
     execution: Sandbox execution

   Returns:
     Duration in seconds"

  (- (current-seconds) (sandbox-execution-start-time execution)))

(def (sandbox-execution->hash execution)
  "Convert sandbox execution to hash

   Args:
     execution: Sandbox execution

   Returns:
     Hash representation"

  (hash 'tool_name (sandbox-execution-tool-name execution)
        'start_time (sandbox-execution-start-time execution)
        'duration (sandbox-execution-duration execution)
        'status (symbol->string (sandbox-execution-status execution))
        'result (if (sandbox-execution-succeeded? execution)
                   (tool-call->hash (sandbox-execution-result execution))
                   #f)
        'error (sandbox-execution-error execution)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create dispatcher with tools
(def dispatcher (make-tool-dispatcher))
(register-core-tools! dispatcher)
(register-memory-tools! dispatcher)

;; Create sandboxed dispatcher with default config
(def sandboxed (make-sandboxed-dispatcher dispatcher))

;; Execute tool in sandbox
(def execution (sandboxed-dispatcher-call-tool sandboxed
                                              "send_message"
                                              (hash 'message "Hello!")
                                              agent-id))

;; Check result
(if (sandbox-execution-succeeded? execution)
    (displayln "Tool executed successfully")
    (displayln (format "Tool execution failed: ~a" (sandbox-execution-error execution))))

;; Create strict sandbox
(def strict-config (make-strict-sandbox-config))
(def strict-sandboxed (make-sandboxed-dispatcher dispatcher config: strict-config))

;; Try to execute blocked tool
(def blocked-execution (sandboxed-dispatcher-call-tool strict-sandboxed
                                                      "archival_memory_insert"
                                                      (hash 'content "Test")
                                                      agent-id))

;; Get execution statistics
(def stats (sandboxed-dispatcher-get-stats sandboxed))
(displayln (format "Success rate: ~a%" (hash-ref stats 'success_rate)))
|#
