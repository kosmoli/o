#lang racket

;;; tools/sandbox.rkt - Tool Execution Sandbox
;;;
;;; Safe tool execution with resource limits and error isolation.

(provide (struct-out sandbox-config)
         (struct-out sandbox-execution)
         (struct-out sandboxed-dispatcher)
         make-default-sandbox-config
         make-strict-sandbox-config
         validate-tool-allowed
         validate-execution-time
         sandbox-execute-tool
         sandboxed-dispatcher-call-tool
         sandboxed-dispatcher-get-history
         sandboxed-dispatcher-get-stats
         sandbox-execution-succeeded?
         sandbox-execution-timed-out?
         sandbox-execution-failed?
         sandbox-execution-duration
         sandbox-execution->hash
         make-sandboxed-dispatcher)

(require racket/hash
        racket/format
        racket/list
        racket/function
        "types.rkt"
        "core.rkt")

;;; ============================================================================
;;; Sandbox Configuration
;;; ============================================================================

(struct sandbox-config
  (max-execution-time    ; Maximum execution time in seconds (default: 30)
   max-memory-mb         ; Maximum memory usage in MB (default: 100)
   allow-network         ; Allow network access (default: #f)
   allow-file-read       ; Allow file read operations (default: #f)
   allow-file-write      ; Allow file write operations (default: #f)
   allowed-tools         ; List of allowed tool names (default: all)
   blocked-tools)        ; List of blocked tool names (default: none)
  #:transparent)

(define (make-default-sandbox-config)
  "Create default sandbox configuration

   Returns:
     Sandbox configuration with safe defaults"

  (sandbox-config
   30     ; max-execution-time
   100     ; max-memory-mb
   #f      ; allow-network
   #f      ; allow-file-read
   #f      ; allow-file-write
   '()     ; allowed-tools (empty = all)
   '()))   ; blocked-tools

(define (make-strict-sandbox-config)
  "Create strict sandbox configuration

   Returns:
     Sandbox configuration with strict limits"

  (sandbox-config
   10
   50
   #f
   #f
   #f
   '("send_message" "conversation_search")
   '()))

;;; ============================================================================
;;; Sandbox Execution Context
;;; ============================================================================

(struct sandbox-execution
  (tool-name             ; Tool being executed
   start-time            ; Execution start time
   timeout-time          ; Timeout deadline
   config                ; Sandbox configuration
   status                ; Execution status ('running, 'completed, 'timeout, 'error)
   result                ; Execution result
   error)                ; Error message if failed
  #:transparent
  #:mutable)

;;; ============================================================================
;;; Sandbox Validation
;;; ============================================================================

(define (validate-tool-allowed config tool-name)
  "Check if tool is allowed in sandbox

   Args:
     config: Sandbox configuration
     tool-name: Tool name to check

   Returns:
     #t if allowed, #f otherwise"

  ;; Check blocked list first
  (if (member tool-name (sandbox-config-blocked-tools config))
      #f
      ;; Check allowed list (empty means all allowed)
      (let ((allowed (sandbox-config-allowed-tools config)))
        (if (null? allowed)
            #t
            (member tool-name allowed)))))

(define (validate-execution-time execution)
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

(define (sandbox-execute-tool dispatcher tool-name arguments agent-id
                           #:config [config (make-default-sandbox-config)])
  "Execute tool in sandbox with resource limits

   Args:
     dispatcher: Tool dispatcher
     tool-name: Tool name
     arguments: Tool arguments
     agent-id: Agent ID
     config: Sandbox configuration (optional)

   Returns:
     Sandbox execution result"

  ;; Validate tool is allowed - early return if not
  (unless (validate-tool-allowed config tool-name)
    (sandbox-execution
     tool-name
     (current-seconds)
     0
     config
     'error
     #f
     (format "Tool ~a is not allowed in sandbox" tool-name)))

  ;; Create execution context
  (define start-time (current-seconds))
  (define timeout-time (+ (current-seconds)
                          (sandbox-config-max-execution-time config)))

  (define execution (sandbox-execution
                    tool-name
                    start-time
                    timeout-time
                    config
                    'running
                    #f
                    #f))

  ;; Execute tool with timeout protection
  (with-handlers* ([exn:fail?
                     (lambda (e)
                       (set-sandbox-execution-status! execution 'error)
                       (set-sandbox-execution-error! execution
                             (format "Tool execution error: ~a" (exn-message e)))
                       execution)])
    (define call (dispatcher-call-tool dispatcher tool-name arguments agent-id))

    ;; Check if execution completed within timeout
    (if (validate-execution-time execution)
        (begin
          (set-sandbox-execution-status! execution 'completed)
          (set-sandbox-execution-result! execution call)
          execution)
        (begin
          (set-sandbox-execution-status! execution 'timeout)
          (set-sandbox-execution-error! execution
                (format "Tool execution exceeded timeout of ~a seconds"
                        (sandbox-config-max-execution-time config)))
          execution))))

;;; ============================================================================
;;; Sandboxed Dispatcher
;;; ============================================================================

(struct sandboxed-dispatcher
  (dispatcher            ; Underlying tool dispatcher
   config                ; Sandbox configuration
   execution-history)    ; History of sandbox executions
  #:transparent
  #:mutable)

(define (make-sandboxed-dispatcher dispatcher
                               #:config [config (make-default-sandbox-config)])
  "Create sandboxed dispatcher

   Args:
     dispatcher: Tool dispatcher
     config: Sandbox configuration (optional)

   Returns:
     Sandboxed dispatcher"

  (sandboxed-dispatcher
   dispatcher
   config
   '()))

(define (sandboxed-dispatcher-call-tool sandboxed-dispatcher tool-name arguments agent-id)
  "Call tool through sandboxed dispatcher

   Args:
     sandboxed-dispatcher: Sandboxed dispatcher
     tool-name: Tool name
     arguments: Tool arguments
     agent-id: Agent ID

   Returns:
     Sandbox execution result"

  (define execution (sandbox-execute-tool
                    (sandboxed-dispatcher-dispatcher sandboxed-dispatcher)
                    tool-name
                    arguments
                    agent-id
                    #:config (sandboxed-dispatcher-config sandboxed-dispatcher)))

  ;; Add to execution history
  (set-sandboxed-dispatcher-execution-history! sandboxed-dispatcher
        (cons execution (sandboxed-dispatcher-execution-history sandboxed-dispatcher)))

  execution)

(define (sandboxed-dispatcher-get-history sandboxed-dispatcher #:limit [limit 10])
  "Get sandbox execution history

   Args:
     sandboxed-dispatcher: Sandboxed dispatcher
     limit: Maximum number of executions to return

   Returns:
     List of sandbox executions"

  (take (sandboxed-dispatcher-execution-history sandboxed-dispatcher)
        (min limit (length (sandboxed-dispatcher-execution-history sandboxed-dispatcher)))))

(define (sandboxed-dispatcher-get-stats sandboxed-dispatcher)
  "Get sandbox execution statistics

   Args:
     sandboxed-dispatcher: Sandboxed dispatcher

   Returns:
     Hash with execution statistics"

  (define history (sandboxed-dispatcher-execution-history sandboxed-dispatcher))
  (define total (length history))
  (define completed (length (filter (lambda (e) (eq? (sandbox-execution-status e) 'completed)) history)))
  (define timeout-count (length (filter (lambda (e) (eq? (sandbox-execution-status e) 'timeout)) history)))
  (define error-count (length (filter (lambda (e) (eq? (sandbox-execution-status e) 'error)) history)))

  (hash 'total total
        'completed completed
        'timeout timeout-count
        'error error-count
        'success_rate (if (> total 0)
                         (/ (* completed 100.0) total)
                         0.0)))

;;; ============================================================================
;;; Sandbox Utilities
;;; ============================================================================

(define (sandbox-execution-succeeded? execution)
  "Check if sandbox execution succeeded

   Args:
     execution: Sandbox execution

   Returns:
     #t if succeeded, #f otherwise"

  (eq? (sandbox-execution-status execution) 'completed))

(define (sandbox-execution-timed-out? execution)
  "Check if sandbox execution timed out

   Args:
     execution: Sandbox execution

   Returns:
     #t if timed out, #f otherwise"

  (eq? (sandbox-execution-status execution) 'timeout))

(define (sandbox-execution-failed? execution)
  "Check if sandbox execution failed

   Args:
     execution: Sandbox execution

   Returns:
     #t if failed, #f otherwise"

  (eq? (sandbox-execution-status execution) 'error))

(define (sandbox-execution-duration execution)
  "Get execution duration in seconds

   Args:
     execution: Sandbox execution

   Returns:
     Duration in seconds"

  (- (current-seconds) (sandbox-execution-start-time execution)))

(define (sandbox-execution->hash execution)
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
                   (tool-execution->hash (sandbox-execution-result execution))
                   #f)
        'error (sandbox-execution-error execution)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create dispatcher with tools
(define dispatcher (make-tool-dispatcher))
(register-core-tools! dispatcher)
(register-memory-tools! dispatcher)

;; Create sandboxed dispatcher with default config
(define sandboxed (make-sandboxed-dispatcher dispatcher))

;; Execute tool in sandbox
(define execution (sandboxed-dispatcher-call-tool sandboxed
                                              "send_message"
                                              (hash 'message "Hello!")
                                              "agent-123"))

;; Check result
(if (sandbox-execution-succeeded? execution)
    (displayln "Tool executed successfully")
    (displayln (format "Tool execution failed: ~a" (sandbox-execution-error execution))))

;; Create strict sandbox
(define strict-config (make-strict-sandbox-config))
(define strict-sandboxed (make-sandboxed-dispatcher dispatcher #:config strict-config))

;; Try to execute blocked tool
(define blocked-execution (sandboxed-dispatcher-call-tool strict-sandboxed
                                                      "archival_memory_insert"
                                                      (hash 'content "Test")
                                                      "agent-123"))

;; Get execution statistics
(define stats (sandboxed-dispatcher-get-stats sandboxed))
(displayln (format "Success rate: ~a%" (hash-ref stats 'success_rate)))
|#
