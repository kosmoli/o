#lang racket

;;; agent/tools.rkt - Tool Framework and Registry
;;;
;;; This module implements the tool system for agents, including:
;;; - Tool definition and registration
;;; - Tool execution with error handling
;;; - Tool parameter validation
;;; - Tool result caching
;;; - Built-in utility tools

(provide (struct-out tool-internal)
         (struct-out tool-registry-agent)
         (struct-out tool-execution-result)
         (struct-out parameter-spec)
         make-tool-registry-instance
         make-tool-instance
         make-parameter-spec
         register-tool!
         unregister-tool!
         get-tool
         list-tools
         tool-exists?
         validate-parameters
         validate-parameter-type
         execute-tool
         try-execute-tool
         execute-sync-tool
         cache-key
         has-cached-result?
         get-cached-result
         cache-result!
         clear-cache!
         log-execution!
         get-execution-log
         get-tool-stats
         register-builtin-tools!
         serialize-tool
         serialize-parameter-spec)

(require racket/hash
        racket/format
        racket/list
        racket/date
        "../elixir-bridge.rkt")

;;; ============================================================================
;;; Tool Structure
;;; ============================================================================

(struct tool-internal
  (id                ; Unique tool identifier
   name              ; Tool name (string)
   description       ; Tool description
   parameters        ; Parameter specification
   function          ; Tool function
   category          ; Tool category: 'builtin 'custom 'external
   async?            ; Whether tool is async
   cacheable?        ; Whether results can be cached
   timeout           ; Execution timeout (seconds)
   metadata          ; Additional metadata
   created-at)       ; Creation timestamp
  #:transparent)

;;; ============================================================================
;;; Tool Registry Structure
;;; ============================================================================

(struct tool-registry-agent
  (id                ; Registry identifier
   tools             ; Hash table of tools (name -> tool)
   categories        ; Hash table of categories (category -> list of tool names)
   cache             ; Result cache (hash table)
   execution-log     ; Execution history
   metadata          ; Registry metadata
   created-at        ; Creation timestamp
   updated-at)       ; Last update timestamp
  #:transparent
  #:mutable)

;;; ============================================================================
;;; Tool Execution Result
;;; ============================================================================

(struct tool-execution-result
  (id                ; Result identifier
   tool-name         ; Tool that was executed
   parameters        ; Parameters used
   result            ; Execution result
   success?          ; Whether execution succeeded
   error             ; Error message (if failed)
   duration          ; Execution duration (milliseconds)
   timestamp)        ; Execution timestamp
  #:transparent)

;;; ============================================================================
;;; Parameter Specification
;;; ============================================================================

(struct parameter-spec
  (name              ; Parameter name
   type              ; Parameter type: 'string 'number 'boolean 'object 'array
   required?         ; Whether parameter is required
   default           ; Default value (if not required)
   description       ; Parameter description
   validator)        ; Optional validation function
  #:transparent)

;;; ============================================================================
;;; Tool Registry Creation
;;; ============================================================================

(define (make-tool-registry-instance)
  "Create a new tool registry"
  (define now (current-seconds))
  (tool-registry-agent
   (uuid-generate)
   (hash)
   (hash)
   (hash)
   '()
   (hash)
   now
   now))

;;; ============================================================================
;;; Tool Creation
;;; ============================================================================

(define (make-tool-instance name function
                           #:description [description ""]
                           #:parameters [parameters '()]
                           #:category [category 'custom]
                           #:async? [async? #f]
                           #:cacheable? [cacheable? #f]
                           #:timeout [timeout 30]
                           #:metadata [metadata (hash)])
  "Create a new tool"
  (tool-internal
   (uuid-generate)
   name
   description
   parameters
   function
   category
   async?
   cacheable?
   timeout
   metadata
   (current-seconds)))

(define (make-parameter-spec name type
                             #:required? [required? #t]
                             #:default [default #f]
                             #:description [description ""]
                             #:validator [validator #f])
  "Create a parameter specification"
  (parameter-spec name type required? default description validator))

;;; ============================================================================
;;; Tool Registration
;;; ============================================================================

(define (register-tool! registry tool)
  "Register a tool in the registry"
  (define tool-name (tool-internal-name tool))
  (define category (tool-internal-category tool))

  ;; Add to tools hash
  (define tools (tool-registry-agent-tools registry))
  (set-tool-registry-agent-tools! registry (hash-set tools tool-name tool))

  ;; Add to category
  (define categories (tool-registry-agent-categories registry))
  (define category-tools (hash-ref categories category '()))
  (set-tool-registry-agent-categories! registry (hash-set categories category (cons tool-name category-tools)))

  (set-tool-registry-agent-updated-at! registry (current-seconds))

  ;; Log to WAL
  (elixir-wal-log! 'tool-register
                   (hash 'registry_id (tool-registry-agent-id registry)
                         'tool_name tool-name
                         'category (symbol->string category)))

  tool)

(define (unregister-tool! registry tool-name)
  "Unregister a tool from the registry"
  (define tool (hash-ref (tool-registry-agent-tools registry) tool-name #f))
  (when tool
    (define category (tool-internal-category tool))
    ;; Remove from tools hash
    (define tools (tool-registry-agent-tools registry))
    (set-tool-registry-agent-tools! registry (hash-remove tools tool-name))

    ;; Remove from category
    (define categories (tool-registry-agent-categories registry))
    (define category-tools (hash-ref categories category '()))
    (set-tool-registry-agent-categories! registry
                                           (hash-set categories category
                                                         (filter (lambda (name) (not (equal? name tool-name)))
                                                                 category-tools)))

    (set-tool-registry-agent-updated-at! registry (current-seconds))

    ;; Log to WAL
    (elixir-wal-log! 'tool-unregister
                     (hash 'registry_id (tool-registry-agent-id registry)
                           'tool_name tool-name))))

;;; ============================================================================
;;; Tool Lookup
;;; ============================================================================

(define (get-tool registry tool-name)
  "Get a tool by name"
  (hash-ref (tool-registry-agent-tools registry) tool-name #f))

(define (list-tools registry #:category [category #f])
  "List all tools, optionally filtered by category"
  (if category
      (hash-ref (tool-registry-agent-categories registry) category '())
      (hash-keys (tool-registry-agent-tools registry))))

(define (tool-exists? registry tool-name)
  "Check if a tool exists"
  (hash-has-key? (tool-registry-agent-tools registry) tool-name))

;;; ============================================================================
;;; Parameter Validation
;;; ============================================================================

(define (validate-parameters tool params)
  "Validate parameters against tool specification"
  (define param-specs (tool-internal-parameters tool))
  (define (loop specs)
    (cond
     [(null? specs) #t]
     [else
      (define spec (car specs))
      (define param-name (parameter-spec-name spec))
      (define required? (parameter-spec-required? spec))
      (define param-value (hash-ref params param-name #f))

      ;; Check required parameters
      (when (and required? (not param-value))
        (error 'validate-parameters (format "Missing required parameter: ~a" param-name)))

      ;; Validate type
      (when param-value
        (unless (validate-parameter-type param-value (parameter-spec-type spec))
          (error 'validate-parameters (format "Invalid type for parameter ~a: expected ~a"
                                            param-name
                                            (parameter-spec-type spec)))))

      ;; Custom validator
      (when (and param-value (parameter-spec-validator spec))
        (unless ((parameter-spec-validator spec) param-value)
          (error 'validate-parameters (format "Validation failed for parameter: ~a" param-name))))

      (loop (cdr specs))]))
  (loop param-specs))

(define (validate-parameter-type value type)
  "Validate parameter type"
  (case type
    [(string) (string? value)]
    [(number) (number? value)]
    [(boolean) (boolean? value)]
    [(object) (hash? value)]
    [(array) (list? value)]
    [(any) #t]
    [else #f]))

;;; ============================================================================
;;; Tool Execution
;;; ============================================================================

(define (execute-tool registry tool-name params)
  "Execute a tool with given parameters"
  (define tool (get-tool registry tool-name))
  (define start-time (current-seconds))

  (unless tool
    (error 'execute-tool (format "Tool not found: ~a" tool-name)))

  ;; Check cache if tool is cacheable
  (if (and (tool-internal-cacheable? tool)
           (has-cached-result? registry tool-name params))
      (begin
        (displayln (format "Using cached result for ~a" tool-name))
        (get-cached-result registry tool-name params))

      ;; Execute tool
      (let ([result (try-execute-tool tool params)])
        (define end-time (current-seconds))
        (define duration (* 1000.0 (- end-time start-time)))
        (define tool-result
          (tool-execution-result
           (uuid-generate)
           tool-name
           params
           result
           #t
           #f
           duration
           end-time))

        ;; Cache result if cacheable
        (when (tool-internal-cacheable? tool)
          (cache-result! registry tool-name params tool-result))

        ;; Log execution
        (log-execution! registry tool-result)

        tool-result)))

(define (try-execute-tool tool params)
  "Try to execute tool with error handling"
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (displayln (format "Tool execution error: ~a" (exn-message e)))
                     (error 'try-execute-tool (exn-message e)))])
    ;; Validate parameters
    (validate-parameters tool params)

    ;; Execute
    (define fn (tool-internal-function tool))
    (apply fn (hash-values params))))

(define (execute-sync-tool tool params)
  "Execute synchronous tool"
  (define fn (tool-internal-function tool))
  (apply fn (hash-values params)))

;;; ============================================================================
;;; Result Caching
;;; ============================================================================

(define (cache-key tool-name params)
  "Generate cache key from tool name and parameters"
  (string-append tool-name ":" (object->string params)))

(define (has-cached-result? registry tool-name params)
  "Check if cached result exists"
  (hash-has-key? (tool-registry-agent-cache registry)
                  (cache-key tool-name params)))

(define (get-cached-result registry tool-name params)
  "Get cached result"
  (hash-ref (tool-registry-agent-cache registry)
            (cache-key tool-name params)
            #f))

(define (cache-result! registry tool-name params result)
  "Cache tool execution result"
  (define cache (tool-registry-agent-cache registry))
  (set-tool-registry-agent-cache! registry (hash-set cache (cache-key tool-name params) result)))

(define (clear-cache! registry #:tool-name [tool-name #f])
  "Clear result cache"
  (if tool-name
      ;; Clear cache for specific tool
      (let ([keys-to-remove '()]
             [cache (tool-registry-agent-cache registry)])
        (for ([(key value) (in-hash cache)])
          (when (string-prefix? (string-append tool-name ":") key)
            (set! keys-to-remove (cons key keys-to-remove))))
        (define new-cache
          (for/fold ([acc cache])
                    ([key (in-list keys-to-remove)])
            (hash-remove acc key)))
        (set-tool-registry-agent-cache! registry new-cache))
      ;; Clear entire cache
      (set-tool-registry-agent-cache! registry (hash))))

;;; ============================================================================
;;; Execution Logging
;;; ============================================================================

(define (log-execution! registry result)
  "Log tool execution"
  (define log (tool-registry-agent-execution-log registry))
  ;; Keep last 1000 executions
  (set-tool-registry-agent-execution-log! registry (take (cons result log) 1000)))

(define (get-execution-log registry #:limit [limit 100])
  "Get execution log"
  (take (tool-registry-agent-execution-log registry) limit))

(define (get-tool-stats registry tool-name)
  "Get statistics for a specific tool"
  (define log (tool-registry-agent-execution-log registry))
  (define tool-executions
    (filter (lambda (result)
              (equal? (tool-execution-result-tool-name result) tool-name))
            log))
  (define total (length tool-executions))
  (define successful (length (filter tool-execution-result-success? tool-executions)))
  (define failed (- total successful))
  (define avg-duration
    (if (> total 0)
        (/ (apply + (map tool-execution-result-duration tool-executions)) total)
        0))

  (hash
   'tool_name tool-name
   'total_executions total
   'successful successful
   'failed failed
   'success_rate (if (> total 0) (/ successful total) 0)
   'avg_duration_ms avg-duration))

;;; ============================================================================
;;; Built-in Tools
;;; ============================================================================

(define (register-builtin-tools! registry)
  "Register built-in utility tools"

  ;; Echo tool
  (register-tool! registry
                  (make-tool-instance
                   "echo"
                   (lambda (message) message)
                   #:description "Echo back the input message"
                   #:parameters (list
                                (make-parameter-spec
                                 'message 'string
                                 #:description "Message to echo"))
                   #:category 'builtin
                   #:cacheable? #t))

  ;; Sleep tool
  (register-tool! registry
                  (make-tool-instance
                   "sleep"
                   (lambda (seconds)
                     (sleep seconds)
                     (format "Slept for ~a seconds" seconds))
                   #:description "Sleep for specified seconds"
                   #:parameters (list
                                (make-parameter-spec
                                 'seconds 'number
                                 #:description "Number of seconds to sleep"))
                   #:category 'builtin))

  ;; Current time tool
  (register-tool! registry
                  (make-tool-instance
                   "current_time"
                   (lambda () (current-seconds))
                   #:description "Get current Unix timestamp"
                   #:parameters '()
                   #:category 'builtin
                   #:cacheable? #f))

  ;; UUID generator tool
  (register-tool! registry
                  (make-tool-instance
                   "generate_uuid"
                   (lambda () (uuid-generate))
                   #:description "Generate a new UUID"
                   #:parameters '()
                   #:category 'builtin
                   #:cacheable? #f))

  ;; Hash tool
  (register-tool! registry
                  (make-tool-instance
                   "hash_string"
                   (lambda (text) (string-hash text))
                   #:description "Compute hash of string"
                   #:parameters (list
                                (make-parameter-spec
                                 'text 'string
                                 #:description "Text to hash"))
                   #:category 'builtin
                   #:cacheable? #t))

  registry)

;;; ============================================================================
;;; Tool Serialization
;;; ============================================================================

(define (serialize-tool tool)
  "Serialize tool to hash"
  (hash
   'id (tool-internal-id tool)
   'name (tool-internal-name tool)
   'description (tool-internal-description tool)
   'parameters (map serialize-parameter-spec (tool-internal-parameters tool))
   'category (symbol->string (tool-internal-category tool))
   'async (tool-internal-async? tool)
   'cacheable (tool-internal-cacheable? tool)
   'timeout (tool-internal-timeout tool)
   'metadata (hash->list (tool-internal-metadata tool))
   'created_at (tool-internal-created-at tool)))

(define (serialize-parameter-spec spec)
  "Serialize parameter specification"
  (hash
   'name (symbol->string (parameter-spec-name spec))
   'type (symbol->string (parameter-spec-type spec))
   'required (parameter-spec-required? spec)
   'default (parameter-spec-default spec)
   'description (parameter-spec-description spec)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (uuid-generate)
  "Generate UUID (simple implementation)"
  (format "~a-~a" (current-seconds) (random 1000000)))

(define (object->string obj)
  "Convert object to string representation"
  (format "~a" obj))

(define (string-hash str)
  "Simple string hash function"
  (define chars (string->list str))
  (define (loop chars hash)
    (if (null? chars)
        hash
        (loop (cdr chars) (+ (* hash 31) (char->integer (car chars))))))
  (loop chars 0))

(define (string-prefix? prefix str)
  "Check if string starts with prefix"
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))
