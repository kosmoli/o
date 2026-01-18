;;; agent/tools.ss - Tool Framework and Registry
;;;
;;; This module implements the tool system for agents, including:
;;; - Tool definition and registration
;;; - Tool execution with error handling
;;; - Tool parameter validation
;;; - Tool result caching
;;; - Built-in utility tools

(export #t)

(import
  :std/misc/threads
  :std/sugar
  :std/format
  :std/srfi/1
  :std/srfi/13
  :std/misc/hash
  :std/misc/uuid
  :o/agent/elixir-bridge)

;;; ============================================================================
;;; Tool Structure
;;; ============================================================================

(defstruct tool
  (id                ; Unique tool identifier
   name              ; Tool name (string)
   description       ; Tool description
   parameters        ; Parameter specification
   function          ; Tool function
   category          ; Tool category: ''builtin 'custom 'external
   async?            ; Whether tool is async
   cacheable?        ; Whether results can be cached
   timeout           ; Execution timeout (seconds)
   metadata          ; Additional metadata
   created-at)       ; Creation timestamp
  transparent: #t)

;;; ============================================================================
;;; Tool Registry Structure
;;; ============================================================================

(defstruct tool-registry
  (id                ; Registry identifier
   tools             ; Hash table of tools (name -> tool)
   categories        ; Hash table of categories (category -> list of tool names)
   cache             ; Result cache (make-hash-table)
   execution-log     ; Execution history
   metadata          ; Registry metadata
   created-at        ; Creation timestamp
   updated-at)       ; Last update timestamp
  transparent: #t)

;;; ============================================================================
;;; Tool Execution Result
;;; ============================================================================

(defstruct tool-result
  (id                ; Result identifier
   tool-name         ; Tool that was executed
   parameters        ; Parameters used
   result            ; Execution result
   success?          ; Whether execution succeeded
   error             ; Error message (if failed)
   duration          ; Execution duration (milliseconds)
   timestamp)        ; Execution timestamp
  transparent: #t)

;;; ============================================================================
;;; Parameter Specification
;;; ============================================================================

(defstruct parameter-spec
  (name              ; Parameter name
   type              ; Parameter type: ''string 'number 'boolean 'object 'array
   required?         ; Whether parameter is required
   default           ; Default value (if not required)
   description       ; Parameter description
   validator)        ; Optional validation function
  transparent: #t)

;;; ============================================================================
;;; Tool Registry Creation
;;; ============================================================================

(def (make-tool-registry-instance)
  "Create a new tool registry"
  (let ((now (time->seconds (current-time))))
    (make-tool-registry
     id: (uuid->string (make-uuid))
     tools: (make-hash-table)
     categories: (make-hash-table)
     cache: (make-hash-table)
     execution-log: '()
     metadata: (make-hash-table)
     created-at: now
     updated-at: now)))

;;; ============================================================================
;;; Tool Creation
;;; ============================================================================

(def (make-tool-instance name function
                         description: (description "")
                         parameters: (parameters '())
                         category: (category ''custom)
                         async?: (async? #f)
                         cacheable?: (cacheable? #f)
                         timeout: (timeout 30)
                         metadata: (metadata (make-hash-table)))
  "Create a new tool"
  (make-tool
   id: (uuid->string (make-uuid))
   name: name
   description: description
   parameters: parameters
   function: function
   category: category
   async?: async?
   cacheable?: cacheable?
   timeout: timeout
   metadata: metadata
   created-at: (time->seconds (current-time))))

;;; ============================================================================
;;; Tool Registration
;;; ============================================================================

(def (register-tool! registry tool)
  "Register a tool in the registry"
  (let ((tool-name (tool-name tool))
        (category (tool-category tool)))

    ;; Add to tools hash
    (hash-put! (tool-registry-tools registry) tool-name tool)

    ;; Add to category
    (let ((category-tools (hash-ref (tool-registry-categories registry) category '())))
      (hash-put! (tool-registry-categories registry)
                 category
                 (cons tool-name category-tools)))

    (set! (tool-registry-updated-at registry) (time->seconds (current-time)))

    ;; Log to WAL
    (elixir-wal-log! 'tool-register
                     (let ((ht (make-hash-table)))
  (hash-put! ht 'registry_id (tool-registry-id registry))
  (hash-put! ht 'tool_name tool-name)
  (hash-put! ht 'category (symbol->string category))
  ht))

    tool))

(def (unregister-tool! registry tool-name)
  "Unregister a tool from the registry"
  (let ((tool (hash-ref (tool-registry-tools registry) tool-name #f)))
    (when tool
      (let ((category (tool-category tool)))
        ;; Remove from tools hash
        (hash-remove! (tool-registry-tools registry) tool-name)

        ;; Remove from category
        (let ((category-tools (hash-ref (tool-registry-categories registry) category '())))
          (hash-put! (tool-registry-categories registry)
                     category
                     (filter (lambda (name) (not (equal? name tool-name)))
                             category-tools)))

        (set! (tool-registry-updated-at registry) (time->seconds (current-time)))

        ;; Log to WAL
        (elixir-wal-log! 'tool-unregister
                         (let ((ht (make-hash-table)))
  (hash-put! ht 'registry_id (tool-registry-id registry))
  (hash-put! ht 'tool_name tool-name)
  ht))))))

;;; ============================================================================
;;; Tool Lookup
;;; ============================================================================

(def (get-tool registry tool-name)
  "Get a tool by name"
  (hash-ref (tool-registry-tools registry) tool-name #f))

(def (list-tools registry (category #f))
  "List all tools, optionally filtered by category"
  (if category
      (hash-ref (tool-registry-categories registry) category '())
      (hash-keys (tool-registry-tools registry))))

(def (tool-exists? registry tool-name)
  "Check if a tool exists"
  (hash-key? (tool-registry-tools registry) tool-name))

;;; ============================================================================
;;; Parameter Validation
;;; ============================================================================

(def (validate-parameters tool params)
  "Validate parameters against tool specification"
  (let ((param-specs (tool-parameters tool)))
    (let loop ((specs param-specs))
      (if (null? specs)
          #t
          (let* ((spec (car specs))
                 (param-name (parameter-spec-name spec))
                 (required? (parameter-spec-required? spec))
                 (param-value (hash-ref params param-name #f)))

            ;; Check required parameters
            (when (and required? (not param-value))
              (error (format "Missing required parameter: ~a" param-name)))

            ;; Validate type
            (when param-value
              (unless (validate-parameter-type param-value (parameter-spec-type spec))
                (error (format "Invalid type for parameter ~a: expected ~a"
                              param-name
                              (parameter-spec-type spec)))))

            ;; Custom validator
            (when (and param-value (parameter-spec-validator spec))
              (unless ((parameter-spec-validator spec) param-value)
                (error (format "Validation failed for parameter: ~a" param-name))))

            (loop (cdr specs)))))))

(def (validate-parameter-type value type)
  "Validate parameter type"
  (case type
    ((''string) (string? value))
    ((''number) (number? value))
    ((''boolean) (boolean? value))
    ((''object) (hash-table? value))
    ((''array) (list? value))
    ((''any) #t)
    (else #f)))

;;; ============================================================================
;;; Tool Execution
;;; ============================================================================

(def (execute-tool registry tool-name params)
  "Execute a tool with given parameters"
  (let* ((tool (get-tool registry tool-name))
         (start-time (time->seconds (current-time))))

    (unless tool
      (error (format "Tool not found: ~a" tool-name)))

    ;; Check cache if tool is cacheable
    (if (and (tool-cacheable? tool)
             (has-cached-result? registry tool-name params))
        (begin
          (displayln (format "Using cached result for ~a" tool-name))
          (get-cached-result registry tool-name params))

        ;; Execute tool
        (let ((result (try-execute-tool tool params)))
          (let* ((end-time (time->seconds (current-time)))
                 (duration (* 1000 (- end-time start-time)))
                 (tool-result (make-tool-result
                               id: (uuid->string (make-uuid))
                               tool-name: tool-name
                               parameters: params
                               result: (if (tool-result? result)
                                           (tool-result-result result)
                                           result)
                               success?: (if (tool-result? result)
                                             (tool-result-success? result)
                                             #t)
                               error: (if (tool-result? result)
                                          (tool-result-error result)
                                          #f)
                               duration: duration
                               timestamp: end-time)))

            ;; Cache result if cacheable
            (when (and (tool-cacheable? tool) (tool-result-success? tool-result))
              (cache-result! registry tool-name params tool-result))

            ;; Log execution
            (log-execution! registry tool-result)

            ;; Notify Elixir
            (elixir-send "tool_executed"
                         (let ((ht (make-hash-table)))
  (hash-put! ht 'tool_name tool-name)
  (hash-put! ht 'success (tool-result-success? tool-result))
  (hash-put! ht 'duration duration)
  ht))

            tool-result)))))

(def (try-execute-tool tool params)
  "Try to execute tool with error handling"
  (try
   (begin
     ;; Validate parameters
     (validate-parameters tool params)

     ;; Execute with timeout
     (let ((result (if (tool-async? tool)
                       (execute-async-tool tool params)
                       (execute-sync-tool tool params))))
       result))

   (catch (e)
     (displayln (format "Tool execution error: ~a" e))
     (make-tool-result
      id: (uuid->string (make-uuid))
      tool-name: (tool-name tool)
      parameters: params
      result: #f
      success?: #f
      error: (error-object->string e)
      duration: 0
      timestamp: (time->seconds (current-time))))))

(def (execute-sync-tool tool params)
  "Execute synchronous tool"
  (let ((fn (tool-function tool)))
    (apply fn (hash-values params))))

(def (execute-async-tool tool params)
  "Execute asynchronous tool"
  ;; Spawn thread and wait for result
  (let* ((result-box (box #f))
         (thread (spawn
                  (lambda ()
                    (let ((result (execute-sync-tool tool params)))
                      (set-box! result-box result))))))

    ;; Wait for completion with timeout
    (thread-join! thread (tool-timeout tool) 'timeout)

    (let ((result (unbox result-box)))
      (if (eq? result #f)
          (error "Tool execution timeout")
          result))))

;;; ============================================================================
;;; Result Caching
;;; ============================================================================

(def (cache-key tool-name params)
  "Generate cache key from tool name and parameters"
  (string-append tool-name ":" (object->string params)))

(def (has-cached-result? registry tool-name params)
  "Check if cached result exists"
  (hash-key? (tool-registry-cache registry)
             (cache-key tool-name params)))

(def (get-cached-result registry tool-name params)
  "Get cached result"
  (hash-ref (tool-registry-cache registry)
            (cache-key tool-name params)
            #f))

(def (cache-result! registry tool-name params result)
  "Cache tool execution result"
  (hash-put! (tool-registry-cache registry)
             (cache-key tool-name params)
             result))

(def (clear-cache! registry (tool-name #f))
  "Clear result cache"
  (if tool-name
      ;; Clear cache for specific tool
      (let ((keys-to-remove '()))
        (hash-for-each
         (lambda (key value)
           (when (string-prefix? (string-append tool-name ":") key)
             (set! keys-to-remove (cons key keys-to-remove))))
         (tool-registry-cache registry))
        (for-each
         (lambda (key)
           (hash-remove! (tool-registry-cache registry) key))
         keys-to-remove))
      ;; Clear entire cache
      (set! (tool-registry-cache registry) (make-hash-table))))

;;; ============================================================================
;;; Execution Logging
;;; ============================================================================

(def (log-execution! registry result)
  "Log tool execution"
  (let ((log (tool-registry-execution-log registry)))
    ;; Keep last 1000 executions
    (set! (tool-registry-execution-log registry)
          (take (cons result log) 1000))))

(def (get-execution-log registry (limit 100))
  "Get execution log"
  (take (tool-registry-execution-log registry) limit))

(def (get-tool-stats registry tool-name)
  "Get statistics for a specific tool"
  (let* ((log (tool-registry-execution-log registry))
         (tool-executions (filter
                           (lambda (result)
                             (equal? (tool-result-tool-name result) tool-name))
                           log))
         (total (length tool-executions))
         (successful (length (filter tool-result-success? tool-executions)))
         (failed (- total successful))
         (avg-duration (if (> total 0)
                           (/ (apply + (map tool-result-duration tool-executions))
                              total)
                           0)))

    (hash
     ('tool_name tool-name)
     ('total_executions total)
     ('successful successful)
     ('failed failed)
     ('success_rate (if (> total 0) (/ successful total) 0))
     ('avg_duration_ms avg-duration))))

;;; ============================================================================
;;; Built-in Tools
;;; ============================================================================

(def (register-builtin-tools! registry)
  "Register built-in utility tools"

  ;; Echo tool
  (register-tool! registry
                  (make-tool-instance
                   "echo"
                   (lambda (message) message)
                   description: "Echo back the input message"
                   parameters: (list
                                (make-parameter-spec
                                 name: 'message
                                 type: ''string
                                 required?: #t
                                 description: "Message to echo"))
                   category: ''builtin
                   cacheable?: #t))

  ;; Sleep tool
  (register-tool! registry
                  (make-tool-instance
                   "sleep"
                   (lambda (seconds)
                     (thread-sleep! seconds)
                     (format "Slept for ~a seconds" seconds))
                   description: "Sleep for specified seconds"
                   parameters: (list
                                (make-parameter-spec
                                 name: 'seconds
                                 type: ''number
                                 required?: #t
                                 description: "Number of seconds to sleep"))
                   category: ''builtin))

  ;; Current time tool
  (register-tool! registry
                  (make-tool-instance
                   "current_time"
                   (lambda ()
                     (time->seconds (current-time)))
                   description: "Get current Unix timestamp"
                   parameters: '()
                   category: ''builtin
                   cacheable?: #f))

  ;; UUID generator tool
  (register-tool! registry
                  (make-tool-instance
                   "generate_uuid"
                   (lambda ()
                     (uuid->string (make-uuid)))
                   description: "Generate a new UUID"
                   parameters: '()
                   category: ''builtin
                   cacheable?: #f))

  ;; Hash tool
  (register-tool! registry
                  (make-tool-instance
                   "hash_string"
                   (lambda (text)
                     (number->string (string-hash text)))
                   description: "Compute hash of string"
                   parameters: (list
                                (make-parameter-spec
                                 name: 'text
                                 type: ''string
                                 required?: #t
                                 description: "Text to hash"))
                   category: ''builtin
                   cacheable?: #t))

  registry)

;;; ============================================================================
;;; Tool Serialization
;;; ============================================================================

(def (serialize-tool tool)
  "Serialize tool to hash"
  (hash
   ('id (tool-id tool))
   ('name (tool-name tool))
   ('description (tool-description tool))
   ('parameters (map serialize-parameter-spec (tool-parameters tool)))
   ('category (symbol->string (tool-category tool)))
   ('async (tool-async? tool))
   ('cacheable (tool-cacheable? tool))
   ('timeout (tool-timeout tool))
   ('metadata (hash->list (tool-metadata tool)))
   ('created_at (tool-created-at tool))))

(def (serialize-parameter-spec spec)
  "Serialize parameter specification"
  (hash
   ('name (symbol->string (parameter-spec-name spec)))
   ('type (symbol->string (parameter-spec-type spec)))
   ('required (parameter-spec-required? spec))
   ('default (parameter-spec-default spec))
   ('description (parameter-spec-description spec))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (take lst n)
  "Take first n elements from list"
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(def (error-object->string e)
  "Convert error object to string"
  (with-output-to-string
    (lambda ()
      (display-exception e (current-output-port)))))

(def (object->string obj)
  "Convert object to string representation"
  (with-output-to-string
    (lambda ()
      (write obj))))

(def (string-hash str)
  "Simple string hash function"
  (let loop ((chars (string->list str))
             (make-hash-table))
    (if (null? chars)
        hash
        (loop (cdr chars)
              (+ (* hash 31) (char->integer (car chars)))))))

(def (string-prefix? prefix str)
  "Check if string starts with prefix"
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create registry
(def my-registry (make-tool-registry-instance))

;; Register built-in tools
(register-builtin-tools! my-registry)

;; Define custom tool
(def calculate-sum-tool
  (make-tool-instance
   "calculate_sum"
   (lambda (a b) (+ a b))
   description: "Calculate sum of two numbers"
   parameters: (list
                (make-parameter-spec
                 name: 'a
                 type: ''number
                 required?: #t
                 description: "First number")
                (make-parameter-spec
                 name: 'b
                 type: ''number
                 required?: #t
                 description: "Second number"))
   category: ''custom
   cacheable?: #t))

;; Register custom tool
(register-tool! my-registry calculate-sum-tool)

;; Execute tool
(def result (execute-tool my-registry "calculate_sum" (let ((ht (make-hash-table)))
  (hash-put! ht 'a 5)
  (hash-put! ht 'b 3)
  ht)))

;; Get tool stats
(def stats (get-tool-stats my-registry "calculate_sum"))
|#
