;;; tools/core-test.ss - Core Tools Tests
;;;
;;; Test suite for core tool implementations.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/test
  ../database/client
  ./types
  ./core)

;;; ============================================================================
;;; Test Setup
;;; ============================================================================

(def test-agent-id "test-agent-tools-123")

(def (setup-test-dispatcher)
  "Create test dispatcher with core tools registered"
  (let ((dispatcher (make-tool-dispatcher)))
    (register-core-tools! dispatcher)
    dispatcher))

;;; ============================================================================
;;; Tool Registry Tests
;;; ============================================================================

(def tool-registry-tests
  (test-suite "Tool Registry Tests"

    (test-case "Create empty registry"
      (def registry (make-tool-registry))
      (check (tool-registry? registry))
      (check (hash? (tool-registry-tools registry)))
      (check (hash? (tool-registry-categories registry))))

    (test-case "Register tool"
      (def registry (make-tool-registry))
      (def test-tool (make-tool-definition
                      name: "test_tool"
                      description: "Test tool"
                      parameters: (hash)
                      handler: (lambda (args ctx) (make-success-result "ok"))
                      category: :custom
                      requires-approval: #f
                      metadata: (hash)))
      (registry-register-tool! registry test-tool)
      (check (registry-has-tool? registry "test_tool")))

    (test-case "Get registered tool"
      (def registry (make-tool-registry))
      (def test-tool (make-tool-definition
                      name: "test_tool"
                      description: "Test tool"
                      parameters: (hash)
                      handler: (lambda (args ctx) (make-success-result "ok"))
                      category: :custom
                      requires-approval: #f
                      metadata: (hash)))
      (registry-register-tool! registry test-tool)
      (def retrieved (registry-get-tool registry "test_tool"))
      (check (tool-definition? retrieved))
      (check (equal? (tool-definition-name retrieved) "test_tool")))

    (test-case "List all tools"
      (def registry (make-tool-registry))
      (registry-register-tool! registry send-message-tool)
      (registry-register-tool! registry conversation-search-tool)
      (def tools (registry-list-tools registry))
      (check (= (length tools) 2))
      (check (member "send_message" tools))
      (check (member "conversation_search" tools)))

    (test-case "List tools by category"
      (def registry (make-tool-registry))
      (registry-register-tool! registry send-message-tool)
      (registry-register-tool! registry conversation-search-tool)
      (def core-tools (registry-list-tools registry category: :core))
      (check (= (length core-tools) 2)))

    (test-case "Register invalid tool fails"
      (def registry (make-tool-registry))
      (def invalid-tool (make-tool-definition
                         name: ""  ; Invalid empty name
                         description: "Test"
                         parameters: (hash)
                         handler: (lambda (args ctx) (make-success-result "ok"))
                         category: :custom
                         requires-approval: #f
                         metadata: (hash)))
      (check-exception
       (registry-register-tool! registry invalid-tool)))))

;;; ============================================================================
;;; Tool Validation Tests
;;; ============================================================================

(def tool-validation-tests
  (test-suite "Tool Validation Tests"

    (test-case "Validate tool name"
      (check (car (validate-tool-name "valid_tool")))
      (check (not (car (validate-tool-name ""))))
      (check (not (car (validate-tool-name "123invalid")))))

    (test-case "Validate tool parameters"
      (def valid-params (hash 'param1 (hash 'type :string 'required #t)))
      (check (car (validate-tool-parameters valid-params)))

      (def invalid-params (hash 'param1 (hash 'type :invalid)))
      (check (not (car (validate-tool-parameters invalid-params)))))

    (test-case "Validate tool arguments"
      (def tool-def (make-tool-definition
                     name: "test"
                     description: "Test"
                     parameters: (hash 'message (hash 'type :string 'required #t))
                     handler: (lambda (args ctx) (make-success-result "ok"))
                     category: :custom
                     requires-approval: #f
                     metadata: (hash)))

      ;; Valid arguments
      (def valid-args (hash 'message "Hello"))
      (check (car (validate-tool-arguments tool-def valid-args)))

      ;; Missing required parameter
      (def missing-args (hash))
      (check (not (car (validate-tool-arguments tool-def missing-args))))

      ;; Wrong type
      (def wrong-type-args (hash 'message 123))
      (check (not (car (validate-tool-arguments tool-def wrong-type-args)))))

    (test-case "Validate parameter types"
      (def tool-def (make-tool-definition
                     name: "test"
                     description: "Test"
                     parameters: (hash
                                  'str_param (hash 'type :string 'required #t)
                                  'num_param (hash 'type :number 'required #t)
                                  'bool_param (hash 'type :boolean 'required #t)
                                  'obj_param (hash 'type :object 'required #t)
                                  'arr_param (hash 'type :array 'required #t))
                     handler: (lambda (args ctx) (make-success-result "ok"))
                     category: :custom
                     requires-approval: #f
                     metadata: (hash)))

      (def valid-args (hash 'str_param "text"
                           'num_param 42
                           'bool_param #t
                           'obj_param (hash)
                           'arr_param '()))
      (check (car (validate-tool-arguments tool-def valid-args))))

    (test-case "Validate enum parameter"
      (def tool-def (make-tool-definition
                     name: "test"
                     description: "Test"
                     parameters: (hash 'option (hash 'type :string
                                                    'required #t
                                                    'enum '("a" "b" "c")))
                     handler: (lambda (args ctx) (make-success-result "ok"))
                     category: :custom
                     requires-approval: #f
                     metadata: (hash)))

      (check (car (validate-tool-arguments tool-def (hash 'option "a"))))
      (check (not (car (validate-tool-arguments tool-def (hash 'option "d"))))))))

;;; ============================================================================
;;; Tool Execution Tests
;;; ============================================================================

(def tool-execution-tests
  (test-suite "Tool Execution Tests"

    (test-case "Execute simple tool"
      (def registry (make-tool-registry))
      (def test-tool (make-tool-definition
                      name: "echo"
                      description: "Echo tool"
                      parameters: (hash 'text (hash 'type :string 'required #t))
                      handler: (lambda (args ctx)
                                (make-success-result (hash-ref args 'text)))
                      category: :custom
                      requires-approval: #f
                      metadata: (hash)))
      (registry-register-tool! registry test-tool)

      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    arguments: (hash 'text "hello")
                    timestamp: (current-seconds)
                    metadata: (hash)))

      (def result (execute-tool registry "echo" (hash 'text "hello") context))
      (check (tool-result-success result))
      (check (equal? (tool-result-value result) "hello")))

    (test-case "Execute non-existent tool fails"
      (def registry (make-tool-registry))
      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    arguments: (hash)
                    timestamp: (current-seconds)
                    metadata: (hash)))

      (def result (execute-tool registry "nonexistent" (hash) context))
      (check (not (tool-result-success result)))
      (check (string-contains (tool-result-error result) "not found")))

    (test-case "Execute tool with invalid arguments fails"
      (def registry (make-tool-registry))
      (registry-register-tool! registry send-message-tool)

      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    arguments: (hash)  ; Missing required 'message'
                    timestamp: (current-seconds)
                    metadata: (hash)))

      (def result (execute-tool registry "send_message" (hash) context))
      (check (not (tool-result-success result)))
      (check (string-contains (tool-result-error result) "Invalid arguments")))

    (test-case "Execute tool with handler error"
      (def registry (make-tool-registry))
      (def error-tool (make-tool-definition
                       name: "error_tool"
                       description: "Tool that throws error"
                       parameters: (hash)
                       handler: (lambda (args ctx) (error "Test error"))
                       category: :custom
                       requires-approval: #f
                       metadata: (hash)))
      (registry-register-tool! registry error-tool)

      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    arguments: (hash)
                    timestamp: (current-seconds)
                    metadata: (hash)))

      (def result (execute-tool registry "error_tool" (hash) context))
      (check (not (tool-result-success result)))
      (check (string-contains (tool-result-error result) "execution error")))))

;;; ============================================================================
;;; Send Message Tool Tests
;;; ============================================================================

(def send-message-tests
  (test-suite "Send Message Tool Tests"

    (test-case "Send message tool definition"
      (check (tool-definition? send-message-tool))
      (check (equal? (tool-definition-name send-message-tool) "send_message"))
      (check (eq? (tool-definition-category send-message-tool) :core))
      (check (not (tool-definition-requires-approval send-message-tool))))

    (test-case "Send message with valid arguments"
      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    arguments: (hash 'message "Hello, user!")
                    timestamp: (current-seconds)
                    metadata: (hash)))

      (def result (send-message-handler (hash 'message "Hello, user!") context))
      (check (tool-result-success result))
      (check (hash-key? (tool-result-value result) 'message_id))
      (check (hash-key? (tool-result-value result) 'content)))

    (test-case "Send message validates arguments"
      (def valid-args (hash 'message "Test message"))
      (check (car (validate-tool-arguments send-message-tool valid-args)))

      (def invalid-args (hash))  ; Missing required 'message'
      (check (not (car (validate-tool-arguments send-message-tool invalid-args)))))))

;;; ============================================================================
;;; Conversation Search Tool Tests
;;; ============================================================================

(def conversation-search-tests
  (test-suite "Conversation Search Tool Tests"

    (test-case "Conversation search tool definition"
      (check (tool-definition? conversation-search-tool))
      (check (equal? (tool-definition-name conversation-search-tool) "conversation_search"))
      (check (eq? (tool-definition-category conversation-search-tool) :core))
      (check (not (tool-definition-requires-approval conversation-search-tool))))

    (test-case "Search conversation with valid arguments"
      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    arguments: (hash 'query "test" 'limit 5)
                    timestamp: (current-seconds)
                    metadata: (hash)))

      (def result (conversation-search-handler (hash 'query "test" 'limit 5) context))
      (check (tool-result-success result))
      (check (hash-key? (tool-result-value result) 'results))
      (check (hash-key? (tool-result-value result) 'count)))

    (test-case "Search with default parameters"
      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    arguments: (hash 'query "test")
                    timestamp: (current-seconds)
                    metadata: (hash)))

      (def result (conversation-search-handler (hash 'query "test") context))
      (check (tool-result-success result)))

    (test-case "Search validates arguments"
      (def valid-args (hash 'query "test"))
      (check (car (validate-tool-arguments conversation-search-tool valid-args)))

      (def invalid-args (hash))  ; Missing required 'query'
      (check (not (car (validate-tool-arguments conversation-search-tool invalid-args)))))))

;;; ============================================================================
;;; Tool Dispatcher Tests
;;; ============================================================================

(def tool-dispatcher-tests
  (test-suite "Tool Dispatcher Tests"

    (test-case "Create dispatcher"
      (def dispatcher (make-tool-dispatcher))
      (check (tool-dispatcher? dispatcher))
      (check (tool-registry? (tool-dispatcher-registry dispatcher))))

    (test-case "Register tools with dispatcher"
      (def dispatcher (make-tool-dispatcher))
      (register-core-tools! dispatcher)
      (check (registry-has-tool? (tool-dispatcher-registry dispatcher) "send_message"))
      (check (registry-has-tool? (tool-dispatcher-registry dispatcher) "conversation_search")))

    (test-case "Call tool through dispatcher"
      (def dispatcher (setup-test-dispatcher))
      (def call (dispatcher-call-tool dispatcher
                                     "send_message"
                                     (hash 'message "Test message")
                                     test-agent-id))
      (check (tool-call? call))
      (check (tool-call-completed? call))
      (check (hash? (tool-call-result call))))

    (test-case "Call non-existent tool fails"
      (def dispatcher (setup-test-dispatcher))
      (def call (dispatcher-call-tool dispatcher
                                     "nonexistent"
                                     (hash)
                                     test-agent-id))
      (check (tool-call-failed? call))
      (check (string? (tool-call-error call))))

    (test-case "Get call history"
      (def dispatcher (setup-test-dispatcher))
      (dispatcher-call-tool dispatcher "send_message" (hash 'message "Test 1") test-agent-id)
      (dispatcher-call-tool dispatcher "send_message" (hash 'message "Test 2") test-agent-id)
      (def history (dispatcher-get-history dispatcher limit: 10))
      (check (>= (length history) 2)))

    (test-case "Get call by ID"
      (def dispatcher (setup-test-dispatcher))
      (def call (dispatcher-call-tool dispatcher
                                     "send_message"
                                     (hash 'message "Test")
                                     test-agent-id))
      (def retrieved (dispatcher-get-call dispatcher (tool-call-id call)))
      (check (tool-call? retrieved))
      (check (equal? (tool-call-id retrieved) (tool-call-id call))))

    (test-case "Tool requiring approval goes to queue"
      (def dispatcher (make-tool-dispatcher))
      (def approval-tool (make-tool-definition
                          name: "approval_tool"
                          description: "Tool requiring approval"
                          parameters: (hash)
                          handler: (lambda (args ctx) (make-success-result "ok"))
                          category: :custom
                          requires-approval: #t
                          metadata: (hash)))
      (dispatcher-register-tool! dispatcher approval-tool)

      (def call (dispatcher-call-tool dispatcher "approval_tool" (hash) test-agent-id))
      (check (tool-call-pending? call))
      (def pending (dispatcher-get-pending dispatcher))
      (check (= (length pending) 1)))

    (test-case "Approve pending call"
      (def dispatcher (make-tool-dispatcher))
      (def approval-tool (make-tool-definition
                          name: "approval_tool"
                          description: "Tool requiring approval"
                          parameters: (hash)
                          handler: (lambda (args ctx) (make-success-result "approved"))
                          category: :custom
                          requires-approval: #t
                          metadata: (hash)))
      (dispatcher-register-tool! dispatcher approval-tool)

      (def call (dispatcher-call-tool dispatcher "approval_tool" (hash) test-agent-id))
      (def approved (dispatcher-approve-call! dispatcher (tool-call-id call)))
      (check (tool-call-completed? approved))
      (check (equal? (tool-call-result approved) "approved")))

    (test-case "Reject pending call"
      (def dispatcher (make-tool-dispatcher))
      (def approval-tool (make-tool-definition
                          name: "approval_tool"
                          description: "Tool requiring approval"
                          parameters: (hash)
                          handler: (lambda (args ctx) (make-success-result "ok"))
                          category: :custom
                          requires-approval: #t
                          metadata: (hash)))
      (dispatcher-register-tool! dispatcher approval-tool)

      (def call (dispatcher-call-tool dispatcher "approval_tool" (hash) test-agent-id))
      (def rejected (dispatcher-reject-call! dispatcher (tool-call-id call) "User rejected"))
      (check (eq? (tool-call-status rejected) :rejected))
      (check (string-contains (tool-call-error rejected) "User rejected")))))

;;; ============================================================================
;;; Tool Result Tests
;;; ============================================================================

(def tool-result-tests
  (test-suite "Tool Result Tests"

    (test-case "Create success result"
      (def result (make-success-result "test value"))
      (check (tool-result-success result))
      (check (equal? (tool-result-value result) "test value"))
      (check (not (tool-result-error result))))

    (test-case "Create error result"
      (def result (make-error-result "test error"))
      (check (not (tool-result-success result)))
      (check (equal? (tool-result-error result) "test error"))
      (check (not (tool-result-value result))))

    (test-case "Result with metadata"
      (def result (make-success-result "value" metadata: (hash 'key "value")))
      (check (hash-key? (tool-result-metadata result) 'key)))))

;;; ============================================================================
;;; Conversion Tests
;;; ============================================================================

(def conversion-tests
  (test-suite "Conversion Tests"

    (test-case "Tool definition to hash"
      (def hash-repr (tool-definition->hash send-message-tool))
      (check (hash? hash-repr))
      (check (hash-key? hash-repr 'name))
      (check (hash-key? hash-repr 'description))
      (check (hash-key? hash-repr 'parameters)))

    (test-case "Tool call to hash"
      (def call (make-tool-call
                 id: "test-id"
                 tool-name: "test_tool"
                 arguments: (hash 'arg "value")
                 timestamp: 123456
                 agent-id: test-agent-id
                 status: :completed
                 result: "result"
                 error: #f))
      (def hash-repr (tool-call->hash call))
      (check (hash? hash-repr))
      (check (equal? (hash-ref hash-repr 'id) "test-id"))
      (check (equal? (hash-ref hash-repr 'status) "completed")))

    (test-case "Hash to tool call"
      (def hash-repr (hash 'id "test-id"
                          'tool_name "test_tool"
                          'arguments (hash)
                          'timestamp 123456
                          'agent_id test-agent-id
                          'status "pending"
                          'result #f
                          'error #f))
      (def call (hash->tool-call hash-repr))
      (check (tool-call? call))
      (check (equal? (tool-call-id call) "test-id"))
      (check (eq? (tool-call-status call) :pending)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(def core-tools-test-suite
  (test-suite "Core Tools Test Suite"
    tool-registry-tests
    tool-validation-tests
    tool-execution-tests
    send-message-tests
    conversation-search-tests
    tool-dispatcher-tests
    tool-result-tests
    conversion-tests))

;;; Run tests
#|
(import :std/test)
(test-run! core-tools-test-suite)
|#
