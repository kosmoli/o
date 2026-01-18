;;; tools/rules-test.ss - Tool Rules and Approval Workflow Tests
;;;
;;; Test suite for rule-based tool execution and approval workflow.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/test
  :o/tools/types
  :o/tools/core
  :o/tools/memory
  :o/tools/rules)

;;; ============================================================================
;;; Test Setup
;;; ============================================================================

(def test-agent-id "test-agent-rules-123")
(def test-reviewer-id "reviewer-456")

(def (setup-test-dispatcher)
  "Create test dispatcher with all tools registered"
  (let ((dispatcher (make-tool-dispatcher)))
    (register-core-tools! dispatcher)
    (register-memory-tools! dispatcher)
    dispatcher))

;;; ============================================================================
;;; Tool Rule Tests
;;; ============================================================================

(def tool-rule-tests
  (test-suite "Tool Rule Tests"

    (test-case "Create tool rule"
      (def rule (make-tool-rule
                 id: "test-rule"
                 name: "Test Rule"
                 description: "Test rule description"
                 condition: (lambda (tn args ctx) #t)
                 action: 'allow
                 priority: 50
                 enabled: #t
                 metadata: (make-hash-table)))
      (check (tool-rule? rule))
      (check (equal? (tool-rule-id rule) "test-rule"))
      (check (equal? (tool-rule-name rule) "Test Rule"))
      (check (eq? (tool-rule-action rule) 'allow))
      (check (= (tool-rule-priority rule) 50))
      (check (tool-rule-enabled rule)))

    (test-case "Built-in always allow rule"
      (def rule (make-always-allow-rule))
      (check (tool-rule? rule))
      (check (eq? (tool-rule-action rule) 'allow))
      (check (tool-rule-enabled rule)))

    (test-case "Built-in always deny rule"
      (def rule (make-always-deny-rule))
      (check (tool-rule? rule))
      (check (eq? (tool-rule-action rule) 'deny))
      (check (not (tool-rule-enabled rule))))

    (test-case "Built-in require approval rule"
      (def rule (make-require-approval-for-tool-rule "send_message"))
      (check (tool-rule? rule))
      (check (eq? (tool-rule-action rule) 'require-approval))
      (check (tool-rule-enabled rule)))

    (test-case "Built-in deny tool rule"
      (def rule (make-deny-tool-rule "dangerous_tool"))
      (check (tool-rule? rule))
      (check (eq? (tool-rule-action rule) 'deny))
      (check (tool-rule-enabled rule)))

    (test-case "Tool rule to hash"
      (def rule (make-always-allow-rule))
      (def hash-repr (tool-rule->hash rule))
      (check (hash-table? hash-repr))
      (check (hash-key? hash-repr 'id))
      (check (hash-key? hash-repr 'name))
      (check (hash-key? hash-repr 'action)))))

;;; ============================================================================
;;; Rule Engine Tests
;;; ============================================================================

(def rule-engine-tests
  (test-suite "Rule Engine Tests"

    (test-case "Create rule engine"
      (def engine (make-rule-engine))
      (check (rule-engine? engine))
      (check (null? (rule-engine-rules engine)))
      (check (null? (rule-engine-evaluation-history engine))))

    (test-case "Add rule to engine"
      (def engine (make-rule-engine))
      (def rule (make-always-allow-rule))
      (rule-engine-add-rule! engine rule)
      (check (= (length (rule-engine-rules engine)) 1)))

    (test-case "Add multiple rules - sorted by priority"
      (def engine (make-rule-engine))
      (def rule1 (make-tool-rule
                  id: "rule1"
                  name: "Rule 1"
                  description: "Low priority"
                  condition: (lambda (tn args ctx) #t)
                  action: 'allow
                  priority: 10
                  enabled: #t
                  metadata: (make-hash-table)))
      (def rule2 (make-tool-rule
                  id: "rule2"
                  name: "Rule 2"
                  description: "High priority"
                  condition: (lambda (tn args ctx) #t)
                  action: 'deny
                  priority: 100
                  enabled: #t
                  metadata: (make-hash-table)))
      (rule-engine-add-rule! engine rule1)
      (rule-engine-add-rule! engine rule2)
      ;; Higher priority should be first
      (check (equal? (tool-rule-id (car (rule-engine-rules engine))) "rule2")))

    (test-case "Remove rule from engine"
      (def engine (make-rule-engine))
      (def rule (make-always-allow-rule))
      (rule-engine-add-rule! engine rule)
      (check (rule-engine-remove-rule! engine "always-allow"))
      (check (null? (rule-engine-rules engine))))

    (test-case "Get rule by ID"
      (def engine (make-rule-engine))
      (def rule (make-always-allow-rule))
      (rule-engine-add-rule! engine rule)
      (def found (rule-engine-get-rule engine "always-allow"))
      (check (tool-rule? found))
      (check (equal? (tool-rule-id found) "always-allow")))

    (test-case "Evaluate rules - allow"
      (def engine (make-rule-engine))
      (def rule (make-always-allow-rule))
      (rule-engine-add-rule! engine rule)
      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    timestamp: (current-seconds)
                    metadata: (make-hash-table)))
      (def result (rule-engine-evaluate engine "send_message" (make-hash-table) context))
      (check (eq? (car result) 'allow)))

    (test-case "Evaluate rules - deny"
      (def engine (make-rule-engine))
      (def rule (make-deny-tool-rule "dangerous_tool"))
      (rule-engine-add-rule! engine rule)
      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    timestamp: (current-seconds)
                    metadata: (make-hash-table)))
      (def result (rule-engine-evaluate engine "dangerous_tool" (make-hash-table) context))
      (check (eq? (car result) 'deny)))

    (test-case "Evaluate rules - require approval"
      (def engine (make-rule-engine))
      (def rule (make-require-approval-for-tool-rule "send_message"))
      (rule-engine-add-rule! engine rule)
      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    timestamp: (current-seconds)
                    metadata: (make-hash-table)))
      (def result (rule-engine-evaluate engine "send_message" (make-hash-table) context))
      (check (eq? (car result) 'require-approval)))

    (test-case "Evaluate rules - priority order"
      (def engine (make-rule-engine))
      ;; Add low priority allow rule
      (def allow-rule (make-tool-rule
                       id: "allow"
                       name: "Allow"
                       description: "Allow"
                       condition: (lambda (tn args ctx) #t)
                       action: 'allow
                       priority: 10
                       enabled: #t
                       metadata: (make-hash-table)))
      ;; Add high priority deny rule
      (def deny-rule (make-tool-rule
                      id: "deny"
                      name: "Deny"
                      description: "Deny"
                      condition: (lambda (tn args ctx) #t)
                      action: 'deny
                      priority: 100
                      enabled: #t
                      metadata: (make-hash-table)))
      (rule-engine-add-rule! engine allow-rule)
      (rule-engine-add-rule! engine deny-rule)
      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    timestamp: (current-seconds)
                    metadata: (make-hash-table)))
      ;; Should match deny rule first (higher priority)
      (def result (rule-engine-evaluate engine "test_tool" (make-hash-table) context))
      (check (eq? (car result) 'deny)))

    (test-case "Evaluate rules - disabled rules skipped"
      (def engine (make-rule-engine))
      (def rule (make-always-deny-rule))  ; Disabled by default
      (rule-engine-add-rule! engine rule)
      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    timestamp: (current-seconds)
                    metadata: (make-hash-table)))
      ;; Should not match disabled rule - default to allow
      (def result (rule-engine-evaluate engine "test_tool" (make-hash-table) context))
      (check (eq? (car result) 'allow)))

    (test-case "Get rule evaluation history"
      (def engine (make-rule-engine))
      (def rule (make-always-allow-rule))
      (rule-engine-add-rule! engine rule)
      (def context (make-tool-execution-context
                    agent-id: test-agent-id
                    call-id: "test-call"
                    timestamp: (current-seconds)
                    metadata: (make-hash-table)))
      ;; Evaluate multiple times
      (rule-engine-evaluate engine "tool1" (make-hash-table) context)
      (rule-engine-evaluate engine "tool2" (make-hash-table) context)
      (def history (rule-engine-get-history engine limit: 10))
      (check (>= (length history) 2)))))

;;; ============================================================================
;;; Approval Manager Tests
;;; ============================================================================

(def approval-manager-tests
  (test-suite "Approval Manager Tests"

    (test-case "Create approval manager"
      (def manager (make-approval-manager))
      (check (approval-manager? manager))
      (check (null? (approval-manager-pending-queue manager))))

    (test-case "Create approval request"
      (def manager (make-approval-manager))
      (def call (make-tool-call
                 id: "test-call"
                 tool-name: "send_message"
                 arguments: (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                 timestamp: (current-seconds)
                 agent-id: test-agent-id
                 status: 'pending
                 result: #f
                 error: #f))
      (def request (approval-manager-create-request! manager call "Test reason"))
      (check (approval-request? request))
      (check (eq? (approval-request-status request) 'pending))
      (check (= (length (approval-manager-pending-queue manager)) 1)))

    (test-case "Approve request"
      (def manager (make-approval-manager))
      (def call (make-tool-call
                 id: "test-call"
                 tool-name: "send_message"
                 arguments: (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                 timestamp: (current-seconds)
                 agent-id: test-agent-id
                 status: 'pending
                 result: #f
                 error: #f))
      (def request (approval-manager-create-request! manager call "Test reason"))
      (def approved (approval-manager-approve-request! manager
                                                       (approval-request-id request)
                                                       test-reviewer-id
                                                       "Looks good"))
      (check (approval-request? approved))
      (check (eq? (approval-request-status approved) 'approved))
      (check (equal? (approval-request-reviewed-by approved) test-reviewer-id))
      (check (null? (approval-manager-pending-queue manager))))

    (test-case "Reject request"
      (def manager (make-approval-manager))
      (def call (make-tool-call
                 id: "test-call"
                 tool-name: "send_message"
                 arguments: (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                 timestamp: (current-seconds)
                 agent-id: test-agent-id
                 status: 'pending
                 result: #f
                 error: #f))
      (def request (approval-manager-create-request! manager call "Test reason"))
      (def rejected (approval-manager-reject-request! manager
                                                      (approval-request-id request)
                                                      test-reviewer-id
                                                      "Not allowed"))
      (check (approval-request? rejected))
      (check (eq? (approval-request-status rejected) 'rejected))
      (check (equal? (approval-request-review-notes rejected) "Not allowed"))
      (check (null? (approval-manager-pending-queue manager))))

    (test-case "Get pending requests"
      (def manager (make-approval-manager))
      (def call1 (make-tool-call
                  id: "call1"
                  tool-name: "tool1"
                  arguments: (make-hash-table)
                  timestamp: (current-seconds)
                  agent-id: test-agent-id
                  status: 'pending
                  result: #f
                  error: #f))
      (def call2 (make-tool-call
                  id: "call2"
                  tool-name: "tool2"
                  arguments: (make-hash-table)
                  timestamp: (current-seconds)
                  agent-id: test-agent-id
                  status: 'pending
                  result: #f
                  error: #f))
      (approval-manager-create-request! manager call1 "Reason 1")
      (approval-manager-create-request! manager call2 "Reason 2")
      (def pending (approval-manager-get-pending manager))
      (check (= (length pending) 2)))

    (test-case "Get request by ID"
      (def manager (make-approval-manager))
      (def call (make-tool-call
                 id: "test-call"
                 tool-name: "send_message"
                 arguments: (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                 timestamp: (current-seconds)
                 agent-id: test-agent-id
                 status: 'pending
                 result: #f
                 error: #f))
      (def request (approval-manager-create-request! manager call "Test reason"))
      (def found (approval-manager-get-request manager (approval-request-id request)))
      (check (approval-request? found))
      (check (equal? (approval-request-id found) (approval-request-id request))))

    (test-case "Approval request to hash"
      (def manager (make-approval-manager))
      (def call (make-tool-call
                 id: "test-call"
                 tool-name: "send_message"
                 arguments: (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                 timestamp: (current-seconds)
                 agent-id: test-agent-id
                 status: 'pending
                 result: #f
                 error: #f))
      (def request (approval-manager-create-request! manager call "Test reason"))
      (def hash-repr (approval-request->hash request))
      (check (hash-table? hash-repr))
      (check (hash-key? hash-repr 'id))
      (check (hash-key? hash-repr 'status))
      (check (hash-key? hash-repr 'reason)))))

;;; ============================================================================
;;; Rule-Based Dispatcher Tests
;;; ============================================================================

(def rule-based-dispatcher-tests
  (test-suite "Rule-Based Dispatcher Tests"

    (test-case "Create rule-based dispatcher"
      (def dispatcher (setup-test-dispatcher))
      (def rbd (make-rule-based-dispatcher dispatcher))
      (check (rule-based-dispatcher? rbd))
      (check (rule-engine? (rule-based-dispatcher-rule-engine rbd)))
      (check (approval-manager? (rule-based-dispatcher-approval-manager rbd))))

    (test-case "Call tool - allow by default"
      (def dispatcher (setup-test-dispatcher))
      (def rbd (make-rule-based-dispatcher dispatcher))
      (def result (rule-based-dispatcher-call-tool rbd
                                                   "send_message"
                                                   (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                                                   test-agent-id))
      (check (tool-call? result))
      (check (tool-call-completed? result)))

    (test-case "Call tool - deny by rule"
      (def dispatcher (setup-test-dispatcher))
      (def rbd (make-rule-based-dispatcher dispatcher))
      ;; Add deny rule
      (rule-engine-add-rule! (rule-based-dispatcher-rule-engine rbd)
                            (make-deny-tool-rule "send_message"))
      (def result (rule-based-dispatcher-call-tool rbd
                                                   "send_message"
                                                   (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                                                   test-agent-id))
      (check (tool-call? result))
      (check (tool-call-failed? result)))

    (test-case "Call tool - require approval"
      (def dispatcher (setup-test-dispatcher))
      (def rbd (make-rule-based-dispatcher dispatcher))
      ;; Add approval rule
      (rule-engine-add-rule! (rule-based-dispatcher-rule-engine rbd)
                            (make-require-approval-for-tool-rule "send_message"))
      (def result (rule-based-dispatcher-call-tool rbd
                                                   "send_message"
                                                   (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                                                   test-agent-id))
      (check (approval-request? result))
      (check (eq? (approval-request-status result) 'pending)))

    (test-case "Approve and execute tool call"
      (def dispatcher (setup-test-dispatcher))
      (def rbd (make-rule-based-dispatcher dispatcher))
      ;; Add approval rule
      (rule-engine-add-rule! (rule-based-dispatcher-rule-engine rbd)
                            (make-require-approval-for-tool-rule "send_message"))
      ;; Call tool - should require approval
      (def request (rule-based-dispatcher-call-tool rbd
                                                    "send_message"
                                                    (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                                                    test-agent-id))
      (check (approval-request? request))
      ;; Approve and execute
      (def executed (rule-based-dispatcher-approve-call! rbd
                                                        (approval-request-id request)
                                                        test-reviewer-id
                                                        "Approved"))
      (check (tool-call? executed))
      (check (tool-call-completed? executed)))

    (test-case "Reject tool call"
      (def dispatcher (setup-test-dispatcher))
      (def rbd (make-rule-based-dispatcher dispatcher))
      ;; Add approval rule
      (rule-engine-add-rule! (rule-based-dispatcher-rule-engine rbd)
                            (make-require-approval-for-tool-rule "send_message"))
      ;; Call tool - should require approval
      (def request (rule-based-dispatcher-call-tool rbd
                                                    "send_message"
                                                    (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                                                    test-agent-id))
      ;; Reject
      (def rejected (rule-based-dispatcher-reject-call! rbd
                                                       (approval-request-id request)
                                                       test-reviewer-id
                                                       "Not allowed"))
      (check (approval-request? rejected))
      (check (eq? (approval-request-status rejected) 'rejected)))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(def integration-tests
  (test-suite "Integration Tests"

    (test-case "Multiple rules with different priorities"
      (def dispatcher (setup-test-dispatcher))
      (def rbd (make-rule-based-dispatcher dispatcher))
      ;; Add rules with different priorities
      (rule-engine-add-rule! (rule-based-dispatcher-rule-engine rbd)
                            (make-require-approval-for-tool-rule "send_message"))
      (rule-engine-add-rule! (rule-based-dispatcher-rule-engine rbd)
                            (make-deny-tool-rule "dangerous_tool"))
      ;; Test send_message - should require approval
      (def msg-result (rule-based-dispatcher-call-tool rbd
                                                      "send_message"
                                                      (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test")
  ht)
                                                      test-agent-id))
      (check (approval-request? msg-result))
      ;; Test dangerous_tool - should be denied
      (def danger-result (rule-based-dispatcher-call-tool rbd
                                                         "dangerous_tool"
                                                         (make-hash-table)
                                                         test-agent-id))
      (check (tool-call? danger-result))
      (check (tool-call-failed? danger-result)))

    (test-case "Approval workflow with multiple requests"
      (def dispatcher (setup-test-dispatcher))
      (def rbd (make-rule-based-dispatcher dispatcher))
      ;; Add approval rule
      (rule-engine-add-rule! (rule-based-dispatcher-rule-engine rbd)
                            (make-require-approval-for-tool-rule "send_message"))
      ;; Create multiple requests
      (def req1 (rule-based-dispatcher-call-tool rbd
                                                "send_message"
                                                (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test 1")
  ht)
                                                test-agent-id))
      (def req2 (rule-based-dispatcher-call-tool rbd
                                                "send_message"
                                                (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Test 2")
  ht)
                                                test-agent-id))
      ;; Check pending queue
      (def pending (approval-manager-get-pending (rule-based-dispatcher-approval-manager rbd)))
      (check (= (length pending) 2))
      ;; Approve first request
      (rule-based-dispatcher-approve-call! rbd
                                          (approval-request-id req1)
                                          test-reviewer-id
                                          "OK")
      ;; Check pending queue - should have 1 left
      (def pending-after (approval-manager-get-pending (rule-based-dispatcher-approval-manager rbd)))
      (check (= (length pending-after) 1)))

    (test-case "Rule evaluation history tracking"
      (def dispatcher (setup-test-dispatcher))
      (def rbd (make-rule-based-dispatcher dispatcher))
      ;; Add rules
      (rule-engine-add-rule! (rule-based-dispatcher-rule-engine rbd)
                            (make-require-approval-for-tool-rule "send_message"))
      ;; Make multiple calls
      (rule-based-dispatcher-call-tool rbd "send_message" (let ((ht (make-hash-table)))
  (hash-put! ht 'message "1")
  ht) test-agent-id)
      (rule-based-dispatcher-call-tool rbd "send_message" (let ((ht (make-hash-table)))
  (hash-put! ht 'message "2")
  ht) test-agent-id)
      (rule-based-dispatcher-call-tool rbd "conversation_search" (let ((ht (make-hash-table)))
  (hash-put! ht 'query "test")
  ht) test-agent-id)
      ;; Check evaluation history
      (def history (rule-engine-get-history (rule-based-dispatcher-rule-engine rbd) limit: 10))
      (check (>= (length history) 2)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(def rules-test-suite
  (test-suite "Rules Test Suite"
    tool-rule-tests
    rule-engine-tests
    approval-manager-tests
    rule-based-dispatcher-tests
    integration-tests))

;;; Run tests
#|
(import :std/test)
(test-run! rules-test-suite)
|#
