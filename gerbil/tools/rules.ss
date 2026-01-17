;;; tools/rules.ss - Tool Rules and Approval Workflow
;;;
;;; Rule-based tool execution control and enhanced approval workflow.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/error
  :o/tools/types
  :o/tools/core)

;;; ============================================================================
;;; Tool Rule Structures
;;; ============================================================================

(defstruct tool-rule
  (id                    ; Unique rule ID (string)
   name                  ; Rule name (string)
   description           ; Rule description (string)
   condition             ; Condition function (arguments, context) -> boolean
   action                ; Action to take (:allow, :deny, :require-approval)
   priority              ; Rule priority (higher = evaluated first)
   enabled               ; Is rule enabled? (boolean)
   metadata)             ; Additional metadata (hash)
  transparent: #t)

(defstruct rule-evaluation
  (rule-id               ; Rule ID that was evaluated
   matched               ; Did condition match? (boolean)
   action                ; Action taken (symbol)
   reason                ; Reason for action (string)
   timestamp)            ; Evaluation timestamp
  transparent: #t)

(defstruct approval-request
  (id                    ; Unique request ID (string)
   tool-call             ; Tool call awaiting approval
   requested-at          ; Request timestamp
   requested-by          ; Agent ID that requested
   reason                ; Reason for approval requirement
   status                ; Request status (:pending, :approved, :rejected)
   reviewed-at           ; Review timestamp (optional)
   reviewed-by           ; Reviewer ID (optional)
   review-notes)         ; Review notes (optional)
  transparent: #t)

;;; ============================================================================
;;; Rule Engine
;;; ============================================================================

(defstruct rule-engine
  (rules                 ; List of rules (sorted by priority)
   evaluation-history)   ; History of rule evaluations
  transparent: #t)

(def (make-rule-engine)
  "Create empty rule engine

   Returns:
     Rule engine"

  (make-rule-engine
   rules: '()
   evaluation-history: '()))

(def (rule-engine-add-rule! engine rule)
  "Add rule to engine

   Args:
     engine: Rule engine
     rule: Tool rule to add

   Returns:
     #t on success"

  ;; Add rule and re-sort by priority
  (set! (rule-engine-rules engine)
        (sort (cons rule (rule-engine-rules engine))
              (lambda (r1 r2)
                (> (tool-rule-priority r1) (tool-rule-priority r2)))))
  #t)

(def (rule-engine-remove-rule! engine rule-id)
  "Remove rule from engine

   Args:
     engine: Rule engine
     rule-id: Rule ID to remove

   Returns:
     #t if removed, #f if not found"

  (let ((rules (rule-engine-rules engine)))
    (let ((new-rules (filter (lambda (r) (not (equal? (tool-rule-id r) rule-id))) rules)))
      (if (= (length new-rules) (length rules))
          #f
          (begin
            (set! (rule-engine-rules engine) new-rules)
            #t)))))

(def (rule-engine-get-rule engine rule-id)
  "Get rule by ID

   Args:
     engine: Rule engine
     rule-id: Rule ID

   Returns:
     Rule or #f if not found"

  (let ((rules (rule-engine-rules engine)))
    (let loop ((rs rules))
      (cond
       ((null? rs) #f)
       ((equal? (tool-rule-id (car rs)) rule-id) (car rs))
       (else (loop (cdr rs)))))))

(def (rule-engine-evaluate engine tool-name arguments context)
  "Evaluate rules for tool call

   Args:
     engine: Rule engine
     tool-name: Tool name
     arguments: Tool arguments
     context: Execution context

   Returns:
     Rule evaluation result (:allow, :deny, :require-approval) and reason"

  (let ((rules (filter tool-rule-enabled (rule-engine-rules engine))))
    (let loop ((rs rules))
      (if (null? rs)
          ;; No rules matched - default to allow
          (cons :allow "No matching rules - default allow")
          (let ((rule (car rs)))
            (try
             (if ((tool-rule-condition rule) tool-name arguments context)
                 ;; Rule matched
                 (let ((evaluation (make-rule-evaluation
                                    rule-id: (tool-rule-id rule)
                                    matched: #t
                                    action: (tool-rule-action rule)
                                    reason: (tool-rule-description rule)
                                    timestamp: (current-seconds))))
                   ;; Add to history
                   (set! (rule-engine-evaluation-history engine)
                         (cons evaluation (rule-engine-evaluation-history engine)))
                   ;; Return action and reason
                   (cons (tool-rule-action rule) (tool-rule-description rule)))
                 ;; Rule didn't match - try next
                 (loop (cdr rs)))
             (catch (e)
               ;; Rule evaluation error - skip rule
               (displayln (format "Rule evaluation error: ~a" (error-message e)))
               (loop (cdr rs)))))))))

(def (rule-engine-get-history engine #!key (limit 10))
  "Get rule evaluation history

   Args:
     engine: Rule engine
     limit: Maximum number of evaluations to return

   Returns:
     List of rule evaluations"

  (take (rule-engine-evaluation-history engine)
        (min limit (length (rule-engine-evaluation-history engine)))))

;;; ============================================================================
;;; Approval Manager
;;; ============================================================================

(defstruct approval-manager
  (requests              ; Hash of approval requests by ID
   pending-queue)        ; Queue of pending requests
  transparent: #t)

(def (make-approval-manager)
  "Create approval manager

   Returns:
     Approval manager"

  (make-approval-manager
   requests: (hash)
   pending-queue: '()))

(def (approval-manager-create-request! manager tool-call reason)
  "Create approval request

   Args:
     manager: Approval manager
     tool-call: Tool call requiring approval
     reason: Reason for approval requirement

   Returns:
     Approval request"

  (let ((request (make-approval-request
                  id: (uuid-generate)
                  tool-call: tool-call
                  requested-at: (current-seconds)
                  requested-by: (tool-call-agent-id tool-call)
                  reason: reason
                  status: :pending
                  reviewed-at: #f
                  reviewed-by: #f
                  review-notes: #f)))
    ;; Add to requests hash
    (hash-put! (approval-manager-requests manager)
               (approval-request-id request)
               request)
    ;; Add to pending queue
    (set! (approval-manager-pending-queue manager)
          (cons request (approval-manager-pending-queue manager)))
    request))

(def (approval-manager-approve-request! manager request-id reviewer-id notes)
  "Approve approval request

   Args:
     manager: Approval manager
     request-id: Request ID
     reviewer-id: Reviewer ID
     notes: Review notes (optional)

   Returns:
     Updated approval request or #f if not found"

  (let ((request (hash-ref (approval-manager-requests manager) request-id #f)))
    (if request
        (begin
          (approval-request-status-set! request :approved)
          (approval-request-reviewed-at-set! request (current-seconds))
          (approval-request-reviewed-by-set! request reviewer-id)
          (approval-request-review-notes-set! request notes)
          ;; Remove from pending queue
          (set! (approval-manager-pending-queue manager)
                (filter (lambda (r) (not (equal? (approval-request-id r) request-id)))
                       (approval-manager-pending-queue manager)))
          request)
        #f)))

(def (approval-manager-reject-request! manager request-id reviewer-id reason)
  "Reject approval request

   Args:
     manager: Approval manager
     request-id: Request ID
     reviewer-id: Reviewer ID
     reason: Rejection reason

   Returns:
     Updated approval request or #f if not found"

  (let ((request (hash-ref (approval-manager-requests manager) request-id #f)))
    (if request
        (begin
          (approval-request-status-set! request :rejected)
          (approval-request-reviewed-at-set! request (current-seconds))
          (approval-request-reviewed-by-set! request reviewer-id)
          (approval-request-review-notes-set! request reason)
          ;; Remove from pending queue
          (set! (approval-manager-pending-queue manager)
                (filter (lambda (r) (not (equal? (approval-request-id r) request-id)))
                       (approval-manager-pending-queue manager)))
          request)
        #f)))

(def (approval-manager-get-pending manager)
  "Get pending approval requests

   Args:
     manager: Approval manager

   Returns:
     List of pending requests"

  (approval-manager-pending-queue manager))

(def (approval-manager-get-request manager request-id)
  "Get approval request by ID

   Args:
     manager: Approval manager
     request-id: Request ID

   Returns:
     Approval request or #f if not found"

  (hash-ref (approval-manager-requests manager) request-id #f))

;;; ============================================================================
;;; Rule-Based Dispatcher
;;; ============================================================================

(defstruct rule-based-dispatcher
  (dispatcher            ; Underlying tool dispatcher
   rule-engine           ; Rule engine
   approval-manager)     ; Approval manager
  transparent: #t)

(def (make-rule-based-dispatcher dispatcher)
  "Create rule-based dispatcher

   Args:
     dispatcher: Tool dispatcher

   Returns:
     Rule-based dispatcher"

  (make-rule-based-dispatcher
   dispatcher: dispatcher
   rule-engine: (make-rule-engine)
   approval-manager: (make-approval-manager)))

(def (rule-based-dispatcher-call-tool rbd tool-name arguments agent-id)
  "Call tool through rule-based dispatcher

   Args:
     rbd: Rule-based dispatcher
     tool-name: Tool name
     arguments: Tool arguments
     agent-id: Agent ID

   Returns:
     Tool call or approval request"

  ;; Create execution context
  (let ((context (make-tool-execution-context
                  agent-id: agent-id
                  call-id: (uuid-generate)
                  timestamp: (current-seconds)
                  metadata: (hash))))

    ;; Evaluate rules
    (let ((evaluation (rule-engine-evaluate (rule-based-dispatcher-rule-engine rbd)
                                           tool-name
                                           arguments
                                           context)))
      (let ((action (car evaluation))
            (reason (cdr evaluation)))

        (cond
         ;; Deny - return error
         ((eq? action :deny)
          (make-tool-call
           id: (tool-execution-context-call-id context)
           tool-name: tool-name
           arguments: arguments
           timestamp: (tool-execution-context-timestamp context)
           agent-id: agent-id
           status: :failed
           result: #f
           error: (format "Tool execution denied: ~a" reason)))

         ;; Require approval - create approval request
         ((eq? action :require-approval)
          (let ((call (make-tool-call
                       id: (tool-execution-context-call-id context)
                       tool-name: tool-name
                       arguments: arguments
                       timestamp: (tool-execution-context-timestamp context)
                       agent-id: agent-id
                       status: :pending
                       result: #f
                       error: #f)))
            (approval-manager-create-request! (rule-based-dispatcher-approval-manager rbd)
                                             call
                                             reason)))

         ;; Allow - execute tool
         (else
          (dispatcher-call-tool (rule-based-dispatcher-dispatcher rbd)
                               tool-name
                               arguments
                               agent-id)))))))

(def (rule-based-dispatcher-approve-call! rbd request-id reviewer-id notes)
  "Approve pending tool call

   Args:
     rbd: Rule-based dispatcher
     request-id: Approval request ID
     reviewer-id: Reviewer ID
     notes: Review notes (optional)

   Returns:
     Executed tool call or #f if not found"

  (let ((request (approval-manager-approve-request! (rule-based-dispatcher-approval-manager rbd)
                                                    request-id
                                                    reviewer-id
                                                    notes)))
    (if request
        ;; Execute the approved tool call
        (let ((call (approval-request-tool-call request)))
          (dispatcher-call-tool (rule-based-dispatcher-dispatcher rbd)
                               (tool-call-tool-name call)
                               (tool-call-arguments call)
                               (tool-call-agent-id call)))
        #f)))

(def (rule-based-dispatcher-reject-call! rbd request-id reviewer-id reason)
  "Reject pending tool call

   Args:
     rbd: Rule-based dispatcher
     request-id: Approval request ID
     reviewer-id: Reviewer ID
     reason: Rejection reason

   Returns:
     Updated approval request or #f if not found"

  (approval-manager-reject-request! (rule-based-dispatcher-approval-manager rbd)
                                   request-id
                                   reviewer-id
                                   reason))

;;; ============================================================================
;;; Built-in Rules
;;; ============================================================================

(def (make-always-allow-rule)
  "Create rule that always allows tool execution"
  (make-tool-rule
   id: "always-allow"
   name: "Always Allow"
   description: "Allow all tool executions"
   condition: (lambda (tool-name arguments context) #t)
   action: :allow
   priority: 0
   enabled: #t
   metadata: (hash)))

(def (make-always-deny-rule)
  "Create rule that always denies tool execution"
  (make-tool-rule
   id: "always-deny"
   name: "Always Deny"
   description: "Deny all tool executions"
   condition: (lambda (tool-name arguments context) #t)
   action: :deny
   priority: 100
   enabled: #f
   metadata: (hash)))

(def (make-require-approval-for-tool-rule tool-name)
  "Create rule that requires approval for specific tool"
  (make-tool-rule
   id: (format "require-approval-~a" tool-name)
   name: (format "Require Approval for ~a" tool-name)
   description: (format "Tool ~a requires approval" tool-name)
   condition: (lambda (tn args ctx) (equal? tn tool-name))
   action: :require-approval
   priority: 50
   enabled: #t
   metadata: (hash 'tool tool-name)))

(def (make-deny-tool-rule tool-name)
  "Create rule that denies specific tool"
  (make-tool-rule
   id: (format "deny-~a" tool-name)
   name: (format "Deny ~a" tool-name)
   description: (format "Tool ~a is not allowed" tool-name)
   condition: (lambda (tn args ctx) (equal? tn tool-name))
   action: :deny
   priority: 75
   enabled: #t
   metadata: (hash 'tool tool-name)))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(def (approval-request->hash request)
  "Convert approval request to hash

   Args:
     request: Approval request

   Returns:
     Hash representation"

  (hash 'id (approval-request-id request)
        'tool_call (tool-call->hash (approval-request-tool-call request))
        'requested_at (approval-request-requested-at request)
        'requested_by (approval-request-requested-by request)
        'reason (approval-request-reason request)
        'status (symbol->string (approval-request-status request))
        'reviewed_at (approval-request-reviewed-at request)
        'reviewed_by (approval-request-reviewed-by request)
        'review_notes (approval-request-review-notes request)))

(def (tool-rule->hash rule)
  "Convert tool rule to hash

   Args:
     rule: Tool rule

   Returns:
     Hash representation"

  (hash 'id (tool-rule-id rule)
        'name (tool-rule-name rule)
        'description (tool-rule-description rule)
        'action (symbol->string (tool-rule-action rule))
        'priority (tool-rule-priority rule)
        'enabled (tool-rule-enabled rule)
        'metadata (tool-rule-metadata rule)))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create dispatcher with rules
(def dispatcher (make-tool-dispatcher))
(register-core-tools! dispatcher)

(def rbd (make-rule-based-dispatcher dispatcher))

;; Add rules
(rule-engine-add-rule! (rule-based-dispatcher-rule-engine rbd)
                       (make-require-approval-for-tool-rule "archival_memory_insert"))

;; Call tool - will require approval
(def result (rule-based-dispatcher-call-tool rbd
                                             "archival_memory_insert"
                                             (hash 'content "Test")
                                             agent-id))

;; Check if approval request
(if (approval-request? result)
    (begin
      (displayln "Approval required")
      ;; Approve request
      (def approved (rule-based-dispatcher-approve-call! rbd
                                                        (approval-request-id result)
                                                        "reviewer-123"
                                                        "Approved"))
      (displayln (format "Approved: ~a" (tool-call-status approved))))
    (displayln "Tool executed directly"))
|#
