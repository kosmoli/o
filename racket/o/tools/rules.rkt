#lang racket

;;; tools/rules.rkt - Tool Rules and Approval Workflow
;;;
;;; Rule-based tool execution control and enhanced approval workflow.

(provide (struct-out tool-rule)
         (struct-out rule-evaluation)
         (struct-out approval-request)
         (struct-out rule-engine)
         (struct-out approval-manager)
         (struct-out rule-based-dispatcher)
         make-rule-engine
         rule-engine-add-rule!
         rule-engine-remove-rule!
         rule-engine-get-rule
         rule-engine-evaluate
         rule-engine-get-history
         make-approval-manager
         approval-manager-create-request!
         approval-manager-approve-request!
         approval-manager-reject-request!
         approval-manager-get-pending
         approval-manager-get-request
         make-rule-based-dispatcher
         rule-based-dispatcher-call-tool
         rule-based-dispatcher-approve-call!
         rule-based-dispatcher-reject-call!
         make-always-allow-rule
         make-always-deny-rule
         make-require-approval-for-tool-rule
         make-deny-tool-rule
         approval-request->hash
         tool-rule->hash)

(require racket/hash
        racket/format
        racket/list
        racket/function
        "types.rkt"
        "core.rkt")

;;; ============================================================================
;;; Tool Rule Structures
;;; ============================================================================

(struct tool-rule
  (id                    ; Unique rule ID (string)
   name                  ; Rule name (string)
   description           ; Rule description (string)
   condition             ; Condition function (arguments, context) -> boolean
   action                ; Action to take ('allow, 'deny, 'require-approval)
   priority              ; Rule priority (higher = evaluated first)
   enabled               ; Is rule enabled? (boolean)
   metadata)             ; Additional metadata (hash)
  #:transparent)

(struct rule-evaluation
  (rule-id               ; Rule ID that was evaluated
   matched               ; Did condition match? (boolean)
   action                ; Action taken (symbol)
   reason                ; Reason for action (string)
   timestamp)            ; Evaluation timestamp
  #:transparent)

(struct approval-request
  (id                    ; Unique request ID (string)
   tool-call             ; Tool call awaiting approval
   requested-at          ; Request timestamp
   requested-by          ; Agent ID that requested
   reason                ; Reason for approval requirement
   status                ; Request status ('pending, 'approved, 'rejected)
   reviewed-at           ; Review timestamp (optional)
   reviewed-by           ; Reviewer ID (optional)
   review-notes)         ; Review notes (optional)
  #:transparent
  #:mutable)

;;; ============================================================================
;;; Rule Engine
;;; ============================================================================

(struct rule-engine
  (rules                 ; List of rules (sorted by priority)
   evaluation-history)   ; History of rule evaluations
  #:transparent
  #:mutable)

(define (make-rule-engine)
  "Create empty rule engine

   Returns:
     Rule engine"
  (rule-engine '() '()))

(define (rule-engine-add-rule! engine rule)
  "Add rule to engine

   Args:
     engine: Rule engine
     rule: Tool rule to add

   Returns:
     #t on success"

  ;; Add rule and re-sort by priority
  (set-rule-engine-rules! engine
        (sort (cons rule (rule-engine-rules engine))
              >
              #:key tool-rule-priority))
  #t)

(define (rule-engine-remove-rule! engine rule-id)
  "Remove rule from engine

   Args:
     engine: Rule engine
     rule-id: Rule ID to remove

   Returns:
     #t if removed, #f if not found"

  (define rules (rule-engine-rules engine))
  (define new-rules (filter (lambda (r) (not (equal? (tool-rule-id r) rule-id))) rules))
  (if (= (length new-rules) (length rules))
      #f
      (begin
        (set-rule-engine-rules! engine new-rules)
        #t)))

(define (rule-engine-get-rule engine rule-id)
  "Get rule by ID

   Args:
     engine: Rule engine
     rule-id: Rule ID

   Returns:
     Rule or #f if not found"

  (define rules (rule-engine-rules engine))
  (define (loop rs)
    (cond
     [(null? rs) #f]
     [(equal? (tool-rule-id (car rs)) rule-id) (car rs)]
     [else (loop (cdr rs))]))
  (loop rules))

(define (rule-engine-evaluate engine tool-name arguments context)
  "Evaluate rules for tool call

   Args:
     engine: Rule engine
     tool-name: Tool name
     arguments: Tool arguments
     context: Execution context

   Returns:
     Rule evaluation result ('allow, 'deny, 'require-approval) and reason"

  (define rules (filter tool-rule-enabled (rule-engine-rules engine)))

  (define (loop rs)
    (cond
     [(null? rs)
     ;; No rules matched - default to allow
     (cons 'allow "No matching rules - default allow")]
    [else
     (define rule (car rs))
     (with-handlers* ([exn:fail?
                       (lambda (e)
                         ;; Rule evaluation error - log and skip
                         (displayln (format "Rule evaluation error: ~a" (exn-message e)))
                         (loop (cdr rs)))])
       (if ((tool-rule-condition rule) tool-name arguments context)
           ;; Rule matched
           (let ([evaluation (rule-evaluation
                               (tool-rule-id rule)
                               #t
                               (tool-rule-action rule)
                               (tool-rule-description rule)
                               (current-seconds))])
             ;; Add to history
             (set-rule-engine-evaluation-history! engine
                   (cons evaluation (rule-engine-evaluation-history engine)))
             ;; Return action and reason
             (cons (tool-rule-action rule) (tool-rule-description rule)))
           ;; Rule didn't match - try next
           (loop (cdr rs))))]))
  (loop rules))

(define (rule-engine-get-history engine #:limit [limit 10])
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

(struct approval-manager
  (requests              ; Hash of approval requests by ID
   pending-queue)        ; Queue of pending requests
  #:transparent
  #:mutable)

(define (make-approval-manager)
  "Create approval manager

   Returns:
     Approval manager"

  (approval-manager (hash) '()))

(define (approval-manager-create-request! manager tool-call reason)
  "Create approval request

   Args:
     manager: Approval manager
     tool-call: Tool call requiring approval
     reason: Reason for approval requirement

   Returns:
     Approval request"

  (define request
    (approval-request
     (format "req-~a" (current-seconds))
     tool-call
     (current-seconds)
     (tool-execution-agent-id tool-call)
     reason
     'pending
     #f
     #f
     #f))

  ;; Add to requests hash
  (hash-set! (approval-manager-requests manager)
              (approval-request-id request)
              request)

  ;; Add to pending queue
  (set-approval-manager-pending-queue! manager
        (cons request (approval-manager-pending-queue manager)))
  request)

(define (approval-manager-approve-request! manager request-id reviewer-id notes)
  "Approve approval request

   Args:
     manager: Approval manager
     request-id: Request ID
     reviewer-id: Reviewer ID
     notes: Review notes (optional)

   Returns:
     Updated approval request or #f if not found"

  (define request (hash-ref (approval-manager-requests manager) request-id #f))
  (if request
      (begin
        (set-approval-request-status! request 'approved)
        (set-approval-request-reviewed-at! request (current-seconds))
        (set-approval-request-reviewed-by! request reviewer-id)
        (set-approval-request-review-notes! request notes)
        ;; Remove from pending queue
        (set-approval-manager-pending-queue! manager
              (filter (lambda (r) (not (equal? (approval-request-id r) request-id)))
                      (approval-manager-pending-queue manager)))
        request)
      #f))

(define (approval-manager-reject-request! manager request-id reviewer-id reason)
  "Reject approval request

   Args:
     manager: Approval manager
     request-id: Request ID
     reviewer-id: Reviewer ID
     reason: Rejection reason

   Returns:
     Updated approval request or #f if not found"

  (define request (hash-ref (approval-manager-requests manager) request-id #f))
  (if request
      (begin
        (set-approval-request-status! request 'rejected)
        (set-approval-request-reviewed-at! request (current-seconds))
        (set-approval-request-reviewed-by! request reviewer-id)
        (set-approval-request-review-notes! request reason)
        ;; Remove from pending queue
        (set-approval-manager-pending-queue! manager
              (filter (lambda (r) (not (equal? (approval-request-id r) request-id)))
                      (approval-manager-pending-queue manager)))
        request)
      #f))

(define (approval-manager-get-pending manager)
  "Get pending approval requests

   Args:
     manager: Approval manager

   Returns:
     List of pending requests"

  (approval-manager-pending-queue manager))

(define (approval-manager-get-request manager request-id)
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

(struct rule-based-dispatcher
  (dispatcher            ; Underlying tool dispatcher
   rule-engine           ; Rule engine
   approval-manager)     ; Approval manager
  #:transparent)

(define (make-rule-based-dispatcher dispatcher)
  "Create rule-based dispatcher

   Args:
     dispatcher: Tool dispatcher

   Returns:
     Rule-based dispatcher"

  (rule-based-dispatcher
   dispatcher
   (make-rule-engine)
   (make-approval-manager)))

(define (rule-based-dispatcher-call-tool rbd tool-name arguments agent-id)
  "Call tool through rule-based dispatcher

   Args:
     rbd: Rule-based dispatcher
     tool-name: Tool name
     arguments: Tool arguments
     agent-id: Agent ID

   Returns:
     Tool call or approval request"

  ;; Create execution context
  (define context
    (tool-execution-context
     agent-id
     (format "call-~a" (current-seconds))
     arguments
     (current-seconds)
     (hash)))

  ;; Evaluate rules
  (define evaluation (rule-engine-evaluate (rule-based-dispatcher-rule-engine rbd)
                                           tool-name
                                           arguments
                                           context))
  (define action (car evaluation))
  (define reason (cdr evaluation))

  (cond
   ;; Deny - return error
   [(eq? action 'deny)
    (tool-execution
     (tool-execution-context-call-id context)
     tool-name
     arguments
     (tool-execution-context-timestamp context)
     agent-id
     'failed
     #f
     (format "Tool execution denied: ~a" reason))]

   ;; Require approval - create approval request
   [(eq? action 'require-approval)
    (define call (tool-execution
                    (tool-execution-context-call-id context)
                    tool-name
                    arguments
                    (tool-execution-context-timestamp context)
                    agent-id
                    'pending
                    #f
                    #f))
    (approval-manager-create-request! (rule-based-dispatcher-approval-manager rbd)
                                         call
                                         reason)]

   ;; Allow - execute tool
   [else
    (dispatcher-call-tool (rule-based-dispatcher-dispatcher rbd)
                         tool-name
                         arguments
                         agent-id)]))

(define (rule-based-dispatcher-approve-call! rbd request-id reviewer-id notes)
  "Approve pending tool call

   Args:
     rbd: Rule-based dispatcher
     request-id: Approval request ID
     reviewer-id: Reviewer ID
     notes: Review notes (optional)

   Returns:
     Executed tool call or #f if not found"

  (define request
    (approval-manager-approve-request! (rule-based-dispatcher-approval-manager rbd)
                                       request-id
                                       reviewer-id
                                       notes))
  (if request
      ;; Execute the approved tool call
      (let ([call (approval-request-tool-call request)])
        (dispatcher-call-tool (rule-based-dispatcher-dispatcher rbd)
                             (tool-execution-tool-name call)
                             (tool-execution-arguments call)
                             (tool-execution-agent-id call)))
      #f))

(define (rule-based-dispatcher-reject-call! rbd request-id reviewer-id reason)
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

(define (make-always-allow-rule)
  "Create rule that always allows tool execution"

  (tool-rule
   "always-allow"
   "Always Allow"
   "Allow all tool executions"
   (lambda (tool-name arguments context) #t)
   'allow
   0
   #t
   (hash)))

(define (make-always-deny-rule)
  "Create rule that always denies tool execution"

  (tool-rule
   "always-deny"
   "Always Deny"
   "Deny all tool executions"
   (lambda (tool-name arguments context) #t)
   'deny
   100
   #f
   (hash)))

(define (make-require-approval-for-tool-rule tool-name)
  "Create rule that requires approval for specific tool

   Args:
     tool-name: Tool name that requires approval

   Returns:
     Tool rule"

  (tool-rule
   (format "require-approval-~a" tool-name)
   (format "Require Approval for ~a" tool-name)
   (format "Tool ~a requires approval" tool-name)
   (lambda (tn args ctx) (equal? tn tool-name))
   'require-approval
   50
   #t
   (hash 'tool tool-name)))

(define (make-deny-tool-rule tool-name)
  "Create rule that denies specific tool

   Args:
     tool-name: Tool name to deny

   Returns:
     Tool rule"

  (tool-rule
   (format "deny-~a" tool-name)
   (format "Deny ~a" tool-name)
   (format "Tool ~a is not allowed" tool-name)
   (lambda (tn args ctx) (equal? tn tool-name))
   'deny
   75
   #t
   (hash 'tool tool-name)))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(define (approval-request->hash request)
  "Convert approval request to hash

   Args:
     request: Approval request

   Returns:
     Hash representation"

  (hash 'id (approval-request-id request)
        'tool_call (tool-execution->hash (approval-request-tool-call request))
        'requested_at (approval-request-requested-at request)
        'requested_by (approval-request-requested-by request)
        'reason (approval-request-reason request)
        'status (symbol->string (approval-request-status request))
        'reviewed_at (approval-request-reviewed-at request)
        'reviewed_by (approval-request-reviewed-by request)
        'review_notes (approval-request-review-notes request)))

(define (tool-rule->hash rule)
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
(define dispatcher (make-tool-dispatcher))
(register-core-tools! dispatcher)

(define rbd (make-rule-based-dispatcher dispatcher))

;; Add rules
(rule-engine-add-rule! (rule-based-dispatcher-rule-engine rbd)
                       (make-require-approval-for-tool-rule "archival_memory_insert"))

;; Call tool - will require approval
(define result (rule-based-dispatcher-call-tool rbd
                                             "archival_memory_insert"
                                             (hash 'content "Test")
                                             "agent-123"))

;; Check if approval request
(if (tool-execution-pending? result)
    (begin
      (displayln "Tool call requires approval")
      (displayln (format "Request ID: ~a" (tool-execution-id result))))
    ;; Tool was executed
    (displayln "Tool executed successfully"))
|#
