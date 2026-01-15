;;; memory/core-test.ss - Core Memory Operations Tests
;;;
;;; Test suite for core memory operations.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/test
  ../database/client
  ./types
  ./blocks
  ./core)

;;; ============================================================================
;;; Test Setup
;;; ============================================================================

(def test-agent-id "test-agent-123")

(def (setup-test-manager)
  "Create test manager with initialized memory"
  (let ((manager (make-core-memory-manager test-agent-id
                                           cache-enabled: #t
                                           max-history: 50)))
    ;; Initialize core memory
    (core-memory-initialize! (core-memory-manager-block-manager manager)
                            persona: "I am a test assistant."
                            human: "Test user information.")
    manager))

;;; ============================================================================
;;; Core Memory Append Tests
;;; ============================================================================

(def core-memory-append-tests
  (test-suite "Core Memory Append Tests"

    (test-case "Append to existing block"
      (let ((manager (setup-test-manager)))
        (def result (core-memory-append! manager "persona" "I am helpful."))
        (check (string-contains result "I am a test assistant."))
        (check (string-contains result "I am helpful."))))

    (test-case "Append with custom separator"
      (let ((manager (setup-test-manager)))
        (def result (core-memory-append! manager "persona" "More info."
                                        separator: " | "))
        (check (string-contains result " | "))))

    (test-case "Append to empty block"
      (let ((manager (setup-test-manager)))
        ;; Clear block first
        (core-memory-clear-block! manager "persona")
        (def result (core-memory-append! manager "persona" "New content."))
        (check (equal? result "New content."))))

    (test-case "Append records history"
      (let ((manager (setup-test-manager)))
        (core-memory-append! manager "persona" "Test append.")
        (def history (core-memory-manager-history manager))
        (def changes (history-get-changes history limit: 1))
        (check (not (null? changes)))
        (check (equal? (memory-change-operation (car changes)) 'append))))))

;;; ============================================================================
;;; Core Memory Replace Tests
;;; ============================================================================

(def core-memory-replace-tests
  (test-suite "Core Memory Replace Tests"

    (test-case "Replace existing text"
      (let ((manager (setup-test-manager)))
        (def result (core-memory-replace! manager "persona" "test" "production"))
        (check (string-contains result "production"))
        (check (not (string-contains result "test")))))

    (test-case "Replace case-sensitive"
      (let ((manager (setup-test-manager)))
        (def result (core-memory-replace! manager "persona" "Test" "Production"
                                         case-sensitive?: #t))
        (check (string-contains result "test"))  ; lowercase 'test' unchanged
        (check (not (string-contains result "Test")))))

    (test-case "Replace non-existent text fails"
      (let ((manager (setup-test-manager)))
        (check-exception
         (core-memory-replace! manager "persona" "nonexistent" "new"))))

    (test-case "Replace records history"
      (let ((manager (setup-test-manager)))
        (core-memory-replace! manager "persona" "test" "production")
        (def history (core-memory-manager-history manager))
        (def changes (history-get-changes history limit: 1))
        (check (equal? (memory-change-operation (car changes)) 'replace))))))

;;; ============================================================================
;;; Memory Patch Tests
;;; ============================================================================

(def memory-patch-tests
  (test-suite "Memory Patch Tests"

    (test-case "Apply single append patch"
      (let ((manager (setup-test-manager)))
        (def result (memory-apply-patch! manager "persona"
                                        (hash 'op "append" 'value "Patched.")))
        (check (string-contains result "Patched."))))

    (test-case "Apply single replace patch"
      (let ((manager (setup-test-manager)))
        (def result (memory-apply-patch! manager "persona"
                                        (hash 'op "replace"
                                              'old "test"
                                              'new "production")))
        (check (string-contains result "production"))))

    (test-case "Apply set patch"
      (let ((manager (setup-test-manager)))
        (def result (memory-apply-patch! manager "persona"
                                        (hash 'op "set" 'value "New content.")))
        (check (equal? result "New content."))))

    (test-case "Apply clear patch"
      (let ((manager (setup-test-manager)))
        (def result (memory-apply-patch! manager "persona"
                                        (hash 'op "clear")))
        (check (equal? result ""))))

    (test-case "Apply prepend patch"
      (let ((manager (setup-test-manager)))
        (def result (memory-apply-patch! manager "persona"
                                        (hash 'op "prepend" 'value "Prefix: ")))
        (check (string-prefix? "Prefix: " result))))

    (test-case "Apply multiple patches"
      (let ((manager (setup-test-manager)))
        (def result (memory-apply-patch! manager "persona"
                                        (list (hash 'op "append" 'value " First.")
                                              (hash 'op "append" 'value " Second."))))
        (check (string-contains result "First."))
        (check (string-contains result "Second."))))

    (test-case "Patch records history"
      (let ((manager (setup-test-manager)))
        (memory-apply-patch! manager "persona" (hash 'op "append" 'value "Test."))
        (def history (core-memory-manager-history manager))
        (def changes (history-get-changes history limit: 1))
        (check (equal? (memory-change-operation (car changes)) 'patch))))))

;;; ============================================================================
;;; Memory Rollback Tests
;;; ============================================================================

(def memory-rollback-tests
  (test-suite "Memory Rollback Tests"

    (test-case "Rollback single step"
      (let ((manager (setup-test-manager)))
        (def original (block-get-value (core-memory-manager-block-manager manager)
                                       "persona"))
        (core-memory-append! manager "persona" "Change 1.")
        (memory-rollback! manager "persona" steps: 1)
        (def restored (block-get-value (core-memory-manager-block-manager manager)
                                       "persona"))
        (check (equal? original restored))))

    (test-case "Rollback multiple steps"
      (let ((manager (setup-test-manager)))
        (def original (block-get-value (core-memory-manager-block-manager manager)
                                       "persona"))
        (core-memory-append! manager "persona" "Change 1.")
        (core-memory-append! manager "persona" "Change 2.")
        (core-memory-append! manager "persona" "Change 3.")
        (memory-rollback! manager "persona" steps: 3)
        (def restored (block-get-value (core-memory-manager-block-manager manager)
                                       "persona"))
        (check (equal? original restored))))

    (test-case "Rollback to timestamp"
      (let ((manager (setup-test-manager)))
        (def timestamp (current-seconds))
        (core-memory-append! manager "persona" "Before timestamp.")
        (thread-sleep! 1)  ; Wait 1 second
        (def target-timestamp (current-seconds))
        (thread-sleep! 1)
        (core-memory-append! manager "persona" "After timestamp.")
        (memory-rollback-to-timestamp! manager "persona" target-timestamp)
        (def restored (block-get-value (core-memory-manager-block-manager manager)
                                       "persona"))
        (check (string-contains restored "Before timestamp."))
        (check (not (string-contains restored "After timestamp.")))))

    (test-case "Rollback with no history fails"
      (let ((manager (setup-test-manager)))
        (check-exception
         (memory-rollback! manager "nonexistent" steps: 1))))

    (test-case "Rollback records history"
      (let ((manager (setup-test-manager)))
        (core-memory-append! manager "persona" "Change.")
        (memory-rollback! manager "persona" steps: 1)
        (def history (core-memory-manager-history manager))
        (def changes (history-get-changes history limit: 1))
        (check (equal? (memory-change-operation (car changes)) 'rollback))))))

;;; ============================================================================
;;; Memory Constraints Tests
;;; ============================================================================

(def memory-constraints-tests
  (test-suite "Memory Constraints Tests"

    (test-case "Max block size constraint"
      (let* ((constraints (make-memory-constraints
                          max-block-size: 100
                          min-block-size: 0
                          readonly-blocks: '()
                          required-blocks: '("persona" "human")
                          custom-validators: '()))
             (manager (make-core-memory-manager test-agent-id
                                               constraints: constraints)))
        (core-memory-initialize! (core-memory-manager-block-manager manager))
        (check-exception
         (core-memory-append! manager "persona"
                             (make-string 200 #\a)))))

    (test-case "Read-only constraint"
      (let* ((constraints (make-memory-constraints
                          max-block-size: 10000
                          min-block-size: 0
                          readonly-blocks: '("persona")
                          required-blocks: '("persona" "human")
                          custom-validators: '()))
             (manager (make-core-memory-manager test-agent-id
                                               constraints: constraints)))
        (core-memory-initialize! (core-memory-manager-block-manager manager))
        (check-exception
         (core-memory-append! manager "persona" "Cannot modify."))))

    (test-case "Custom validator"
      (let* ((no-numbers-validator
              (lambda (label value)
                (if (string-contains value "0123456789")
                    (cons #f (list "Numbers not allowed"))
                    (cons #t #f))))
             (constraints (make-memory-constraints
                          max-block-size: 10000
                          min-block-size: 0
                          readonly-blocks: '()
                          required-blocks: '("persona" "human")
                          custom-validators: (list no-numbers-validator)))
             (manager (make-core-memory-manager test-agent-id
                                               constraints: constraints)))
        (core-memory-initialize! (core-memory-manager-block-manager manager))
        (check-exception
         (core-memory-append! manager "persona" "Contains 123."))))))

;;; ============================================================================
;;; Memory Validation Tests
;;; ============================================================================

(def memory-validation-tests
  (test-suite "Memory Validation Tests"

    (test-case "Valid core memory"
      (let ((manager (setup-test-manager)))
        (def result (validate-core-memory manager))
        (check (car result))))

    (test-case "Missing required block"
      (let* ((constraints (make-memory-constraints
                          max-block-size: 10000
                          min-block-size: 0
                          readonly-blocks: '()
                          required-blocks: '("persona" "human" "custom")
                          custom-validators: '()))
             (manager (make-core-memory-manager test-agent-id
                                               constraints: constraints)))
        (core-memory-initialize! (core-memory-manager-block-manager manager))
        (def result (validate-core-memory manager))
        (check (not (car result)))
        (check (member "Required block missing: custom" (cdr result)))))))

;;; ============================================================================
;;; Memory History Tests
;;; ============================================================================

(def memory-history-tests
  (test-suite "Memory History Tests"

    (test-case "History tracks changes"
      (let ((manager (setup-test-manager)))
        (core-memory-append! manager "persona" "Change 1.")
        (core-memory-replace! manager "persona" "test" "production")
        (memory-apply-patch! manager "persona" (hash 'op "append" 'value "Change 2."))
        (def history (core-memory-manager-history manager))
        (def changes (history-get-changes history limit: 10))
        (check (= (length changes) 3))))

    (test-case "History limited by max-history"
      (let ((manager (make-core-memory-manager test-agent-id max-history: 5)))
        (core-memory-initialize! (core-memory-manager-block-manager manager))
        ;; Make 10 changes
        (for-each
         (lambda (i)
           (core-memory-append! manager "persona" (format "Change ~a." i)))
         (iota 10))
        (def history (core-memory-manager-history manager))
        (def changes (history-get-changes history limit: 100))
        (check (<= (length changes) 5))))

    (test-case "Get changes for specific block"
      (let ((manager (setup-test-manager)))
        (core-memory-append! manager "persona" "Persona change.")
        (core-memory-append! manager "human" "Human change.")
        (def history (core-memory-manager-history manager))
        (def persona-changes (history-get-changes-for-block history "persona"))
        (check (= (length persona-changes) 1))
        (check (equal? (memory-change-block-label (car persona-changes)) "persona"))))))

;;; ============================================================================
;;; Memory Statistics Tests
;;; ============================================================================

(def memory-statistics-tests
  (test-suite "Memory Statistics Tests"

    (test-case "Get memory stats"
      (let ((manager (setup-test-manager)))
        (core-memory-append! manager "persona" "Change.")
        (def stats (get-memory-stats manager))
        (check (hash-key? stats 'total_blocks))
        (check (hash-key? stats 'total_changes))
        (check (> (hash-ref stats 'total_changes) 0))))))

;;; ============================================================================
;;; Convenience Functions Tests
;;; ============================================================================

(def convenience-functions-tests
  (test-suite "Convenience Functions Tests"

    (test-case "Set block directly"
      (let ((manager (setup-test-manager)))
        (def result (core-memory-set-block! manager "persona" "New content."))
        (check (equal? result "New content."))))

    (test-case "Clear block"
      (let ((manager (setup-test-manager)))
        (def result (core-memory-clear-block! manager "persona"))
        (check (equal? result ""))))

    (test-case "Prepend to block"
      (let ((manager (setup-test-manager)))
        (def result (core-memory-prepend! manager "persona" "Prefix: "))
        (check (string-prefix? "Prefix: " result))))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(def core-memory-test-suite
  (test-suite "Core Memory Operations Test Suite"
    core-memory-append-tests
    core-memory-replace-tests
    memory-patch-tests
    memory-rollback-tests
    memory-constraints-tests
    memory-validation-tests
    memory-history-tests
    memory-statistics-tests
    convenience-functions-tests))

;;; Run tests
#|
(import :std/test)
(test-run! core-memory-test-suite)
|#
