#lang racket

(require rackunit rackunit/text-ui
         "../agent/types.rkt"
         "../memory/types.rkt"
         "../memory/blocks.rkt")

;;; ============================================================================
;;; Agent Types Tests
;;; ============================================================================

(define agent-types-tests
  (test-suite
   "Agent Types Tests"

   (test-case "Create default agent config"
     (define config (make-default-agent-config "agent-123" "TestAgent"))
     (check-equal? (agent-config-id config) "agent-123")
     (check-equal? (agent-config-name config) "TestAgent")
     (check-equal? (agent-config-llm-provider config) 'anthropic))

   (test-case "Create initial agent state"
     (define state (make-initial-agent-state "agent-123"))
     (check-equal? (agent-state-agent-id state) "agent-123")
     (check-equal? (agent-state-step-count state) 0)
     (check-equal? (agent-state-status state) agent-status-idle))))

;;; ============================================================================
;;; Memory Types Tests
;;; ============================================================================

(define memory-types-tests
  (test-suite
   "Memory Types Tests"

   (test-case "Create memory block"
     (define block (memory-block
                    "id-123"
                    "agent-456"
                    "persona"
                    "You are helpful."
                    #t
                    #f
                    1234567890
                    1234567890))
     (check-equal? (memory-block-id block) "id-123")
     (check-equal? (memory-block-label block) "persona")
     (check-true (memory-block-is-template block)))

   (test-case "Core memory"
     (define memory (make-default-core-memory))
     (check-equal? (core-memory-persona memory) default-persona-template))

   (test-case "Validate block params"
     (define valid-params (hash 'label "test" 'value "test value"))
     (define result (validate-block-params valid-params))
     (check-true (car result)))))

;;; ============================================================================
;;; Memory Blocks Tests
;;; ============================================================================

(define memory-blocks-tests
  (test-suite
   "Memory Blocks Tests"

   (test-case "Create block manager"
     (define manager (create-block-manager "agent-123"))
     (check-equal? (block-manager-agent-id manager) "agent-123")
     (check-true (block-manager-cache-enabled manager)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(define (run-all-tests)
  (run-tests
   (test-suite
    "Project O Simple Tests"
    agent-types-tests
    memory-types-tests
    memory-blocks-tests)))

;;; Run tests if this file is executed directly
(module+ main
  (run-all-tests))
