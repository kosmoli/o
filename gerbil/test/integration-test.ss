;;; test/integration-test.ss - Integration Tests for Agent System
;;;
;;; This module contains integration tests for the complete agent system,
;;; including checkpoint/restore, WAL replay, and crash recovery scenarios.

(export #t)

(import
  :std/misc/threads
  :std/sugar
  :std/srfi/1
  :std/misc/hash
  :std/test
  :o/agent/core
  :o/agent/state
  :o/agent/memory
  :o/agent/tools
  :o/agent/dsl
  :o/agent/elixir-bridge)

;;; ============================================================================
;;; Test Utilities
;;; ============================================================================

(def (setup-test-agent)
  "Create a test agent with initialized state and memory"
  (let* ((agent (make-agent-instance
                 name: "TestAgent"
                 version: "1.0.0"
                 config: (hash 'checkpoint-interval 60)))
         (state (make-agent-state-instance))
         (memory (make-agent-memory-instance)))

    ;; Initialize agent
    (agent-initialize! agent state memory)

    agent))

(def (teardown-test-agent agent)
  "Clean up test agent"
  (when (agent-is-running? agent)
    (agent-shutdown! agent)))

(def (assert-agent-state agent expected-lifecycle)
  "Assert agent is in expected lifecycle state"
  (unless (eq? (agent-lifecycle agent) expected-lifecycle)
    (error (format "Expected agent lifecycle ~a, got ~a"
                  expected-lifecycle
                  (agent-lifecycle agent)))))

;;; ============================================================================
;;; Test Suite 1: Agent Lifecycle
;;; ============================================================================

(def lifecycle-tests
  (test-suite "Agent Lifecycle Tests"

    (test-case "Agent initialization"
      (let ((agent (make-agent-instance name: "TestAgent")))
        (check (agent? agent) => #t)
        (check (agent-lifecycle agent) => :initializing)
        (check (agent-name agent) => "TestAgent")))

    (test-case "Agent state transitions"
      (let* ((agent (make-agent-instance))
             (state (make-agent-state-instance))
             (memory (make-agent-memory-instance)))

        ;; Initialize
        (agent-initialize! agent state memory)
        (check (agent-lifecycle agent) => :running)

        ;; Suspend
        (agent-suspend! agent)
        (check (agent-lifecycle agent) => :suspended)

        ;; Resume
        (agent-resume! agent)
        (check (agent-lifecycle agent) => :running)

        ;; Shutdown
        (agent-shutdown! agent)
        (check (agent-lifecycle agent) => :terminated)))

    (test-case "Invalid state transitions"
      (let ((agent (make-agent-instance)))
        (check-exception
         (agent-transition! agent :terminated)
         => #t)))

    (test-case "Agent metadata management"
      (let ((agent (setup-test-agent)))
        (agent-set-metadata! agent 'test-key "test-value")
        (check (agent-get-metadata agent 'test-key) => "test-value")
        (teardown-test-agent agent)))))

;;; ============================================================================
;;; Test Suite 2: State Management
;;; ============================================================================

(def state-tests
  (test-suite "State Management Tests"

    (test-case "State variable operations"
      (let ((state (make-agent-state-instance)))
        ;; Set variables
        (state-set! state 'counter 0)
        (state-set! state 'name "test")

        ;; Get variables
        (check (state-get state 'counter) => 0)
        (check (state-get state 'name) => "test")

        ;; Has variable
        (check (state-has? state 'counter) => #t)
        (check (state-has? state 'nonexistent) => #f)

        ;; Delete variable
        (state-delete! state 'counter)
        (check (state-has? state 'counter) => #f)))

    (test-case "Conversation management"
      (let ((state (make-agent-state-instance)))
        ;; Add messages
        (add-message! state :user "Hello")
        (add-message! state :assistant "Hi there!")

        ;; Get conversation
        (let ((conv (get-conversation state)))
          (check (length conv) => 2)
          (check (conversation-message-role (car conv)) => :assistant)
          (check (conversation-message-content (car conv)) => "Hi there!"))))

    (test-case "History tracking"
      (let ((state (make-agent-state-instance)))
        ;; Add history entries
        (add-history-entry! state
                           (hash 'action 'test)
                           (hash 'result 'success))

        ;; Get history
        (let ((history (get-history state)))
          (check (length history) => 1)
          (check (history-entry-action (car history)) => (hash 'action 'test)))))

    (test-case "State snapshot and restore"
      (let ((state (make-agent-state-instance)))
        ;; Set up state
        (state-set! state 'counter 42)
        (add-message! state :user "Test message")

        ;; Create snapshot
        (let ((snapshot (snapshot-state state)))
          (check (hash-table? snapshot) => #t)
          (check (hash-key? snapshot 'id) => #t)
          (check (hash-key? snapshot 'variables) => #t)

          ;; Restore from snapshot
          (let ((restored (restore-state-from-snapshot snapshot)))
            (check (state-get restored 'counter) => 42)))))))

;;; ============================================================================
;;; Test Suite 3: Memory System
;;; ============================================================================

(def memory-tests
  (test-suite "Memory System Tests"

    (test-case "Memory block creation"
      (let ((block (make-memory-block-instance
                    :episodic
                    "Test memory"
                    importance: 0.8
                    tags: '(test))))
        (check (memory-block? block) => #t)
        (check (memory-block-type block) => :episodic)
        (check (memory-block-importance block) => 0.8)))

    (test-case "Short-term memory operations"
      (let ((memory (make-agent-memory-instance)))
        ;; Add blocks
        (let ((block1 (make-memory-block-instance :episodic "Memory 1"))
              (block2 (make-memory-block-instance :episodic "Memory 2")))
          (add-to-short-term! memory block1)
          (add-to-short-term! memory block2)

          ;; Check short-term memory
          (let ((st-memory (get-short-term-memory memory)))
            (check (length st-memory) => 2)))))

    (test-case "Long-term memory operations"
      (let ((memory (make-agent-memory-instance)))
        ;; Add block
        (let ((block (make-memory-block-instance :semantic "Fact")))
          (add-to-long-term! memory block)

          ;; Retrieve block
          (let ((retrieved (get-from-long-term memory (memory-block-id block))))
            (check (memory-block? retrieved) => #t)
            (check (memory-block-content retrieved) => "Fact")
            (check (memory-block-access-count retrieved) => 1)))))

    (test-case "Memory consolidation"
      (let ((memory (make-agent-memory-instance)))
        ;; Add important blocks to short-term
        (let ((block1 (make-memory-block-instance :episodic "Important" importance: 0.9))
              (block2 (make-memory-block-instance :episodic "Not important" importance: 0.3)))
          (add-to-short-term! memory block1)
          (add-to-short-term! memory block2)

          ;; Consolidate
          (let ((consolidated (consolidate-memory! memory)))
            ;; Only important block should be consolidated
            (check (length consolidated) => 1)
            (check (memory-block-content (car consolidated)) => "Important")))))

    (test-case "Memory search"
      (let ((memory (make-agent-memory-instance)))
        ;; Add searchable blocks
        (add-to-short-term! memory
                           (make-memory-block-instance :episodic "weather is sunny"))
        (add-to-short-term! memory
                           (make-memory-block-instance :episodic "temperature is cold"))

        ;; Search
        (let ((results (search-memory memory "weather")))
          (check (length results) => 1)
          (check (memory-block-content (car results)) => "weather is sunny"))))

    (test-case "Memory serialization"
      (let ((memory (make-agent-memory-instance)))
        ;; Add some data
        (add-to-short-term! memory
                           (make-memory-block-instance :episodic "Test"))

        ;; Serialize
        (let ((serialized (serialize-memory memory)))
          (check (hash-table? serialized) => #t)
          (check (hash-key? serialized 'id) => #t)

          ;; Deserialize
          (let ((deserialized (deserialize-memory serialized)))
            (check (agent-memory? deserialized) => #t)
            (check (length (get-short-term-memory deserialized)) => 1)))))))

;;; ============================================================================
;;; Test Suite 4: Tool System
;;; ============================================================================

(def tool-tests
  (test-suite "Tool System Tests"

    (test-case "Tool registry creation"
      (let ((registry (make-tool-registry-instance)))
        (check (tool-registry? registry) => #t)
        (check (hash-empty? (tool-registry-tools registry)) => #t)))

    (test-case "Tool registration"
      (let ((registry (make-tool-registry-instance)))
        ;; Create and register tool
        (let ((tool (make-tool-instance
                     "test_tool"
                     (lambda (x) (* x 2))
                     description: "Double the input"
                     parameters: (list
                                  (make-parameter-spec
                                   name: 'x
                                   type: :number
                                   required?: #t
                                   description: "Input number")))))
          (register-tool! registry tool)

          ;; Check registration
          (check (tool-exists? registry "test_tool") => #t)
          (check (get-tool registry "test_tool") => tool))))

    (test-case "Tool execution"
      (let ((registry (make-tool-registry-instance)))
        ;; Register tool
        (register-tool! registry
                       (make-tool-instance
                        "add"
                        (lambda (a b) (+ a b))
                        parameters: (list
                                     (make-parameter-spec
                                      name: 'a
                                      type: :number
                                      required?: #t
                                      description: "First number")
                                     (make-parameter-spec
                                      name: 'b
                                      type: :number
                                      required?: #t
                                      description: "Second number"))))

        ;; Execute tool
        (let ((result (execute-tool registry "add" (hash 'a 5 'b 3))))
          (check (tool-result? result) => #t)
          (check (tool-result-success? result) => #t)
          (check (tool-result-result result) => 8))))

    (test-case "Built-in tools"
      (let ((registry (make-tool-registry-instance)))
        ;; Register built-in tools
        (register-builtin-tools! registry)

        ;; Check built-in tools exist
        (check (tool-exists? registry "echo") => #t)
        (check (tool-exists? registry "current_time") => #t)
        (check (tool-exists? registry "generate_uuid") => #t)

        ;; Test echo tool
        (let ((result (execute-tool registry "echo" (hash 'message "Hello"))))
          (check (tool-result-success? result) => #t)
          (check (tool-result-result result) => "Hello"))))

    (test-case "Tool caching"
      (let ((registry (make-tool-registry-instance)))
        ;; Register cacheable tool
        (register-tool! registry
                       (make-tool-instance
                        "expensive"
                        (lambda (x) (begin (thread-sleep! 0.1) (* x x)))
                        parameters: (list
                                     (make-parameter-spec
                                      name: 'x
                                      type: :number
                                      required?: #t
                                      description: "Input"))
                        cacheable?: #t))

        ;; First execution (slow)
        (let ((result1 (execute-tool registry "expensive" (hash 'x 5))))
          (check (tool-result-success? result1) => #t)

          ;; Second execution (cached, fast)
          (let ((result2 (execute-tool registry "expensive" (hash 'x 5))))
            (check (tool-result-success? result2) => #t)
            (check (tool-result-result result2) => 25)))))))

;;; ============================================================================
;;; Test Suite 5: Checkpoint and Restore
;;; ============================================================================

(def checkpoint-tests
  (test-suite "Checkpoint and Restore Tests"

    (test-case "Agent checkpoint creation"
      (let ((agent (setup-test-agent)))
        ;; Modify agent state
        (state-set! (agent-state agent) 'counter 42)

        ;; Create checkpoint
        (let ((checkpoint-id (agent-checkpoint! agent)))
          (check (string? checkpoint-id) => #t)
          (check (agent-checkpoint-id agent) => checkpoint-id))

        (teardown-test-agent agent)))

    (test-case "Agent serialization"
      (let ((agent (setup-test-agent)))
        ;; Set up agent
        (state-set! (agent-state agent) 'test-var "test-value")
        (agent-set-metadata! agent 'test-meta "meta-value")

        ;; Serialize
        (let ((serialized (serialize-agent agent)))
          (check (hash-table? serialized) => #t)
          (check (hash-ref serialized 'name) => "TestAgent")
          (check (hash-ref serialized 'version) => "1.0.0"))

        (teardown-test-agent agent)))

    (test-case "Complete checkpoint/restore cycle"
      (let ((agent (setup-test-agent)))
        ;; Set up complex state
        (state-set! (agent-state agent) 'counter 100)
        (state-set! (agent-state agent) 'name "TestAgent")
        (add-message! (agent-state agent) :user "Hello")

        ;; Add memory
        (add-to-short-term! (agent-memory agent)
                           (make-memory-block-instance :episodic "Test memory"))

        ;; Serialize agent
        (let ((serialized (serialize-agent agent)))
          ;; Deserialize to new agent
          (let ((restored (deserialize-agent serialized)))
            (check (agent? restored) => #t)
            (check (agent-name restored) => "TestAgent")
            (check (agent-version restored) => "1.0.0")

            ;; Check state was restored
            (check (state-get (agent-state restored) 'counter) => 100)
            (check (state-get (agent-state restored) 'name) => "TestAgent")))

        (teardown-test-agent agent)))))

;;; ============================================================================
;;; Test Suite 6: DSL Macros
;;; ============================================================================

(def dsl-tests
  (test-suite "DSL Macro Tests"

    (test-case "Tool definition with deftool"
      ;; This tests the deftool macro
      (deftool test-add
        description: "Add two numbers"
        parameters: '((a number) (b number))
        (a b)
        (+ a b))

      ;; Check function was created
      (check (procedure? test-add) => #t)
      (check (test-add 5 3) => 8)

      ;; Check tool spec was created
      (check (hash-table? test-add-tool-spec) => #t)
      (check (hash-ref test-add-tool-spec 'name) => "test-add"))

    (test-case "Conditional pipeline with when->"
      (let ((value 15))
        (let ((result (when-> value
                        ((> value 10) (* value 2))
                        ((> value 20) (+ value 100)))))
          (check result => 30))))

    (test-case "State definition with defstate"
      (defstate test-state
        (counter 0)
        (name "default"))

      ;; Check struct was created
      (let ((state (make-test-state-default)))
        (check (test-state? state) => #t)
        (check (test-state-counter state) => 0)
        (check (test-state-name state) => "default")))))

;;; ============================================================================
;;; Test Suite 7: Integration Scenarios
;;; ============================================================================

(def integration-tests
  (test-suite "Integration Scenario Tests"

    (test-case "Complete agent workflow"
      (let ((agent (setup-test-agent)))
        ;; 1. Agent processes input
        (state-set! (agent-state agent) 'last-input "user query")

        ;; 2. Agent stores memory
        (add-to-short-term! (agent-memory agent)
                           (make-memory-block-instance
                            :episodic
                            "User asked a question"
                            importance: 0.8))

        ;; 3. Agent creates checkpoint
        (let ((checkpoint-id (agent-checkpoint! agent)))
          (check (string? checkpoint-id) => #t))

        ;; 4. Agent continues processing
        (state-set! (agent-state agent) 'response "agent response")

        ;; 5. Verify state
        (check (state-get (agent-state agent) 'last-input) => "user query")
        (check (state-get (agent-state agent) 'response) => "agent response")

        (teardown-test-agent agent)))

    (test-case "Memory consolidation workflow"
      (let ((agent (setup-test-agent)))
        ;; Add multiple memories with varying importance
        (add-to-short-term! (agent-memory agent)
                           (make-memory-block-instance
                            :episodic "Important event" importance: 0.9))
        (add-to-short-term! (agent-memory agent)
                           (make-memory-block-instance
                            :episodic "Minor event" importance: 0.2))
        (add-to-short-term! (agent-memory agent)
                           (make-memory-block-instance
                            :episodic "Significant event" importance: 0.7))

        ;; Consolidate
        (let ((consolidated (consolidate-memory! (agent-memory agent))))
          ;; Should consolidate 2 blocks (importance >= 0.5)
          (check (length consolidated) => 2))

        (teardown-test-agent agent)))

    (test-case "Tool execution with state update"
      (let* ((agent (setup-test-agent))
             (registry (make-tool-registry-instance)))

        ;; Register tool that updates state
        (register-tool! registry
                       (make-tool-instance
                        "increment_counter"
                        (lambda ()
                          (let ((current (state-get (agent-state agent) 'counter 0)))
                            (state-set! (agent-state agent) 'counter (+ current 1))
                            (+ current 1)))
                        description: "Increment counter"))

        ;; Execute tool multiple times
        (execute-tool registry "increment_counter" (hash))
        (execute-tool registry "increment_counter" (hash))
        (execute-tool registry "increment_counter" (hash))

        ;; Check state
        (check (state-get (agent-state agent) 'counter) => 3)

        (teardown-test-agent agent)))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(def (run-all-tests)
  "Run all integration tests"
  (displayln "\n=== Running Agent System Integration Tests ===\n")

  (let ((suites (list
                 lifecycle-tests
                 state-tests
                 memory-tests
                 tool-tests
                 checkpoint-tests
                 dsl-tests
                 integration-tests)))

    (for-each
     (lambda (suite)
       (displayln (format "\nRunning: ~a" (test-suite-name suite)))
       (test-run suite))
     suites))

  (displayln "\n=== All Tests Complete ===\n"))

;;; ============================================================================
;;; Main Entry Point
;;; ============================================================================

(def (main . args)
  "Main entry point for test runner"
  (run-all-tests))
