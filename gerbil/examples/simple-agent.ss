;;; examples/simple-agent.ss - Simple Example Agent
;;;
;;; This example demonstrates how to create a basic self-evolving agent
;;; using the Project O framework.

(export #t)

(import
  :gerbil/gambit/threads
  :std/sugar
  :std/srfi/1
  :std/misc/hash
  ../agent/core
  ../agent/state
  ../agent/memory
  ../agent/tools
  ../agent/dsl
  ../agent/elixir-bridge)

;;; ============================================================================
;;; Example 1: Simple Echo Agent
;;; ============================================================================

(def (create-echo-agent)
  "Create a simple agent that echoes user input"
  (let* ((agent (make-agent-instance
                 name: "EchoAgent"
                 version: "1.0.0"
                 config: (hash 'checkpoint-interval 300)))
         (state (make-agent-state-instance))
         (memory (make-agent-memory-instance)))

    ;; Initialize agent
    (agent-initialize! agent state memory)

    ;; Define perceive function
    (hash-put! (agent-metadata agent) 'perceive-fn
               (lambda (agent input)
                 (displayln (format "Perceived: ~a" input))
                 ;; Store in memory
                 (add-to-short-term! (agent-memory agent)
                                    (make-memory-block-instance
                                     :episodic
                                     (format "User said: ~a" input)
                                     importance: 0.5))
                 input))

    ;; Define think function
    (hash-put! (agent-metadata agent) 'think-fn
               (lambda (agent context)
                 (displayln (format "Thinking about: ~a" context))
                 ;; Simple echo response
                 (hash 'action 'respond
                       'message (format "You said: ~a" context))))

    ;; Define act function
    (hash-put! (agent-metadata agent) 'act-fn
               (lambda (agent action)
                 (displayln (format "Acting: ~a" (hash-ref action 'message)))
                 ;; Log to WAL
                 (elixir-wal-log! 'agent-response action)
                 action))

    agent))

;;; ============================================================================
;;; Example 2: Counter Agent with Tools
;;; ============================================================================

(def (create-counter-agent)
  "Create an agent that maintains a counter and provides tools to manipulate it"
  (let* ((agent (make-agent-instance
                 name: "CounterAgent"
                 version: "1.0.0"))
         (state (make-agent-state-instance))
         (memory (make-agent-memory-instance))
         (registry (make-tool-registry-instance)))

    ;; Initialize agent
    (agent-initialize! agent state memory)

    ;; Initialize counter
    (state-set! state 'counter 0)

    ;; Register increment tool
    (register-tool! registry
                   (make-tool-instance
                    "increment"
                    (lambda ()
                      (let ((current (state-get state 'counter)))
                        (state-set! state 'counter (+ current 1))
                        (+ current 1)))
                    description: "Increment the counter"
                    category: :custom))

    ;; Register decrement tool
    (register-tool! registry
                   (make-tool-instance
                    "decrement"
                    (lambda ()
                      (let ((current (state-get state 'counter)))
                        (state-set! state 'counter (- current 1))
                        (- current 1)))
                    description: "Decrement the counter"
                    category: :custom))

    ;; Register get counter tool
    (register-tool! registry
                   (make-tool-instance
                    "get_counter"
                    (lambda ()
                      (state-get state 'counter))
                    description: "Get current counter value"
                    category: :custom
                    cacheable?: #f))

    ;; Register reset tool
    (register-tool! registry
                   (make-tool-instance
                    "reset"
                    (lambda ()
                      (state-set! state 'counter 0)
                      0)
                    description: "Reset counter to zero"
                    category: :custom))

    ;; Store registry in agent metadata
    (agent-set-metadata! agent 'tool-registry registry)

    agent))

;;; ============================================================================
;;; Example 3: Memory-Enabled Agent
;;; ============================================================================

(def (create-memory-agent)
  "Create an agent that uses memory to remember past interactions"
  (let* ((agent (make-agent-instance
                 name: "MemoryAgent"
                 version: "1.0.0"))
         (state (make-agent-state-instance))
         (memory (make-agent-memory-instance)))

    ;; Initialize agent
    (agent-initialize! agent state memory)

    ;; Define perceive function
    (hash-put! (agent-metadata agent) 'perceive-fn
               (lambda (agent input)
                 ;; Store input in memory
                 (add-to-short-term! (agent-memory agent)
                                    (make-memory-block-instance
                                     :episodic
                                     input
                                     importance: 0.7
                                     tags: '(user-input)))
                 input))

    ;; Define think function
    (hash-put! (agent-metadata agent) 'think-fn
               (lambda (agent context)
                 ;; Search memory for similar past interactions
                 (let ((similar-memories (search-memory (agent-memory agent)
                                                       context
                                                       limit: 3)))
                   (hash 'action 'respond
                         'context context
                         'similar_memories (length similar-memories)))))

    ;; Define act function
    (hash-put! (agent-metadata agent) 'act-fn
               (lambda (agent action)
                 (let ((response (format "I found ~a similar memories"
                                       (hash-ref action 'similar_memories))))
                   ;; Store response in memory
                   (add-to-short-term! (agent-memory agent)
                                      (make-memory-block-instance
                                       :episodic
                                       response
                                       importance: 0.6
                                       tags: '(agent-response)))
                   response)))

    agent))

;;; ============================================================================
;;; Example 4: Agent with DSL
;;; ============================================================================

;; Define a custom tool using deftool macro
(deftool calculate-factorial
  description: "Calculate factorial of a number"
  parameters: '((n number))
  (n)
  (let loop ((i n) (result 1))
    (if (<= i 1)
        result
        (loop (- i 1) (* result i)))))

;; Define a custom state structure
(defstate calculator-state
  (last-result 0)
  (operation-count 0))

(def (create-dsl-agent)
  "Create an agent using DSL macros"
  (let* ((agent (make-agent-instance
                 name: "DSLAgent"
                 version: "1.0.0"))
         (state (make-calculator-state-default))
         (memory (make-agent-memory-instance))
         (registry (make-tool-registry-instance)))

    ;; Initialize agent
    (agent-initialize! agent state memory)

    ;; Register the factorial tool
    (register-tool! registry calculate-factorial-tool-spec)

    ;; Store registry
    (agent-set-metadata! agent 'tool-registry registry)

    agent))

;;; ============================================================================
;;; Example 5: Self-Evolving Agent
;;; ============================================================================

(def (create-evolving-agent)
  "Create an agent that can trigger its own evolution"
  (let* ((agent (make-agent-instance
                 name: "EvolvingAgent"
                 version: "1.0.0"
                 config: (hash 'evolution-threshold 100)))
         (state (make-agent-state-instance))
         (memory (make-agent-memory-instance)))

    ;; Initialize agent
    (agent-initialize! agent state memory)

    ;; Initialize performance counter
    (state-set! state 'request-count 0)
    (state-set! state 'error-count 0)

    ;; Define perceive function
    (hash-put! (agent-metadata agent) 'perceive-fn
               (lambda (agent input)
                 ;; Increment request count
                 (let ((count (state-get (agent-state agent) 'request-count)))
                   (state-set! (agent-state agent) 'request-count (+ count 1)))
                 input))

    ;; Define think function with evolution trigger
    (hash-put! (agent-metadata agent) 'think-fn
               (lambda (agent context)
                 (let* ((request-count (state-get (agent-state agent) 'request-count))
                        (error-count (state-get (agent-state agent) 'error-count))
                        (error-rate (if (> request-count 0)
                                       (/ error-count request-count)
                                       0))
                        (threshold (agent-get-config agent 'evolution-threshold)))

                   ;; Check if evolution should be triggered
                   (when (and (> request-count threshold)
                             (> error-rate 0.1))
                     (displayln "Evolution threshold reached!")
                     (agent-begin-evolution! agent))

                   ;; Return action
                   (hash 'action 'process
                         'request_count request-count
                         'error_rate error-rate))))

    ;; Define act function
    (hash-put! (agent-metadata agent) 'act-fn
               (lambda (agent action)
                 (displayln (format "Processing request #~a"
                                  (hash-ref action 'request_count)))
                 action))

    agent))

;;; ============================================================================
;;; Example 6: Complete Agent Workflow
;;; ============================================================================

(def (run-agent-workflow agent input-sequence)
  "Run a complete agent workflow with a sequence of inputs"
  (displayln (format "\n=== Starting Agent Workflow: ~a ===" (agent-name agent)))

  ;; Get agent functions
  (let ((perceive-fn (agent-get-metadata agent 'perceive-fn))
        (think-fn (agent-get-metadata agent 'think-fn))
        (act-fn (agent-get-metadata agent 'act-fn)))

    ;; Process each input
    (for-each
     (lambda (input)
       (displayln (format "\n--- Processing Input: ~a ---" input))

       ;; Perceive
       (let ((perceived (perceive-fn agent input)))
         ;; Think
         (let ((action (think-fn agent perceived)))
           ;; Act
           (act-fn agent action)))

       ;; Checkpoint periodically
       (when (should-checkpoint? agent)
         (displayln "Creating checkpoint...")
         (agent-checkpoint! agent)))

     input-sequence)

    ;; Final checkpoint
    (displayln "\nCreating final checkpoint...")
    (agent-checkpoint! agent)

    ;; Display statistics
    (displayln "\n=== Agent Statistics ===")
    (let ((status (agent-status agent)))
      (hash-for-each
       (lambda (k v)
         (displayln (format "~a: ~a" k v)))
       status))

    (displayln "\n=== Memory Statistics ===")
    (let ((mem-stats (memory-stats (agent-memory agent))))
      (hash-for-each
       (lambda (k v)
         (displayln (format "~a: ~a" k v)))
       mem-stats))

    (displayln (format "\n=== Workflow Complete: ~a ===\n" (agent-name agent)))))

(def (should-checkpoint? agent)
  "Check if agent should create checkpoint"
  (let* ((last-checkpoint (agent-updated-at agent))
         (now (time->seconds (current-time)))
         (interval (agent-get-config agent 'checkpoint-interval 300)))
    (> (- now last-checkpoint) interval)))

;;; ============================================================================
;;; Example Usage Demonstrations
;;; ============================================================================

(def (demo-echo-agent)
  "Demonstrate echo agent"
  (displayln "\n========================================")
  (displayln "DEMO 1: Echo Agent")
  (displayln "========================================")

  (let ((agent (create-echo-agent)))
    (run-agent-workflow agent
                       '("Hello, agent!"
                         "How are you?"
                         "Tell me a joke"))
    (agent-shutdown! agent)))

(def (demo-counter-agent)
  "Demonstrate counter agent"
  (displayln "\n========================================")
  (displayln "DEMO 2: Counter Agent with Tools")
  (displayln "========================================")

  (let* ((agent (create-counter-agent))
         (registry (agent-get-metadata agent 'tool-registry)))

    ;; Execute tools
    (displayln "\nExecuting tools:")
    (execute-tool registry "increment" (hash))
    (execute-tool registry "increment" (hash))
    (execute-tool registry "increment" (hash))

    (let ((result (execute-tool registry "get_counter" (hash))))
      (displayln (format "Counter value: ~a" (tool-result-result result))))

    (execute-tool registry "decrement" (hash))

    (let ((result (execute-tool registry "get_counter" (hash))))
      (displayln (format "Counter value: ~a" (tool-result-result result))))

    ;; Display tool statistics
    (displayln "\nTool Statistics:")
    (for-each
     (lambda (tool-name)
       (let ((stats (get-tool-stats registry tool-name)))
         (displayln (format "~a: ~a executions"
                          tool-name
                          (hash-ref stats 'total_executions)))))
     (list-tools registry))

    (agent-shutdown! agent)))

(def (demo-memory-agent)
  "Demonstrate memory agent"
  (displayln "\n========================================")
  (displayln "DEMO 3: Memory-Enabled Agent")
  (displayln "========================================")

  (let ((agent (create-memory-agent)))
    (run-agent-workflow agent
                       '("What is the weather like?"
                         "Tell me about the weather"
                         "How's the weather today?"))

    ;; Consolidate memory
    (displayln "\nConsolidating memory...")
    (consolidate-memory! (agent-memory agent))

    (agent-shutdown! agent)))

(def (demo-evolving-agent)
  "Demonstrate self-evolving agent"
  (displayln "\n========================================")
  (displayln "DEMO 4: Self-Evolving Agent")
  (displayln "========================================")

  (let ((agent (create-evolving-agent)))
    ;; Simulate many requests to trigger evolution
    (run-agent-workflow agent
                       (make-list 150 "test request"))

    (agent-shutdown! agent)))

;;; ============================================================================
;;; Main Demo Runner
;;; ============================================================================

(def (run-all-demos)
  "Run all example demonstrations"
  (displayln "\n")
  (displayln "╔════════════════════════════════════════════════════════╗")
  (displayln "║                                                        ║")
  (displayln "║          PROJECT O - AGENT SYSTEM EXAMPLES            ║")
  (displayln "║                                                        ║")
  (displayln "╚════════════════════════════════════════════════════════╝")
  (displayln "\n")

  ;; Start heartbeat thread
  (start-heartbeat-thread!)

  ;; Run demos
  (demo-echo-agent)
  (demo-counter-agent)
  (demo-memory-agent)
  (demo-evolving-agent)

  (displayln "\n")
  (displayln "╔════════════════════════════════════════════════════════╗")
  (displayln "║                                                        ║")
  (displayln "║              ALL DEMOS COMPLETED!                      ║")
  (displayln "║                                                        ║")
  (displayln "╚════════════════════════════════════════════════════════╝")
  (displayln "\n"))

;;; ============================================================================
;;; Main Entry Point
;;; ============================================================================

(def (main . args)
  "Main entry point for examples"
  (if (null? args)
      (run-all-demos)
      (case (string->symbol (car args))
        ((echo) (demo-echo-agent))
        ((counter) (demo-counter-agent))
        ((memory) (demo-memory-agent))
        ((evolving) (demo-evolving-agent))
        ((all) (run-all-demos))
        (else
         (displayln "Usage: gxi simple-agent.ss [echo|counter|memory|evolving|all]")
         (displayln "  echo     - Run echo agent demo")
         (displayln "  counter  - Run counter agent demo")
         (displayln "  memory   - Run memory agent demo")
         (displayln "  evolving - Run evolving agent demo")
         (displayln "  all      - Run all demos (default)")))))
