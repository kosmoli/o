;;; llm/test/run-tests.ss - Test runner for LLM clients
;;;
;;; Run all LLM client tests

(export #t)

(import
  :std/test
  :o/llm/test/types-test
  :o/llm/test/client-test)

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(def (run-all-tests)
  "Run all LLM client tests"
  (displayln "\n╔════════════════════════════════════════════════════════════╗")
  (displayln "║         LLM Client Test Suite                              ║")
  (displayln "╚════════════════════════════════════════════════════════════╝\n")

  ;; Run unit tests
  (displayln "=== Running Unit Tests ===\n")
  (run-types-tests)

  ;; Run integration tests
  (displayln "\n=== Running Integration Tests ===")
  (displayln "Note: Integration tests require API keys to be set:")
  (displayln "  - OPENAI_API_KEY")
  (displayln "  - ANTHROPIC_API_KEY")
  (displayln "  - GROQ_API_KEY")
  (displayln "  - Ollama server (optional)\n")
  (run-client-tests)

  (displayln "\n╔════════════════════════════════════════════════════════════╗")
  (displayln "║         Test Suite Complete                                ║")
  (displayln "╚════════════════════════════════════════════════════════════╝\n"))

;;; ============================================================================
;;; Main Entry Point
;;; ============================================================================

(def (main . args)
  "Main entry point for test runner"
  (run-all-tests))
