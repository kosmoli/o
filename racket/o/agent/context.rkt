#lang racket

;;; agent/context.rkt - Context Window Manager
;;;
;;; Context window management for agent execution, handling token limits,
;;; message truncation, and context optimization.

(provide (struct-out context-window-manager)
         make-context-window-manager
         estimate-tokens
         count-message-tokens
         count-messages-tokens
         count-memory-block-tokens
         count-system-message-tokens
         optimize-context
         optimize-context-truncate
         optimize-context-summarize
         optimize-context-sliding
         truncate-messages
         analyze-context-usage
         check-context-fits)

(require racket/hash
        racket/format
        "../message/types.rkt"
        "../memory/types.rkt"
        "./types.rkt")

;;; ============================================================================
;;; Context Window Manager
;;; ============================================================================

(struct context-window-manager
  (token-counter         ; Token counter function
   max-tokens            ; Maximum tokens allowed
   system-tokens         ; Reserved tokens for system message
   response-tokens       ; Reserved tokens for response
   strategy              ; Optimization strategy ('truncate 'summarize 'sliding)
   metadata)             ; Additional metadata
  #:transparent)

(define (make-context-window-manager #:token-counter [token-counter estimate-tokens]
                                      #:max-tokens [max-tokens 100000]
                                      #:system-tokens [system-tokens 2000]
                                      #:response-tokens [response-tokens 4096]
                                      #:strategy [strategy 'truncate])
  "Create context window manager

   Args:
     token-counter: Function to count tokens (default: estimate-tokens)
     max-tokens: Maximum tokens allowed in context window
     system-tokens: Reserved tokens for system message
     response-tokens: Reserved tokens for response
     strategy: Optimization strategy ('truncate, 'summarize, 'sliding)

   Returns:
     Context window manager"

  (context-window-manager token-counter
                         max-tokens
                         system-tokens
                         response-tokens
                         strategy
                         (hash)))

;;; ============================================================================
;;; Token Counting
;;; ============================================================================

(define (estimate-tokens text)
  "Estimate token count for text using simple heuristic

   Args:
     text: Text to count tokens for

   Returns:
     Estimated token count"

  ;; Simple estimation: ~4 characters per token
  ;; This is a rough approximation, actual tokenization varies by model
  (if (string? text)
      (exact-round (/ (string-length text) 4))
      0))

(define (count-message-tokens manager msg)
  "Count tokens in a message

   Args:
     manager: Context window manager
     msg: Message to count

   Returns:
     Token count"

  (define counter (context-window-manager-token-counter manager))
  (define content (message-content msg))
  ;; Base tokens for message structure
  (+ 4  ; Message overhead (role, formatting, etc.)
     (counter content)))

(define (count-messages-tokens manager messages)
  "Count total tokens in messages

   Args:
     manager: Context window manager
     messages: List of messages

   Returns:
     Total token count"

  (apply + (map (lambda (msg) (count-message-tokens manager msg)) messages)))

(define (count-memory-block-tokens manager block)
  "Count tokens in a memory block

   Args:
     manager: Context window manager
     block: Memory block

   Returns:
     Token count"

  (define counter (context-window-manager-token-counter manager))
  (define label (memory-block-label block))
  (define value (memory-block-value block))
  ;; Count label + value + formatting
  (+ (counter label)
     (counter value)
     10))  ; Overhead for formatting

(define (count-system-message-tokens manager persona memory-blocks)
  "Count tokens in system message

   Args:
     manager: Context window manager
     persona: Agent persona text
     memory-blocks: List of memory blocks

   Returns:
     Token count"

  (define counter (context-window-manager-token-counter manager))
  (+ (counter persona)
     (apply + (map (lambda (block) (count-memory-block-tokens manager block))
                  memory-blocks))
     20))  ; Overhead for system message structure

;;; ============================================================================
;;; Context Window Optimization
;;; ============================================================================

(define (optimize-context manager context)
  "Optimize context to fit within token limits

   Args:
     manager: Context window manager
     context: Execution context

   Returns:
     Optimized execution context"

  (define strategy (context-window-manager-strategy manager))
  (case strategy
    [(truncate) (optimize-context-truncate manager context)]
    [(summarize) (optimize-context-summarize manager context)]
    [(sliding) (optimize-context-sliding manager context)]
    [else (error 'optimize-context "Unknown optimization strategy" strategy)]))

(define (optimize-context-truncate manager context)
  "Optimize context using truncation strategy

   Removes oldest messages until context fits within limits.

   Args:
     manager: Context window manager
     context: Execution context

   Returns:
     Optimized execution context"

  (define config (execution-context-agent-config context))
  (define persona (agent-config-persona config))
  (define memory-blocks (execution-context-memory-blocks context))
  (define history (execution-context-conversation-history context))

  ;; Calculate available tokens for messages
  (define max-tokens (context-window-manager-max-tokens manager))
  (define system-tokens (count-system-message-tokens manager persona memory-blocks))
  (define response-tokens (context-window-manager-response-tokens manager))
  (define available-tokens (- max-tokens system-tokens response-tokens))

  ;; Truncate messages from oldest
  (define optimized-history (truncate-messages manager history available-tokens))

  ;; Update context with optimized history
  (set-execution-context-conversation-history! context optimized-history)
  context)

(define (truncate-messages manager messages available-tokens)
  "Truncate messages to fit within available tokens

   Args:
     manager: Context window manager
     messages: List of messages (oldest first)
     available-tokens: Available token budget

   Returns:
     Truncated message list"

  (define (loop msgs tokens result)
    (cond
     [(null? msgs) (reverse result)]
     [else
      (define msg (car msgs))
      (define msg-tokens (count-message-tokens manager msg))
      (define new-tokens (+ tokens msg-tokens))
      (if (<= new-tokens available-tokens)
          ;; Message fits, include it
          (loop (cdr msgs) new-tokens (cons msg result))
          ;; Message doesn't fit, stop
          (reverse result))]))

  (loop (reverse messages) 0 '()))

(define (optimize-context-summarize manager context)
  "Optimize context using summarization strategy

   Summarizes older messages to reduce token count.
   Uses LLM to generate concise summaries of message groups.

   Args:
     manager: Context window manager
     context: Execution context

   Returns:
     Optimized execution context"

  (define config (execution-context-agent-config context))
  (define persona (agent-config-persona config))
  (define memory-blocks (execution-context-memory-blocks context))
  (define history (execution-context-conversation-history context))

  ;; Calculate available tokens
  (define max-tokens (context-window-manager-max-tokens manager))
  (define system-tokens (count-system-message-tokens manager persona memory-blocks))
  (define response-tokens (context-window-manager-response-tokens manager))
  (define available-tokens (- max-tokens system-tokens response-tokens))

  ;; Calculate current usage
  (define current-tokens (count-messages-tokens manager history))

  (if (<= current-tokens available-tokens)
      ;; Context fits, no optimization needed
      context
      ;; Need to summarize older messages
      (let* ([messages-to-summarize (max 0 (- (length history) 4))]  ; Keep last 4 messages
             [old-messages (take history messages-to-summarize)]
             [recent-messages (drop history messages-to-summarize)]

             ;; Create summary text from old messages
             [summary-text (format-conversation-for-summary old-messages)]
             ;; In a full implementation, we would call LLM here:
             ;; (define summary (llm-summarize summary-text))
             ;; For now, use a simple truncated summary
             [truncated-summary (truncate-summary-text summary-text 500)])

        ;; Create a summary message
        (define summary-message
          (make-system-message
           (format "[Previous conversation summary]~n~a" truncated-summary)))

        ;; Combine summary with recent messages
        (define optimized-history (append (list summary-message) recent-messages))

        ;; Update context with optimized history
        (set-execution-context-conversation-history! context optimized-history)
        context)))

(define (optimize-context-sliding manager context)
  "Optimize context using sliding window strategy

   Keeps most recent messages and a few important early messages.

   Args:
     manager: Context window manager
     context: Execution context

   Returns:
     Optimized execution context"

  (define config (execution-context-agent-config context))
  (define persona (agent-config-persona config))
  (define memory-blocks (execution-context-memory-blocks context))
  (define history (execution-context-conversation-history context))

  ;; Calculate available tokens
  (define max-tokens (context-window-manager-max-tokens manager))
  (define system-tokens (count-system-message-tokens manager persona memory-blocks))
  (define response-tokens (context-window-manager-response-tokens manager))
  (define available-tokens (- max-tokens system-tokens response-tokens))

  ;; Keep first 2 messages (context) and most recent messages
  (define-values (early-messages recent-messages)
    (if (> (length history) 2)
        (values (take history 2) (drop history 2))
        (values history '())))

  ;; Calculate tokens for early messages
  (define early-tokens (count-messages-tokens manager early-messages))
  (define remaining-tokens (- available-tokens early-tokens))

  ;; Truncate recent messages
  (define truncated-recent (truncate-messages manager recent-messages remaining-tokens))

  ;; Combine early and recent messages
  (define optimized-history (append early-messages truncated-recent))

  ;; Update context with optimized history
  (set-execution-context-conversation-history! context optimized-history)
  context)

;;; ============================================================================
;;; Context Window Analysis
;;; ============================================================================

(define (analyze-context-usage manager context)
  "Analyze token usage in context

   Args:
     manager: Context window manager
     context: Execution context

   Returns:
     Hash with usage statistics"

  (define config (execution-context-agent-config context))
  (define persona (agent-config-persona config))
  (define memory-blocks (execution-context-memory-blocks context))
  (define history (execution-context-conversation-history context))

  ;; Count tokens
  (define system-tokens (count-system-message-tokens manager persona memory-blocks))
  (define message-tokens (count-messages-tokens manager history))
  (define response-tokens (context-window-manager-response-tokens manager))
  (define total-tokens (+ system-tokens message-tokens response-tokens))
  (define max-tokens (context-window-manager-max-tokens manager))

  ;; Calculate percentages
  (define usage-percent (exact-floor (* 100 (/ total-tokens max-tokens))))
  (define available-tokens (- max-tokens total-tokens))

  (hash 'max_tokens max-tokens
        'system_tokens system-tokens
        'message_tokens message-tokens
        'response_tokens response-tokens
        'total_tokens total-tokens
        'available_tokens available-tokens
        'usage_percent usage-percent
        'message_count (length history)
        'needs_optimization (> total-tokens max-tokens)))

(define (check-context-fits manager context)
  "Check if context fits within token limits

   Args:
     manager: Context window manager
     context: Execution context

   Returns:
     #t if context fits, #f otherwise"

  (define usage (analyze-context-usage manager context))
  (<= (hash-ref usage 'total_tokens) (hash-ref usage 'max_tokens)))

;;; ============================================================================
;;; Summarization Helper Functions
;;; ============================================================================

(define (format-conversation-for-summary messages)
  "Format conversation messages for summarization

   Args:
     messages: List of messages to format

   Returns:
     Formatted string containing conversation"

  (define (format-single-msg msg)
    (define role (message-role msg))
    (define content (message-content msg))
    (define role-str (symbol->string role))
    (format "~a: ~a" role-str content))

  (string-join (map format-single-msg messages) "\n\n"))

(define (truncate-summary-text text max-chars)
  "Truncate summary text to maximum character count

   Args:
     text: Text to truncate
     max-chars: Maximum characters

   Returns:
     Truncated text with ellipsis if needed"

  (if (<= (string-length text) max-chars)
      text
      (string-append (substring text 0 (- max-chars 3)) "...")))
