;;; agent/context.ss - Context Window Manager
;;;
;;; Context window management for agent execution, handling token limits,
;;; message truncation, and context optimization.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/error
  :o/message/types
  :o/memory/types
  :o/agent/types)

;;; ============================================================================
;;; Context Window Manager
;;; ============================================================================

(defstruct context-window-manager
  (token-counter         ; Token counter function
   max-tokens            ; Maximum tokens allowed
   system-tokens         ; Reserved tokens for system message
   response-tokens       ; Reserved tokens for response
   strategy              ; Optimization strategy (:truncate, :summarize, 'sliding)
   metadata)             ; Additional metadata
  transparent: #t)

(def (make-context-window-manager . rest
                                   (token-counter estimate-tokens)
                                   (max-tokens 100000)
                                   (system-tokens 2000)
                                   (response-tokens 4096)
                                   (strategy 'truncate))
  "Create context window manager

   Args:
     token-counter: Function to count tokens (default: estimate-tokens)
     max-tokens: Maximum tokens allowed in context window
     system-tokens: Reserved tokens for system message
     response-tokens: Reserved tokens for response
     strategy: Optimization strategy (:truncate, :summarize, 'sliding)

   Returns:
     Context window manager"

  (make-context-window-manager
   token-counter: token-counter
   max-tokens: max-tokens
   system-tokens: system-tokens
   response-tokens: response-tokens
   strategy: strategy
   metadata: (make-hash-table)))

;;; ============================================================================
;;; Token Counting
;;; ============================================================================

(def (estimate-tokens text)
  "Estimate token count for text using simple heuristic

   Args:
     text: Text to count tokens for

   Returns:
     Estimated token count"

  ;; Simple estimation: ~4 characters per token
  ;; This is a rough approximation, actual tokenization varies by model
  (inexact->exact (ceiling (/ (string-length text) 4))))

(def (count-message-tokens manager message)
  "Count tokens in a message

   Args:
     manager: Context window manager
     message: Message to count

   Returns:
     Token count"

  (let ((counter (context-window-manager-token-counter manager))
        (content (message-content message)))
    ;; Base tokens for message structure
    (+ 4  ; Message overhead (role, formatting, etc.)
       (counter content))))

(def (count-messages-tokens manager messages)
  "Count total tokens in messages

   Args:
     manager: Context window manager
     messages: List of messages

   Returns:
     Total token count"

  (apply + (map (lambda (msg) (count-message-tokens manager msg)) messages)))

(def (count-memory-block-tokens manager block)
  "Count tokens in a memory block

   Args:
     manager: Context window manager
     block: Memory block

   Returns:
     Token count"

  (let ((counter (context-window-manager-token-counter manager))
        (label (memory-block-label block))
        (value (memory-block-value block)))
    ;; Count label + value + formatting
    (+ (counter label)
       (counter value)
       10)))  ; Overhead for formatting

(def (count-system-message-tokens manager persona memory-blocks)
  "Count tokens in system message

   Args:
     manager: Context window manager
     persona: Agent persona text
     memory-blocks: List of memory blocks

   Returns:
     Token count"

  (let ((counter (context-window-manager-token-counter manager)))
    (+ (counter persona)
       (apply + (map (lambda (block) (count-memory-block-tokens manager block))
                    memory-blocks))
       20)))  ; Overhead for system message structure

;;; ============================================================================
;;; Context Window Optimization
;;; ============================================================================

(def (optimize-context manager context)
  "Optimize context to fit within token limits

   Args:
     manager: Context window manager
     context: Execution context

   Returns:
     Optimized execution context"

  (let ((strategy (context-window-manager-strategy manager)))
    (case strategy
      (('truncate)
       (optimize-context-truncate manager context))
      (('summarize)
       (optimize-context-summarize manager context))
      (('sliding)
       (optimize-context-sliding manager context))
      (else
       (error "Unknown optimization strategy" strategy)))))

(def (optimize-context-truncate manager context)
  "Optimize context using truncation strategy

   Removes oldest messages until context fits within limits.

   Args:
     manager: Context window manager
     context: Execution context

   Returns:
     Optimized execution context"

  (let* ((config (execution-context-agent-config context))
         (persona (agent-config-persona config))
         (memory-blocks (execution-context-memory-blocks context))
         (history (execution-context-conversation-history context))

         ;; Calculate available tokens for messages
         (max-tokens (context-window-manager-max-tokens manager))
         (system-tokens (count-system-message-tokens manager persona memory-blocks))
         (response-tokens (context-window-manager-response-tokens manager))
         (available-tokens (- max-tokens system-tokens response-tokens))

         ;; Truncate messages from oldest
         (optimized-history (truncate-messages manager history available-tokens)))

    ;; Update context with optimized history
    (execution-context-conversation-history-set! context optimized-history)
    context))

(def (truncate-messages manager messages available-tokens)
  "Truncate messages to fit within available tokens

   Args:
     manager: Context window manager
     messages: List of messages (oldest first)
     available-tokens: Available token budget

   Returns:
     Truncated message list"

  (let loop ((msgs (reverse messages))  ; Start from newest
             (tokens 0)
             (result '()))
    (if (null? msgs)
        result
        (let* ((msg (car msgs))
               (msg-tokens (count-message-tokens manager msg))
               (new-tokens (+ tokens msg-tokens)))
          (if (<= new-tokens available-tokens)
              ;; Message fits, include it
              (loop (cdr msgs) new-tokens (cons msg result))
              ;; Message doesn't fit, stop
              result)))))

(def (optimize-context-summarize manager context)
  "Optimize context using summarization strategy

   Summarizes older messages to reduce token count.
   Note: This is a placeholder - actual summarization requires LLM.

   Args:
     manager: Context window manager
     context: Execution context

   Returns:
     Optimized execution context"

  ;; For now, fall back to truncation
  ;; In a full implementation, this would:
  ;; 1. Identify old messages to summarize
  ;; 2. Call LLM to generate summary
  ;; 3. Replace old messages with summary
  (optimize-context-truncate manager context))

(def (optimize-context-sliding manager context)
  "Optimize context using sliding window strategy

   Keeps most recent messages and a few important early messages.

   Args:
     manager: Context window manager
     context: Execution context

   Returns:
     Optimized execution context"

  (let* ((config (execution-context-agent-config context))
         (persona (agent-config-persona config))
         (memory-blocks (execution-context-memory-blocks context))
         (history (execution-context-conversation-history context))

         ;; Calculate available tokens
         (max-tokens (context-window-manager-max-tokens manager))
         (system-tokens (count-system-message-tokens manager persona memory-blocks))
         (response-tokens (context-window-manager-response-tokens manager))
         (available-tokens (- max-tokens system-tokens response-tokens))

         ;; Keep first 2 messages (context) and most recent messages
         (early-messages (if (> (length history) 2)
                            (take history 2)
                            history))
         (recent-messages (if (> (length history) 2)
                             (drop history 2)
                             '()))

         ;; Calculate tokens for early messages
         (early-tokens (count-messages-tokens manager early-messages))
         (remaining-tokens (- available-tokens early-tokens))

         ;; Truncate recent messages
         (truncated-recent (truncate-messages manager recent-messages remaining-tokens))

         ;; Combine early and recent messages
         (optimized-history (append early-messages truncated-recent)))

    ;; Update context with optimized history
    (execution-context-conversation-history-set! context optimized-history)
    context))

;;; ============================================================================
;;; Context Window Analysis
;;; ============================================================================

(def (analyze-context-usage manager context)
  "Analyze token usage in context

   Args:
     manager: Context window manager
     context: Execution context

   Returns:
     Hash with usage statistics"

  (let* ((config (execution-context-agent-config context))
         (persona (agent-config-persona config))
         (memory-blocks (execution-context-memory-blocks context))
         (history (execution-context-conversation-history context))

         ;; Count tokens
         (system-tokens (count-system-message-tokens manager persona memory-blocks))
         (message-tokens (count-messages-tokens manager history))
         (response-tokens (context-window-manager-response-tokens manager))
         (total-tokens (+ system-tokens message-tokens response-tokens))
         (max-tokens (context-window-manager-max-tokens manager))

         ;; Calculate percentages
         (usage-percent (inexact->exact (floor (* 100 (/ total-tokens max-tokens)))))
         (available-tokens (- max-tokens total-tokens)))

    (let ((ht (make-hash-table)))
  (hash-put! ht 'max_tokens max-tokens)
  (hash-put! ht 'system_tokens system-tokens)
  (hash-put! ht 'message_tokens message-tokens)
  (hash-put! ht 'response_tokens response-tokens)
  (hash-put! ht 'total_tokens total-tokens)
  (hash-put! ht 'available_tokens available-tokens)
  (hash-put! ht 'usage_percent usage-percent)
  (hash-put! ht 'message_count (length history))
  ht)))

(def (check-context-fits manager context)
  "Check if context fits within token limits

   Args:
     manager: Context window manager
     context: Execution context

   Returns:
     #t if context fits, #f otherwise"

  (let ((usage (analyze-context-usage manager context)))
    (<= (hash-ref usage 'total_tokens) (hash-ref usage 'max_tokens))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (take lst n)
  "Take first n elements from list

   Args:
     lst: List
     n: Number of elements

   Returns:
     List of first n elements"

  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(def (drop lst n)
  "Drop first n elements from list

   Args:
     lst: List
     n: Number of elements

   Returns:
     List without first n elements"

  (if (or (null? lst) (<= n 0))
      lst
      (drop (cdr lst) (- n 1))))
