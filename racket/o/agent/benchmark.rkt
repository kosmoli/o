#lang racket

;;; agent/benchmark.rkt - Performance Benchmarking
;;;
;;; Performance benchmarking and profiling for agent execution system.

(provide (struct-out benchmark-config)
         (struct-out benchmark-result)
         (struct-out benchmark-suite)
         make-default-benchmark-config
         measure-time
         measure-time-with-result
         calculate-mean
         calculate-min
         calculate-max
         calculate-stddev
         run-benchmark
         format-time
         print-benchmark-result
         print-benchmark-suite
         benchmark-step-execution
         benchmark-agent-execution
         benchmark-result->hash
         benchmark-suite->hash)

(require racket/hash
        racket/format
        racket/math
        racket/list
        "./executor.rkt")

;;; ============================================================================
;;; Benchmark Types
;;; ============================================================================

(struct benchmark-config
  (name                  ; Benchmark name
   iterations            ; Number of iterations to run
   warmup-iterations     ; Number of warmup iterations
   timeout               ; Timeout per iteration (seconds)
   collect-memory        ; Collect memory statistics?
   metadata)             ; Additional metadata
  #:transparent)

(struct benchmark-result
  (name                  ; Benchmark name
   iterations            ; Number of iterations completed
   total-time            ; Total execution time (seconds)
   mean-time             ; Mean execution time (seconds)
   min-time              ; Minimum execution time (seconds)
   max-time              ; Maximum execution time (seconds)
   stddev-time           ; Standard deviation (seconds)
   throughput            ; Operations per second
   memory-used           ; Memory used (bytes, optional)
   metadata)             ; Additional metadata
  #:transparent)

(struct benchmark-suite
  (name                  ; Suite name
   benchmarks            ; List of benchmark results
   total-time            ; Total suite execution time
   metadata)             ; Additional metadata
  #:transparent)

;;; ============================================================================
;;; Benchmark Configuration
;;; ============================================================================

(define (make-default-benchmark-config name)
  "Create default benchmark configuration

   Args:
     name: Benchmark name

   Returns:
     Benchmark configuration"

  (benchmark-config
   name
   10
   2
   60
   #f
   (hash)))

;;; ============================================================================
;;; Timing Utilities
;;; ============================================================================

(define (measure-time thunk)
  "Measure execution time of a thunk

   Args:
     thunk: Function to measure

   Returns:
     Execution time in seconds"

  (define start (current-seconds))
  (thunk)
  (- (current-seconds) start))

(define (measure-time-with-result thunk)
  "Measure execution time and return both time and result

   Args:
     thunk: Function to measure

   Returns:
     (values result time)"

  (define start (current-seconds))
  (define result (thunk))
  (define time (- (current-seconds) start))
  (values result time))

;;; ============================================================================
;;; Statistics
;;; ============================================================================

(define (calculate-mean values)
  "Calculate mean of values

   Args:
     values: List of numbers

   Returns:
     Mean value"

  (if (null? values)
      0.0
      (/ (apply + values) (length values))))

(define (calculate-min values)
  "Calculate minimum of values

   Args:
     values: List of numbers

   Returns:
     Minimum value"

  (if (null? values)
      0.0
      (apply min values)))

(define (calculate-max values)
  "Calculate maximum of values

   Args:
     values: List of numbers

   Returns:
     Maximum value"

  (if (null? values)
      0.0
      (apply max values)))

(define (calculate-stddev values mean)
  "Calculate standard deviation

   Args:
     values: List of numbers
     mean: Mean value

   Returns:
     Standard deviation"

  (if (or (null? values) (<= (length values) 1))
      0.0
      (let* ([squared-diffs (map (lambda (x) (expt (- x mean) 2)) values)]
             [variance (/ (apply + squared-diffs) (- (length values) 1))])
        (sqrt variance))))

;;; ============================================================================
;;; Benchmark Execution
;;; ============================================================================

(define (run-benchmark config thunk)
  "Run benchmark with given configuration

   Args:
     config: Benchmark configuration
     thunk: Function to benchmark

   Returns:
     Benchmark result"

  (define name (benchmark-config-name config))
  (define iterations (benchmark-config-iterations config))
  (define warmup (benchmark-config-warmup-iterations config))
  (define times '())

  ;; Warmup iterations
  (displayln (format "[BENCHMARK] Warming up ~a (~a iterations)..." name warmup))
  (for-each (lambda (_) (thunk)) (range warmup))

  ;; Actual benchmark iterations
  (displayln (format "[BENCHMARK] Running ~a (~a iterations)..." name iterations))
  (define start-time (current-seconds))

  (for ([i (in-range iterations)])
    (define time (measure-time thunk))
    (set! times (cons time times))
    (when (= (modulo (+ i 1) (max 1 (quotient iterations 10))) 0)
      (displayln (format "[BENCHMARK] Progress: ~a/~a iterations" (+ i 1) iterations))))

  (define total-time (- (current-seconds) start-time))
  (define times-list (reverse times))
  (define mean (calculate-mean times-list))
  (define min-val (calculate-min times-list))
  (define max-val (calculate-max times-list))
  (define stddev (calculate-stddev times-list mean))
  (define throughput (/ iterations total-time))

  (benchmark-result
   name
   iterations
   total-time
   mean
   min-val
   max-val
   stddev
   throughput
   #f
   (hash)))

;;; ============================================================================
;;; Benchmark Reporting
;;; ============================================================================

(define (format-time seconds)
  "Format time in human-readable format

   Args:
     seconds: Time in seconds

   Returns:
     Formatted string"

  (cond
   [(< seconds 0.001)
    (format "~ams" (inexact->exact (round (* seconds 1000000))))]
   [(< seconds 1.0)
    (format "~ams" (inexact->exact (round (* seconds 1000))))]
   [else
    (format "~as" (exact->inexact seconds))]))

(define (print-benchmark-result result)
  "Print benchmark result

   Args:
     result: Benchmark result

   Returns:
     void"

  (displayln "")
  (displayln (format "Benchmark: ~a" (benchmark-result-name result)))
  (displayln (format "  Iterations:  ~a" (benchmark-result-iterations result)))
  (displayln (format "  Total time:  ~a" (format-time (benchmark-result-total-time result))))
  (displayln (format "  Mean time:   ~a" (format-time (benchmark-result-mean-time result))))
  (displayln (format "  Min time:    ~a" (format-time (benchmark-result-min-time result))))
  (displayln (format "  Max time:    ~a" (format-time (benchmark-result-max-time result))))
  (displayln (format "  Std dev:     ~a" (format-time (benchmark-result-stddev-time result))))
  (displayln (format "  Throughput:  ~a ops/sec"
                    (exact->inexact (benchmark-result-throughput result)))))

(define (print-benchmark-suite suite)
  "Print benchmark suite results

   Args:
     suite: Benchmark suite

   Returns:
     void"

  (displayln "")
  (displayln "========================================")
  (displayln (format "Benchmark Suite: ~a" (benchmark-suite-name suite)))
  (displayln "========================================")

  (for-each print-benchmark-result (benchmark-suite-benchmarks suite))

  (displayln "")
  (displayln (format "Total suite time: ~a"
                    (format-time (benchmark-suite-total-time suite))))
  (displayln "========================================")
  (displayln ""))

;;; ============================================================================
;;; Agent Execution Benchmarks
;;; ============================================================================

(define (benchmark-step-execution executor context step-type input
                                 #:config [config #f])
  "Benchmark single step execution

   Args:
     executor: Step executor
     context: Execution context
     step-type: Step type to benchmark
     input: Step input
     config: Benchmark configuration (optional)

   Returns:
     Benchmark result"

  (define bench-config
    (or config
        (make-default-benchmark-config
         (format "step-execution-~a" step-type))))

  (run-benchmark
   bench-config
   (lambda ()
     (define step (make-execution-step-record
                    (execution-context-agent-id context)
                    0
                    step-type
                    input))
     (execute-step executor context step))))

(define (benchmark-agent-execution executor context
                                   #:config [config #f])
  "Benchmark full agent execution

   Args:
     executor: Step executor
     context: Execution context
     config: Benchmark configuration (optional)

   Returns:
     Benchmark result"

  (define bench-config
    (or config
        (make-default-benchmark-config "agent-execution")))

  (run-benchmark
   bench-config
   (lambda ()
     (execute-agent executor context))))

;;; ============================================================================
;;; Conversion Functions
;;; ============================================================================

(define (benchmark-result->hash result)
  "Convert benchmark result to hash

   Args:
     result: Benchmark result

   Returns:
     Hash representation"

  (hash 'name (benchmark-result-name result)
        'iterations (benchmark-result-iterations result)
        'total_time (benchmark-result-total-time result)
        'mean_time (benchmark-result-mean-time result)
        'min_time (benchmark-result-min-time result)
        'max_time (benchmark-result-max-time result)
        'stddev_time (benchmark-result-stddev-time result)
        'throughput (benchmark-result-throughput result)
        'memory_used (benchmark-result-memory-used result)
        'metadata (benchmark-result-metadata result)))

(define (benchmark-suite->hash suite)
  "Convert benchmark suite to hash

   Args:
     suite: Benchmark suite

   Returns:
     Hash representation"

  (hash 'name (benchmark-suite-name suite)
        'benchmarks (map benchmark-result->hash (benchmark-suite-benchmarks suite))
        'total_time (benchmark-suite-total-time suite)
        'metadata (benchmark-suite-metadata suite)))
