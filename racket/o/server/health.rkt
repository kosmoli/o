#lang racket

;;; server/health.rkt - Health Check Endpoints
;;;
;;; This module provides health check and status endpoints for monitoring.

(provide health-check-handler
         status-handler
         readiness-handler
         liveness-handler
         metrics-handler
         version-handler
         server-ready?
         server-alive?
         get-uptime
         collect-metrics)

(require racket/hash
        racket/format
        "./http.rkt")

;;; ============================================================================
;;; Server State
;;; ============================================================================

(define *server-start-time* (current-seconds))

;;; ============================================================================
;;; Health Check
;;; ============================================================================

(define (get-uptime)
  "Get server uptime in seconds"
  (- (current-seconds) *server-start-time*))

(define (health-check-handler req)
  "Basic health check endpoint"
  (make-json-response
   (hash 'status "ok"
         'timestamp (current-seconds)
         'uptime (get-uptime))))

;;; ============================================================================
;;; Detailed Status
;;; ============================================================================

(define (status-handler req)
  "Detailed status endpoint"
  (make-json-response
   (hash 'status "ok"
         'timestamp (current-seconds)
         'uptime (get-uptime)
         'version "0.1.0"
         'environment (get-environment)
         'system (get-system-info))))

(define (get-environment)
  "Get environment information"
  (hash 'racket_version "8.14"
        'platform (system-type)))

(define (get-system-info)
  "Get system information"
  (hash 'memory_usage "N/A"
        'cpu_usage "N/A"
        'active_connections 0))

;;; ============================================================================
;;; Readiness Check
;;; ============================================================================

(define (readiness-handler req)
  "Readiness check endpoint (for Kubernetes, etc.)"
  (if (server-ready?)
      (make-json-response
       (hash 'ready #t)
       #:status 200)
      (make-json-response
       (hash 'ready #f 'reason "Server not ready")
       #:status 503)))

(define (server-ready?)
  "Check if server is ready to accept requests"
  #t)

;;; ============================================================================
;;; Liveness Check
;;; ============================================================================

(define (liveness-handler req)
  "Liveness check endpoint (for Kubernetes, etc.)"
  (if (server-alive?)
      (make-json-response
       (hash 'alive #t)
       #:status 200)
      (make-json-response
       (hash 'alive #f)
       #:status 503)))

(define (server-alive?)
  "Check if server is alive"
  #t)

;;; ============================================================================
;;; Metrics Endpoint
;;; ============================================================================

(define (metrics-handler req)
  "Metrics endpoint (Prometheus-compatible format)"
  (define metrics (collect-metrics))
  (make-text-response
   (format-prometheus-metrics metrics)
   #:status 200))

(define (collect-metrics)
  "Collect server metrics"
  (hash 'http_requests_total 0
        'http_request_duration_seconds 0
        'http_errors_total 0
        'server_uptime_seconds (get-uptime)))

(define (format-prometheus-metrics metrics)
  "Format metrics in Prometheus format"
  (string-append
   "# HELP server_uptime_seconds Server uptime in seconds\n"
   "# TYPE server_uptime_seconds gauge\n"
   (format "server_uptime_seconds ~a\n" (hash-ref metrics 'server_uptime_seconds))
   "\n"
   "# HELP http_requests_total Total HTTP requests\n"
   "# TYPE http_requests_total counter\n"
   (format "http_requests_total ~a\n" (hash-ref metrics 'http_requests_total))
   "\n"))

;;; ============================================================================
;;; Version Endpoint
;;; ============================================================================

(define (version-handler req)
  "Version information endpoint"
  (make-json-response
   (hash 'version "0.1.0"
         'build_date "2026-01-20"
         'git_commit "unknown"
         'racket_version "8.14")))
