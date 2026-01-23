#lang racket

;;; server/middleware.rkt - HTTP Middleware
;;;
;;; This module provides middleware functionality for HTTP request processing,
;;; including logging, error handling, CORS, and request validation.

(provide logging-middleware
         detailed-logging-middleware
         error-handling-middleware
         validation-error-middleware
         cors-middleware
         json-body-required-middleware
         content-type-validation-middleware
         rate-limiter
         rate-limiter-requests
         rate-limiter-window-start
         rate-limiter-max-requests
         rate-limiter-window-size
         set-rate-limiter-requests!
         set-rate-limiter-window-start!
         make-rate-limiter-instance
         rate-limiting-middleware
         request-id-middleware
         generate-request-id
         compose-middleware
         apply-middlewares
         standard-middleware-stack
         validation-error
         not-found-error
         unauthorized-error)

(require racket/hash
        racket/format
        "./http.rkt")

;;; ============================================================================
;;; Logging Middleware
;;; ============================================================================

(define (logging-middleware handler)
  "Log all HTTP requests and responses"
  (lambda (req)
    (define start-time (current-inexact-milliseconds))
    (define method (http-request-method req))
    (define path (http-request-path req))

    ;; Log request
    (printf "→ ~a ~a~n" method path)

    ;; Process request
    (define response (handler req))

    ;; Calculate duration
    (define end-time (current-inexact-milliseconds))
    (define duration-ms (- end-time start-time))

    ;; Log response
    (printf "← ~a ~a [~a] (~ams)~n"
            method path
            (http-response-status response)
            (exact-round duration-ms))

    response))

(define (detailed-logging-middleware handler)
  "Log requests with detailed information"
  (lambda (req)
    (define start-time (current-inexact-milliseconds))
    (define method (http-request-method req))
    (define path (http-request-path req))
    (define query-params (http-request-query-params req))
    (define headers (http-request-headers req))

    ;; Log detailed request
    (printf "→ Request: ~a ~a~n" method path)
    (unless (hash-empty? query-params)
      (printf "  Query: ~a~n" query-params))
    (printf "  Headers: ~a~n" (hash-keys headers))

    ;; Process request
    (define response (handler req))

    ;; Calculate duration
    (define end-time (current-inexact-milliseconds))
    (define duration-ms (- end-time start-time))

    ;; Log detailed response
    (printf "← Response: ~a (~ams)~n"
            (http-response-status response)
            (exact-round duration-ms))

    response))

;;; ============================================================================
;;; Error Handling Middleware
;;; ============================================================================

(define (error-handling-middleware handler)
  "Catch and handle errors in request processing"
  (lambda (req)
    (with-handlers* ([exn:fail?
                     (lambda (e)
                       (define msg (exn-message e))
                       (eprintf "Request error: ~a ~a - ~a~n"
                               (http-request-method req)
                               (http-request-path req)
                               msg)
                       (make-error-response
                        "Internal server error"
                        #:status 500
                        #:code "INTERNAL_ERROR"))])
      (handler req))))

(define (validation-error-middleware handler)
  "Handle validation errors specifically"
  (lambda (req)
    (with-handlers*
      ([validation-error?
        (lambda (e)
          (make-error-response
           "Validation error"
           #:status 400
           #:code "VALIDATION_ERROR"))]
       [not-found-error?
        (lambda (e)
          (make-error-response
           "Resource not found"
           #:status 404
           #:code "NOT_FOUND"))]
       [unauthorized-error?
        (lambda (e)
          (make-error-response
           "Unauthorized"
           #:status 401
           #:code "UNAUTHORIZED"))]
       [exn:fail?
        (lambda (e)
          (eprintf "Unexpected error: ~a~n" (exn-message e))
          (make-error-response
           "Internal server error"
           #:status 500
           #:code "INTERNAL_ERROR"))])
      (handler req))))

;;; ============================================================================
;;; CORS Middleware
;;; ============================================================================

(define (cors-middleware
         #:allowed-origins [allowed-origins '("*")]
         #:allowed-methods [allowed-methods '("GET" "POST" "PUT" "DELETE" "PATCH" "OPTIONS")]
         #:allowed-headers [allowed-headers '("Content-Type" "Authorization")]
         #:max-age [max-age 86400])
  "Add CORS headers to responses"
  (lambda (handler)
    (lambda (req)
      (define response
        (if (eq? (http-request-method req) 'OPTIONS)
            ;; Handle preflight request
            (make-text-response "" #:status 204)
            ;; Process normal request
            (handler req)))

      ;; Get current headers
      (define current-headers (http-response-headers response))

      ;; Add CORS headers
      (define origin
        (if (member "*" allowed-origins)
            "*"
            (string-join allowed-origins ", ")))

      (define new-headers
        (hash-set* current-headers
                   "Access-Control-Allow-Origin" origin
                   "Access-Control-Allow-Methods" (string-join allowed-methods ", ")
                   "Access-Control-Allow-Headers" (string-join allowed-headers ", ")
                   "Access-Control-Max-Age" (number->string max-age)))

      (struct-copy http-response response
                   [headers new-headers]))))

;;; ============================================================================
;;; Request Validation Middleware
;;; ============================================================================

(define (json-body-required-middleware handler)
  "Require JSON body for POST/PUT/PATCH requests"
  (lambda (req)
    (if (and (member (http-request-method req) '(POST PUT PATCH))
             (not (http-request-json req)))
        (make-error-response
         "JSON body required"
         #:status 400
         #:code "INVALID_REQUEST")
        (handler req))))

(define (content-type-validation-middleware expected-type)
  "Validate Content-Type header"
  (lambda (handler)
    (lambda (req)
      (define content-type (get-request-header req "content-type"))
      (if (and content-type
               (string-contains? content-type expected-type))
          (handler req)
          (make-error-response
           (format "Expected Content-Type: ~a" expected-type)
           #:status 415
           #:code "UNSUPPORTED_MEDIA_TYPE")))))

;;; ============================================================================
;;; Rate Limiting Middleware
;;; ============================================================================

(struct rate-limiter
  (requests      ; hash of IP -> request count
   window-start  ; hash of IP -> window start time
   max-requests  ; max requests per window
   window-size)  ; window size in seconds
  #:mutable
  #:transparent)

(define (make-rate-limiter-instance
         #:max-requests [max-requests 100]
         #:window-size [window-size 60])
  "Create rate limiter"
  (rate-limiter (hash) (hash) max-requests window-size))

(define (rate-limiting-middleware limiter)
  "Rate limit requests by IP address"
  (lambda (handler)
    (lambda (req)
      (define ip
        (or (get-request-header req "x-forwarded-for")
            (get-request-header req "remote-addr")
            "unknown"))
      (define now (current-seconds))
      (define window-start
        (hash-ref (rate-limiter-window-start limiter) ip 0))
      (define requests
        (hash-ref (rate-limiter-requests limiter) ip 0))

      ;; Check if window has expired
      (when (> (- now window-start) (rate-limiter-window-size limiter))
        ;; Reset window
        (set-rate-limiter-window-start! limiter (hash-set (rate-limiter-window-start limiter) ip now))
        (set-rate-limiter-requests! limiter (hash-set (rate-limiter-requests limiter) ip 0)))

      ;; Check rate limit
      (define current-requests
        (hash-ref (rate-limiter-requests limiter) ip 0))

      (if (>= current-requests (rate-limiter-max-requests limiter))
          (make-error-response
           "Rate limit exceeded"
           #:status 429
           #:code "RATE_LIMIT_EXCEEDED")
          (begin
            ;; Increment request count
            (set-rate-limiter-requests!
             limiter
             (hash-set (rate-limiter-requests limiter) ip (+ current-requests 1)))
            ;; Process request
            (handler req))))))

;;; ============================================================================
;;; Request ID Middleware
;;; ============================================================================

(define (request-id-middleware handler)
  "Add unique request ID to each request"
  (lambda (req)
    (define request-id (generate-request-id))
    (define response (handler req))
    (define current-headers (http-response-headers response))
    (define new-headers (hash-set current-headers "X-Request-ID" request-id))
    (struct-copy http-response response
                 [headers new-headers])))

(define (generate-request-id)
  "Generate unique request ID"
  (format "req-~a-~a"
          (current-seconds)
          (random 1000000)))

;;; ============================================================================
;;; Middleware Composition
;;; ============================================================================

(define (compose-middleware . middlewares)
  "Compose multiple middleware functions
   Example: (compose-middleware logging error-handling cors)"
  (lambda (handler)
    (for/fold ([h handler])
              ([middleware (in-list (reverse middlewares))])
      (middleware h))))

(define (apply-middlewares handler . middlewares)
  "Apply multiple middlewares to a handler"
  ((apply compose-middleware middlewares) handler))

;;; ============================================================================
;;; Standard Middleware Stack
;;; ============================================================================

(define (standard-middleware-stack
         #:enable-logging [enable-logging #t]
         #:enable-cors [enable-cors #t]
         #:enable-error-handling [enable-error-handling #t]
         #:enable-request-id [enable-request-id #t])
  "Create standard middleware stack"
  (define middlewares '())
  (define (add-middleware m) (cons m middlewares))

  (define final-middleware
    (append
     (if enable-request-id (list request-id-middleware) '())
     (if enable-logging (list logging-middleware) '())
     (if enable-error-handling (list error-handling-middleware) '())
     (if enable-cors (list (cors-middleware)) '())))

  (apply compose-middleware final-middleware))

;;; ============================================================================
;;; Error Types
;;; ============================================================================

(struct validation-error (message) #:transparent)
(struct not-found-error (message) #:transparent)
(struct unauthorized-error (message) #:transparent)
