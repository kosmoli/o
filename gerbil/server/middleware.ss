;;; server/middleware.ss - HTTP Middleware
;;;
;;; This module provides middleware functionality for HTTP request processing,
;;; including logging, error handling, CORS, and request validation.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :std/logger
  :std/srfi/19
  :o/server/http)

;;; ============================================================================
;;; Middleware Types
;;; ============================================================================

;; Middleware is a function that takes a handler and returns a new handler
;; Type: (handler -> handler)
;; Where handler: (http-request -> http-response)

;;; ============================================================================
;;; Logging Middleware
;;; ============================================================================

(def (logging-middleware handler)
  "Log all HTTP requests and responses"
  (lambda (req)
    (let* ((start-time (current-time))
           (method (http-request-method req))
           (path (http-request-path req))

           ;; Log request
           (_ (infof "→ ~a ~a" method path))

           ;; Process request
           (response (handler req))

           ;; Calculate duration
           (end-time (current-time))
           (duration-ms (* 1000 (time-difference end-time start-time)))

           ;; Log response
           (_ (infof "← ~a ~a [~a] (~ams)"
                    method path
                    (http-response-status response)
                    duration-ms)))

      response)))

(def (detailed-logging-middleware handler)
  "Log requests with detailed information"
  (lambda (req)
    (let* ((start-time (current-time))
           (method (http-request-method req))
           (path (http-request-path req))
           (query-params (http-request-query-params req))
           (headers (http-request-headers req))

           ;; Log detailed request
           (_ (begin
                (infof "→ Request: ~a ~a" method path)
                (when (not (hash-empty? query-params))
                  (infof "  Query: ~a" query-params))
                (infof "  Headers: ~a" (hash-keys headers))))

           ;; Process request
           (response (handler req))

           ;; Calculate duration
           (end-time (current-time))
           (duration-ms (* 1000 (time-difference end-time start-time)))

           ;; Log detailed response
           (_ (infof "← Response: ~a (~ams)"
                    (http-response-status response)
                    duration-ms)))

      response)))

;;; ============================================================================
;;; Error Handling Middleware
;;; ============================================================================

(def (error-handling-middleware handler)
  "Catch and handle errors in request processing"
  (lambda (req)
    (try
     (handler req)

     (catch (e)
       ;; Log error
       (errorf "Request error: ~a ~a - ~a"
              (http-request-method req)
              (http-request-path req)
              (error-message e))

       ;; Return error response
       (make-error-response
        "Internal server error"
        status: 500
        details: (error-message e))))))

(def (validation-error-middleware handler)
  "Handle validation errors specifically"
  (lambda (req)
    (try
     (handler req)

     (catch (e)
       (cond
        ;; Validation error
        ((validation-error? e)
         (make-error-response
          "Validation error"
          status: 400
          details: (error-message e)))

        ;; Not found error
        ((not-found-error? e)
         (make-error-response
          "Resource not found"
          status: 404
          details: (error-message e)))

        ;; Unauthorized error
        ((unauthorized-error? e)
         (make-error-response
          "Unauthorized"
          status: 401
          details: (error-message e)))

        ;; Other errors
        (else
         (errorf "Unexpected error: ~a" e)
         (make-error-response
          "Internal server error"
          status: 500)))))))

;;; ============================================================================
;;; CORS Middleware
;;; ============================================================================

(def (cors-middleware . rest
      (allowed-origins '("*"))
      (allowed-methods '("GET" "POST" "PUT" "DELETE" "PATCH" "OPTIONS"))
      (allowed-headers '("Content-Type" "Authorization"))
      (max-age 86400))
  "Add CORS headers to responses"
  (lambda (handler)
    (lambda (req)
      (let* ((response (if (eq? (http-request-method req) :OPTIONS)
                          ;; Handle preflight request
                          (make-text-response "" status: 204)
                          ;; Process normal request
                          (handler req)))

             ;; Add CORS headers
             (headers (http-response-headers response)))

        ;; Add CORS headers
        (hash-put! headers "Access-Control-Allow-Origin"
                  (if (member "*" allowed-origins)
                      "*"
                      (string-join allowed-origins ", ")))
        (hash-put! headers "Access-Control-Allow-Methods"
                  (string-join allowed-methods ", "))
        (hash-put! headers "Access-Control-Allow-Headers"
                  (string-join allowed-headers ", "))
        (hash-put! headers "Access-Control-Max-Age"
                  (number->string max-age))

        response))))

;;; ============================================================================
;;; Request Validation Middleware
;;; ============================================================================

(def (json-body-required-middleware handler)
  "Require JSON body for POST/PUT/PATCH requests"
  (lambda (req)
    (if (and (member (http-request-method req) '(:POST :PUT :PATCH))
             (not (http-request-json req)))
        (make-error-response
         "JSON body required"
         status: 400)
        (handler req))))

(def (content-type-validation-middleware expected-type)
  "Validate Content-Type header"
  (lambda (handler)
    (lambda (req)
      (let ((content-type (get-request-header req "content-type")))
        (if (and content-type
                 (string-contains content-type expected-type))
            (handler req)
            (make-error-response
             (format "Expected Content-Type: ~a" expected-type)
             status: 415))))))

;;; ============================================================================
;;; Rate Limiting Middleware
;;; ============================================================================

(defstruct rate-limiter
  (requests      ; hash of IP -> request count
   window-start  ; hash of IP -> window start time
   max-requests  ; max requests per window
   window-size)  ; window size in seconds
  transparent: #t)

(def (make-rate-limiter-instance . rest
      (max-requests 100)
      (window-size 60))
  "Create rate limiter"
  (make-rate-limiter
   requests: (make-hash-table)
   window-start: (make-hash-table)
   max-requests: max-requests
   window-size: window-size))

(def (rate-limiting-middleware limiter)
  "Rate limit requests by IP address"
  (lambda (handler)
    (lambda (req)
      (let* ((ip (get-request-header req "x-forwarded-for"))
             (ip (or ip (get-request-header req "remote-addr")))
             (ip (or ip "unknown"))
             (now (current-seconds))
             (window-start (hash-ref (rate-limiter-window-start limiter) ip 0))
             (requests (hash-ref (rate-limiter-requests limiter) ip 0)))

        ;; Check if window has expired
        (when (> (- now window-start) (rate-limiter-window-size limiter))
          ;; Reset window
          (hash-put! (rate-limiter-window-start limiter) ip now)
          (hash-put! (rate-limiter-requests limiter) ip 0)
          (set! requests 0))

        ;; Check rate limit
        (if (>= requests (rate-limiter-max-requests limiter))
            (make-error-response
             "Rate limit exceeded"
             status: 429)
            (begin
              ;; Increment request count
              (hash-put! (rate-limiter-requests limiter) ip (+ requests 1))
              ;; Process request
              (handler req)))))))

;;; ============================================================================
;;; Request ID Middleware
;;; ============================================================================

(def (request-id-middleware handler)
  "Add unique request ID to each request"
  (lambda (req)
    (let* ((request-id (generate-request-id))
           (response (handler req))
           (headers (http-response-headers response)))

      ;; Add request ID to response headers
      (hash-put! headers "X-Request-ID" request-id)

      response)))

(def (generate-request-id)
  "Generate unique request ID"
  (string-append
   "req-"
   (number->string (current-seconds))
   "-"
   (number->string (random-integer 1000000))))

;;; ============================================================================
;;; Middleware Composition
;;; ============================================================================

(def (compose-middleware . middlewares)
  "Compose multiple middleware functions
   Example: (compose-middleware logging error-handling cors)"
  (lambda (handler)
    (fold (lambda (middleware h)
            (middleware h))
          handler
          (reverse middlewares))))

(def (apply-middlewares handler . middlewares)
  "Apply multiple middlewares to a handler"
  ((compose-middleware . middlewares) handler))

;;; ============================================================================
;;; Standard Middleware Stack
;;; ============================================================================

(def (standard-middleware-stack . rest
      (enable-logging #t)
      (enable-cors #t)
      (enable-error-handling #t)
      (enable-request-id #t))
  "Create standard middleware stack"
  (let ((middlewares '()))

    ;; Add middlewares in order
    (when enable-request-id
      (set! middlewares (cons request-id-middleware middlewares)))

    (when enable-logging
      (set! middlewares (cons logging-middleware middlewares)))

    (when enable-error-handling
      (set! middlewares (cons error-handling-middleware middlewares)))

    (when enable-cors
      (set! middlewares (cons (cors-middleware) middlewares)))

    ;; Compose all middlewares
    (apply compose-middleware middlewares)))

;;; ============================================================================
;;; Error Types
;;; ============================================================================

(defstruct validation-error (message) transparent: #t)
(defstruct not-found-error (message) transparent: #t)
(defstruct unauthorized-error (message) transparent: #t)

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create handler
(def (my-handler req)
  (make-json-response (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Hello!")
  ht)))

;; Apply single middleware
(def logged-handler (logging-middleware my-handler))

;; Apply multiple middlewares
(def protected-handler
  (apply-middlewares my-handler
                     logging-middleware
                     error-handling-middleware
                     (cors-middleware)))

;; Use standard middleware stack
(def standard-handler
  (('standard-middleware-stack)  my-handler))

;; With router
(def router (make-router-instance))
(get! router "/" my-handler)

(def handler-with-middleware
  (('standard-middleware-stack) 
   (make-router-handler router)))

(def config (make-http-server-config-instance
             handler: handler-with-middleware))

(def server (start-http-server! config))
|#
