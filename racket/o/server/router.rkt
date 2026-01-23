#lang racket

;;; server/router.rkt - HTTP Router
;;;
;;; This module provides routing functionality for HTTP requests,
;;; including path matching, parameter extraction, and route registration.

(provide route
         route-method
         route-pattern
         route-handler
         route-params
         router
         router-routes
         router-not-found
         router-method-not-allowed
         make-router-instance
         default-not-found-handler
         default-method-not-allowed-handler
         add-route!
         get!
         post!
         put!
         delete!
         patch!
         any!
         extract-params-from-pattern
         pattern->regex
         match-route?
         extract-params
         find-matching-route
         find-routes-for-path
         route-request
         make-router-handler
         group-routes!
         list-routes
         print-routes)

(require racket/match
        racket/hash
        racket/format
        "./http.rkt")

;;; ============================================================================
;;; Route Structures
;;; ============================================================================

(struct route
  (method        ; HTTP method ('GET 'POST 'PUT 'DELETE 'PATCH 'ANY)
   pattern       ; path pattern (e.g., "/users/:id")
   handler       ; handler function
   params)
  #:transparent)

(struct router
  (routes        ; list of route structures
   not-found     ; 404 handler
   method-not-allowed) ; 405 handler
  #:transparent
  #:mutable)

;;; ============================================================================
;;; Router Creation
;;; ============================================================================

(define (make-router-instance
         #:not-found [not-found default-not-found-handler]
         #:method-not-allowed [method-not-allowed default-method-not-allowed-handler])
  "Create a new router"
  (router '() not-found method-not-allowed))

(define (default-not-found-handler req)
  "Default 404 handler"
  (make-error-response
   (format "Not found: ~a" (http-request-path req))
   #:status 404))

(define (default-method-not-allowed-handler req)
  "Default 405 handler"
  (make-error-response
   (format "Method not allowed: ~a ~a"
           (http-request-method req)
           (http-request-path req))
   #:status 405))

;;; ============================================================================
;;; Route Registration
;;; ============================================================================

(define (add-route! r method pattern handler)
  "Add a route to the router"
  (define params (extract-params-from-pattern pattern))
  (define new-route (route method pattern handler params))
  (set-router-routes! r (cons new-route (router-routes r)))
  r)

(define (get! r pattern handler)
  "Register GET route"
  (add-route! r 'GET pattern handler))

(define (post! r pattern handler)
  "Register POST route"
  (add-route! r 'POST pattern handler))

(define (put! r pattern handler)
  "Register PUT route"
  (add-route! r 'PUT pattern handler))

(define (delete! r pattern handler)
  "Register DELETE route"
  (add-route! r 'DELETE pattern handler))

(define (patch! r pattern handler)
  "Register PATCH route"
  (add-route! r 'PATCH pattern handler))

(define (any! r pattern handler)
  "Register route for any HTTP method"
  (add-route! r 'ANY pattern handler))

;;; ============================================================================
;;; Pattern Matching
;;; ============================================================================

(define (extract-params-from-pattern pattern)
  "Extract parameter names from route pattern
   Example: '/users/:id/posts/:post_id' => '(id post_id)"
  (define segments (string-split pattern "/"))
  (filter-map
   (lambda (seg)
     (and (> (string-length seg) 0)
          (char=? (string-ref seg 0) #\:)
          (string->symbol (substring seg 1))))
   segments))

(define (pattern->regex pattern)
  "Convert route pattern to regex
   Example: '/users/:id' => '^/users/([^/]+)$'"
  (define segments (string-split pattern "/"))
  (define regex-segments
    (map (lambda (seg)
           (if (and (> (string-length seg) 0)
                    (char=? (string-ref seg 0) #\:))
               "([^/]+)"  ; Match parameter
               seg))       ; Literal segment
         segments))
  (string-append "^" (string-join regex-segments "/") "$"))

(define (match-route? r path)
  "Check if route pattern matches path"
  (define regex (pattern->regex (route-pattern r)))
  (regexp-match? regex path))

(define (extract-params r path)
  "Extract parameter values from path using route pattern"
  (define regex (pattern->regex (route-pattern r)))
  (define match-result (regexp-match regex path))
  (if match-result
      (let ([param-values (cdr match-result)]  ; Skip full match
            [param-names (route-params r)])
        (for/hash ([name param-names]
                   [val (in-list param-values)])
          (values name val)))
      (hash)))

;;; ============================================================================
;;; Route Matching
;;; ============================================================================

(define (find-matching-route r method path)
  "Find route that matches method and path"
  (for/first ([rt (in-list (router-routes r))]
              #:when (and (or (eq? (route-method rt) 'ANY)
                              (eq? (route-method rt) method))
                         (match-route? rt path)))
    rt))

(define (find-routes-for-path r path)
  "Find all routes that match path (regardless of method)"
  (filter (lambda (rt) (match-route? rt path))
          (router-routes r)))

;;; ============================================================================
;;; Request Routing
;;; ============================================================================

(define (route-request r req)
  "Route HTTP request to appropriate handler"
  (define method (http-request-method req))
  (define path (http-request-path req))
  (define matching-route (find-matching-route r method path))

  (cond
   ;; Route found
   [matching-route
    (define params (extract-params matching-route path))
    (set-http-request-params! req params)
    ((route-handler matching-route) req)]

   ;; Path exists but method not allowed
   [(not (null? (find-routes-for-path r path)))
    ((router-method-not-allowed r) req)]

   ;; Not found
   [else
    ((router-not-found r) req)]))

(define (make-router-handler r)
  "Create request handler function from router"
  (lambda (req)
    (route-request r req)))

;;; ============================================================================
;;; Route Groups
;;; ============================================================================

(define (group-routes! r prefix routes-fn)
  "Group routes under a common prefix
   Example: (group-routes! router '/api/v1' (lambda (r) ...))"
  (define sub-router (make-router-instance))
  ;; Register routes in sub-router
  (routes-fn sub-router)
  ;; Add all sub-router routes to main router with prefix
  (for ([rt (in-list (router-routes sub-router))])
    (define prefixed-pattern (string-append prefix (route-pattern rt)))
    (add-route! r
                (route-method rt)
                prefixed-pattern
                (route-handler rt)))
  r)

;;; ============================================================================
;;; Route Inspection
;;; ============================================================================

(define (list-routes r)
  "List all registered routes"
  (map (lambda (rt)
         (hash 'method (route-method rt)
               'pattern (route-pattern rt)
               'params (route-params rt)))
       (router-routes r)))

(define (print-routes r)
  "Print all registered routes"
  (displayln "\n=== Registered Routes ===")
  (for-each
   (lambda (rt)
     (displayln
      (format "~a ~a ~a"
              (route-method rt)
              (route-pattern rt)
              (if (null? (route-params rt))
                  ""
                  (format "(params: ~a)" (route-params rt))))))
   (reverse (router-routes r)))
  (displayln "========================\n"))
