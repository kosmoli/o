;;; server/router.ss - HTTP Router
;;;
;;; This module provides routing functionality for HTTP requests,
;;; including path matching, parameter extraction, and route registration.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/srfi/13
  :std/format
  ./http)

;;; ============================================================================
;;; Route Structures
;;; ============================================================================

(defstruct route
  (method        ; HTTP method (:GET :POST :PUT :DELETE :PATCH :ANY)
   pattern       ; path pattern (e.g., "/users/:id")
   handler       ; handler function
   params)       ; parameter names extracted from pattern
  transparent: #t)

(defstruct router
  (routes        ; list of route structures
   not-found     ; 404 handler
   method-not-allowed) ; 405 handler
  transparent: #t)

;;; ============================================================================
;;; Router Creation
;;; ============================================================================

(def (make-router-instance
      #!key
      (not-found default-not-found-handler)
      (method-not-allowed default-method-not-allowed-handler))
  "Create a new router"
  (make-router
   routes: '()
   not-found: not-found
   method-not-allowed: method-not-allowed))

(def (default-not-found-handler req)
  "Default 404 handler"
  (make-error-response
   (format "Not found: ~a" (http-request-path req))
   status: 404))

(def (default-method-not-allowed-handler req)
  "Default 405 handler"
  (make-error-response
   (format "Method not allowed: ~a ~a"
           (http-request-method req)
           (http-request-path req))
   status: 405))

;;; ============================================================================
;;; Route Registration
;;; ============================================================================

(def (add-route! router method pattern handler)
  "Add a route to the router"
  (let* ((params (extract-params-from-pattern pattern))
         (route (make-route
                 method: method
                 pattern: pattern
                 handler: handler
                 params: params)))
    (set! (router-routes router)
          (cons route (router-routes router)))
    router))

(def (get! router pattern handler)
  "Register GET route"
  (add-route! router :GET pattern handler))

(def (post! router pattern handler)
  "Register POST route"
  (add-route! router :POST pattern handler))

(def (put! router pattern handler)
  "Register PUT route"
  (add-route! router :PUT pattern handler))

(def (delete! router pattern handler)
  "Register DELETE route"
  (add-route! router :DELETE pattern handler))

(def (patch! router pattern handler)
  "Register PATCH route"
  (add-route! router :PATCH pattern handler))

(def (any! router pattern handler)
  "Register route for any HTTP method"
  (add-route! router :ANY pattern handler))

;;; ============================================================================
;;; Pattern Matching
;;; ============================================================================

(def (extract-params-from-pattern pattern)
  "Extract parameter names from route pattern
   Example: '/users/:id/posts/:post_id' => '(id post_id)"
  (let ((segments (string-split pattern #\/)))
    (filter-map
     (lambda (seg)
       (if (and (> (string-length seg) 0)
                (char=? (string-ref seg 0) #\:))
           (string->symbol (substring seg 1 (string-length seg)))
           #f))
     segments)))

(def (pattern->regex pattern)
  "Convert route pattern to regex
   Example: '/users/:id' => '^/users/([^/]+)$'"
  (let* ((segments (string-split pattern #\/))
         (regex-segments
          (map (lambda (seg)
                 (if (and (> (string-length seg) 0)
                          (char=? (string-ref seg 0) #\:))
                     "([^/]+)"  ;; Match parameter
                     seg))       ;; Literal segment
               segments)))
    (string-append "^" (string-join regex-segments "/") "$")))

(def (match-route? route path)
  "Check if route pattern matches path"
  (let ((regex (pattern->regex (route-pattern route))))
    (regexp-match regex path)))

(def (extract-params route path)
  "Extract parameter values from path using route pattern"
  (let* ((regex (pattern->regex (route-pattern route)))
         (match (regexp-match regex path)))
    (if match
        (let ((param-values (cdr match))  ;; Skip full match
              (param-names (route-params route)))
          (list->hash
           (map cons param-names param-values)))
        (hash))))

;;; ============================================================================
;;; Route Matching
;;; ============================================================================

(def (find-matching-route router method path)
  "Find route that matches method and path"
  (let loop ((routes (router-routes router)))
    (if (null? routes)
        #f
        (let ((route (car routes)))
          (if (and (or (eq? (route-method route) :ANY)
                      (eq? (route-method route) method))
                   (match-route? route path))
              route
              (loop (cdr routes)))))))

(def (find-routes-for-path router path)
  "Find all routes that match path (regardless of method)"
  (filter (lambda (route)
            (match-route? route path))
          (router-routes router)))

;;; ============================================================================
;;; Request Routing
;;; ============================================================================

(def (route-request router req)
  "Route HTTP request to appropriate handler"
  (let* ((method (http-request-method req))
         (path (http-request-path req))
         (route (find-matching-route router method path)))

    (cond
     ;; Route found
     (route
      (let ((params (extract-params route path)))
        ;; Add path params to request
        (set! (http-request-params req) params)
        ;; Call handler
        ((route-handler route) req)))

     ;; Path exists but method not allowed
     ((not (null? (find-routes-for-path router path)))
      ((router-method-not-allowed router) req))

     ;; Not found
     (else
      ((router-not-found router) req)))))

(def (make-router-handler router)
  "Create request handler function from router"
  (lambda (req)
    (route-request router req)))

;;; ============================================================================
;;; Route Groups
;;; ============================================================================

(def (group-routes! router prefix routes-fn)
  "Group routes under a common prefix
   Example: (group-routes! router '/api/v1' (lambda (r) ...))"
  (let ((sub-router (make-router-instance)))
    ;; Register routes in sub-router
    (routes-fn sub-router)
    ;; Add all sub-router routes to main router with prefix
    (for-each
     (lambda (route)
       (let ((prefixed-pattern (string-append prefix (route-pattern route))))
         (add-route! router
                    (route-method route)
                    prefixed-pattern
                    (route-handler route))))
     (router-routes sub-router))
    router))

;;; ============================================================================
;;; Route Inspection
;;; ============================================================================

(def (list-routes router)
  "List all registered routes"
  (map (lambda (route)
         (hash
          'method (route-method route)
          'pattern (route-pattern route)
          'params (route-params route)))
       (router-routes router)))

(def (print-routes router)
  "Print all registered routes"
  (displayln "\n=== Registered Routes ===")
  (for-each
   (lambda (route)
     (displayln (format "~a ~a ~a"
                       (route-method route)
                       (route-pattern route)
                       (if (null? (route-params route))
                           ""
                           (format "(params: ~a)" (route-params route))))))
   (reverse (router-routes router)))
  (displayln "========================\n"))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Create router
(def router (make-router-instance))

;; Register routes
(get! router "/" (lambda (req)
                   (make-json-response (hash 'message "Welcome!"))))

(get! router "/users" (lambda (req)
                        (make-json-response (hash 'users '()))))

(get! router "/users/:id" (lambda (req)
                            (let ((id (get-path-param req 'id)))
                              (make-json-response (hash 'user_id id)))))

(post! router "/users" (lambda (req)
                         (if (http-request-json req)
                             (make-json-response
                              (hash 'created (http-request-json req))
                              status: 201)
                             (make-error-response "Expected JSON" status: 400))))

;; Group routes
(group-routes! router "/api/v1"
  (lambda (r)
    (get! r "/status" (lambda (req)
                        (make-json-response (hash 'status "ok"))))
    (get! r "/version" (lambda (req)
                         (make-json-response (hash 'version "1.0.0"))))))

;; Print routes
(print-routes router)

;; Create server with router
(def config (make-http-server-config-instance
             handler: (make-router-handler router)))

(def server (start-http-server! config))
|#
