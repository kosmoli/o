;;; server/example.ss - HTTP Server Example
;;;
;;; This example demonstrates how to use the HTTP server with routing,
;;; middleware, and health check endpoints.

(export #t)

(import
  :std/sugar
  :std/misc/hash
  :std/format
  :o/server/http
  :o/server/router
  :o/server/middleware
  :o/server/health)

;;; ============================================================================
;;; Example Handlers
;;; ============================================================================

(def (home-handler req)
  "Home page handler"
  (make-json-response
   (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Welcome to Project O HTTP Server")
  (hash-put! ht 'version "0.1.0")
  (hash-put! ht 'endpoints (list
                "/health"
                "/status"
                "/api/v1/hello"
                "/api/v1/echo"
                "/api/v1/users"
                "/api/v1/users/:id"))
  ht)))

(def (hello-handler req)
  "Simple hello handler"
  (let ((name (get-query-param req "name")))
    (make-json-response
     (let ((ht (make-hash-table)))
  (hash-put! ht 'message (if name
                   (format "Hello, ~a!" name))
                   "Hello, World!")
  ht))))

(def (echo-handler req)
  "Echo back the request body"
  (if (http-request-json req)
      (make-json-response
       (let ((ht (make-hash-table)))
  (hash-put! ht 'echo (http-request-json req))
  ht))
      (make-error-response
       "Expected JSON body"
       status: 400)))

(def (list-users-handler req)
  "List all users"
  (make-json-response
   (let ((ht (make-hash-table)))
  (hash-put! ht 'users (list
            (hash 'id 1 'name "Alice"))
            (hash ('id 2) ('name "Bob"))
            (hash ('id 3) ('name "Charlie")))
  ht)))

(def (get-user-handler req)
  "Get user by ID"
  (let ((id (get-path-param req 'id)))
    (make-json-response
     (let ((ht (make-hash-table)))
  (hash-put! ht 'user (hash 'id id 'name (format "User ~a" id)))
  ht))))

(def (create-user-handler req)
  "Create new user"
  (if (http-request-json req)
      (let ((user-data (http-request-json req)))
        (make-json-response
         (let ((ht (make-hash-table)))
  (hash-put! ht 'created #t)
  (hash-put! ht 'user user-data)
  ht)
         status: 201))
      (make-error-response
       "Expected JSON body"
       status: 400)))

;;; ============================================================================
;;; Router Setup
;;; ============================================================================

(def (setup-router)
  "Setup router with all routes"
  (let ((router (make-router-instance)))

    ;; Home
    (get! router "/" home-handler)

    ;; Health checks
    (get! router "/health" health-check-handler)
    (get! router "/status" status-handler)
    (get! router "/ready" readiness-handler)
    (get! router "/alive" liveness-handler)
    (get! router "/metrics" metrics-handler)
    (get! router "/version" version-handler)

    ;; API v1 routes
    (group-routes! router "/api/v1"
      (lambda (r)
        (get! r "/hello" hello-handler)
        (post! r "/echo" echo-handler)
        (get! r "/users" list-users-handler)
        (get! r "/users/:id" get-user-handler)
        (post! r "/users" create-user-handler)))

    router))

;;; ============================================================================
;;; Server Setup
;;; ============================================================================

(def (start-example-server . rest
      (host "0.0.0.0")
      (port 8283))
  "Start example HTTP server"

  ;; Setup router
  (let* ((router (setup-router))

         ;; Print routes
         (_ (print-routes router))

         ;; Create handler with middleware
         (handler ((standard-middleware-stack
                    enable-logging: #t
                    enable-cors: #t
                    enable-error-handling: #t
                    enable-request-id: #t)
                   (make-router-handler router)))

         ;; Create server config
         (config (make-http-server-config-instance
                  host: host
                  port: port
                  handler: handler))

         ;; Start server
         (server (start-http-server! config)))

    (displayln "\n╔════════════════════════════════════════════════════════════╗")
    (displayln "║         Project O HTTP Server                              ║")
    (displayln "╚════════════════════════════════════════════════════════════╝")
    (displayln (format "\nServer running on http://~a:~a" host port))
    (displayln "\nAvailable endpoints:")
    (displayln "  GET  /                    - Home page")
    (displayln "  GET  /health              - Health check")
    (displayln "  GET  /status              - Detailed status")
    (displayln "  GET  /ready               - Readiness check")
    (displayln "  GET  /alive               - Liveness check")
    (displayln "  GET  /metrics             - Prometheus metrics")
    (displayln "  GET  /version             - Version info")
    (displayln "  GET  /api/v1/hello        - Hello endpoint")
    (displayln "  POST /api/v1/echo         - Echo endpoint")
    (displayln "  GET  /api/v1/users        - List users")
    (displayln "  GET  /api/v1/users/'id    - Get user")
    (displayln "  POST /api/v1/users        - Create user")
    (displayln "\nPress Ctrl+C to stop the server\n")

    server))

;;; ============================================================================
;;; Main Entry Point
;;; ============================================================================

(def (main . args)
  "Main entry point"
  (let ((port (if (and (pair? args) (string? (car args)))
                  (string->number (car args))
                  8283)))

    (start-example-server port: port)

    ;; Keep server running
    (thread-sleep! +inf.0)))

;;; ============================================================================
;;; Example Usage
;;; ============================================================================

#|
;; Run the example server
gxi gerbil/server/example.ss

;; Or with custom port
gxi gerbil/server/example.ss 8080

;; Test endpoints with curl:

# Health check
curl http://localhost:8283/health

# Status
curl http://localhost:8283/status

# Hello
curl http://localhost:8283/api/v1/hello
curl http://localhost:8283/api/v1/hello?name=Alice

# Echo
curl -X POST http://localhost:8283/api/v1/echo \
  -H "Content-Type: application/json" \
  -d '{"message": "Hello!"}'

# List users
curl http://localhost:8283/api/v1/users

# Get user
curl http://localhost:8283/api/v1/users/123

# Create user
curl -X POST http://localhost:8283/api/v1/users \
  -H "Content-Type: application/json" \
  -d '{"name": "Alice", "email": "alice@example.com"}'
|#
