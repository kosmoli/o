# HTTP Server

HTTP server framework for Project O, built on Gerbil's `:std/net/httpd` with routing, middleware, and health check support.

## Features

- ✅ HTTP server with configurable host/port
- ✅ Path-based routing with parameter extraction
- ✅ Middleware system (logging, CORS, error handling)
- ✅ Health check endpoints
- ✅ JSON request/response handling
- ✅ Route grouping and prefixes
- ✅ Request validation
- ✅ Rate limiting
- ✅ Prometheus metrics (basic)

## Quick Start

### Simple Server

```scheme
(import :gerbil/server/http)

;; Define handler
(def (my-handler req)
  (make-json-response (hash 'message "Hello, World!")))

;; Create config
(def config (make-http-server-config-instance
             host: "0.0.0.0"
             port: 8283
             handler: my-handler))

;; Start server
(def server (start-http-server! config))

;; Stop server when done
(stop-http-server! server)
```

### With Router

```scheme
(import :gerbil/server/http :gerbil/server/router)

;; Create router
(def router (make-router-instance))

;; Register routes
(get! router "/" (lambda (req)
                   (make-json-response (hash 'message "Home"))))

(get! router "/users/:id" (lambda (req)
                            (let ((id (get-path-param req 'id)))
                              (make-json-response (hash 'user_id id)))))

(post! router "/users" (lambda (req)
                         (make-json-response
                          (hash 'created (http-request-json req))
                          status: 201)))

;; Create server with router
(def config (make-http-server-config-instance
             handler: (make-router-handler router)))

(def server (start-http-server! config))
```

### With Middleware

```scheme
(import :gerbil/server/http
        :gerbil/server/router
        :gerbil/server/middleware)

;; Create router
(def router (make-router-instance))
(get! router "/" (lambda (req) (make-json-response (hash 'ok #t))))

;; Apply middleware
(def handler ((standard-middleware-stack
               enable-logging: #t
               enable-cors: #t
               enable-error-handling: #t)
              (make-router-handler router)))

;; Start server
(def config (make-http-server-config-instance handler: handler))
(def server (start-http-server! config))
```

### Complete Example

See `gerbil/server/example.ss` for a complete working example with:
- Multiple routes
- Health check endpoints
- Middleware stack
- Route grouping
- Error handling

Run the example:
```bash
gxi gerbil/server/example.ss
```

## API Reference

### HTTP Server (`http.ss`)

#### Server Configuration

```scheme
(make-http-server-config-instance
  #!key
  (host "0.0.0.0")
  (port 8283)
  (max-connections 100)
  (read-timeout 30)
  (write-timeout 30)
  (handler #f))
```

#### Server Management

```scheme
;; Start server
(start-http-server! config) => http-server

;; Stop server
(stop-http-server! server)
```

#### Request Structure

```scheme
(defstruct http-request
  (method        ; :GET :POST :PUT :DELETE :PATCH
   path          ; "/users/123"
   query-params  ; (hash "name" "Alice")
   headers       ; (hash "content-type" "application/json")
   body          ; "raw body string"
   json          ; parsed JSON (hash)
   params)       ; path params (hash 'id "123")
  transparent: #t)
```

#### Response Builders

```scheme
;; JSON response
(make-json-response data #!key (status 200))

;; Text response
(make-text-response text #!key (status 200))

;; HTML response
(make-html-response html #!key (status 200))

;; Error response
(make-error-response message #!key (status 500) (details #f))
```

#### Request Utilities

```scheme
;; Get header
(get-request-header req "content-type")

;; Get query parameter
(get-query-param req "name")

;; Get path parameter
(get-path-param req 'id)

;; Check if JSON request
(request-json? req)
```

### Router (`router.ss`)

#### Router Creation

```scheme
;; Create router
(make-router-instance
  #!key
  (not-found default-not-found-handler)
  (method-not-allowed default-method-not-allowed-handler))
```

#### Route Registration

```scheme
;; Register routes
(get! router "/path" handler)
(post! router "/path" handler)
(put! router "/path" handler)
(delete! router "/path" handler)
(patch! router "/path" handler)
(any! router "/path" handler)  ; Any HTTP method
```

#### Path Parameters

```scheme
;; Route with parameters
(get! router "/users/:id/posts/:post_id" handler)

;; Extract parameters in handler
(def (handler req)
  (let ((user-id (get-path-param req 'id))
        (post-id (get-path-param req 'post_id)))
    (make-json-response (hash 'user_id user-id 'post_id post-id))))
```

#### Route Groups

```scheme
;; Group routes under prefix
(group-routes! router "/api/v1"
  (lambda (r)
    (get! r "/users" list-users-handler)
    (get! r "/users/:id" get-user-handler)
    (post! r "/users" create-user-handler)))

;; Results in:
;; GET  /api/v1/users
;; GET  /api/v1/users/:id
;; POST /api/v1/users
```

#### Router Utilities

```scheme
;; List all routes
(list-routes router)

;; Print routes (for debugging)
(print-routes router)

;; Create handler from router
(make-router-handler router)
```

### Middleware (`middleware.ss`)

#### Logging Middleware

```scheme
;; Basic logging
(logging-middleware handler)

;; Detailed logging
(detailed-logging-middleware handler)
```

#### Error Handling

```scheme
;; General error handling
(error-handling-middleware handler)

;; Validation error handling
(validation-error-middleware handler)
```

#### CORS

```scheme
;; CORS middleware
((cors-middleware
  allowed-origins: '("*")
  allowed-methods: '("GET" "POST" "PUT" "DELETE")
  allowed-headers: '("Content-Type" "Authorization")
  max-age: 86400)
 handler)
```

#### Request Validation

```scheme
;; Require JSON body
(json-body-required-middleware handler)

;; Validate content type
((content-type-validation-middleware "application/json") handler)
```

#### Rate Limiting

```scheme
;; Create rate limiter
(def limiter (make-rate-limiter-instance
              max-requests: 100
              window-size: 60))

;; Apply rate limiting
((rate-limiting-middleware limiter) handler)
```

#### Request ID

```scheme
;; Add unique request ID
(request-id-middleware handler)
```

#### Middleware Composition

```scheme
;; Compose multiple middlewares
(def composed ((compose-middleware
                logging-middleware
                error-handling-middleware
                (cors-middleware))
               handler))

;; Apply middlewares
(def protected (apply-middlewares handler
                                  logging-middleware
                                  error-handling-middleware))

;; Standard middleware stack
(def standard ((standard-middleware-stack
                enable-logging: #t
                enable-cors: #t
                enable-error-handling: #t
                enable-request-id: #t)
               handler))
```

### Health Checks (`health.ss`)

#### Health Check Endpoints

```scheme
;; Basic health check
(get! router "/health" health-check-handler)

;; Detailed status
(get! router "/status" status-handler)

;; Readiness check (Kubernetes)
(get! router "/ready" readiness-handler)

;; Liveness check (Kubernetes)
(get! router "/alive" liveness-handler)

;; Prometheus metrics
(get! router "/metrics" metrics-handler)

;; Version info
(get! router "/version" version-handler)
```

#### Health Check Responses

**GET /health**
```json
{
  "status": "ok",
  "timestamp": 1705401600,
  "uptime": 3600
}
```

**GET /status**
```json
{
  "status": "ok",
  "timestamp": 1705401600,
  "uptime": 3600,
  "version": "0.1.0",
  "environment": {
    "gerbil_version": "v0.18",
    "platform": "unix"
  },
  "system": {
    "memory_usage": "N/A",
    "cpu_usage": "N/A",
    "active_connections": 0
  }
}
```

**GET /metrics** (Prometheus format)
```
# HELP server_uptime_seconds Server uptime in seconds
# TYPE server_uptime_seconds gauge
server_uptime_seconds 3600

# HELP http_requests_total Total HTTP requests
# TYPE http_requests_total counter
http_requests_total 1234
```

## Examples

### REST API Example

```scheme
(import :gerbil/server/http
        :gerbil/server/router
        :gerbil/server/middleware)

;; User handlers
(def (list-users req)
  (make-json-response
   (hash 'users (list
                 (hash 'id 1 'name "Alice")
                 (hash 'id 2 'name "Bob")))))

(def (get-user req)
  (let ((id (get-path-param req 'id)))
    (make-json-response
     (hash 'user (hash 'id id 'name (format "User ~a" id))))))

(def (create-user req)
  (if (http-request-json req)
      (make-json-response
       (hash 'created #t 'user (http-request-json req))
       status: 201)
      (make-error-response "Expected JSON" status: 400)))

(def (update-user req)
  (let ((id (get-path-param req 'id)))
    (if (http-request-json req)
        (make-json-response
         (hash 'updated #t 'id id 'user (http-request-json req)))
        (make-error-response "Expected JSON" status: 400))))

(def (delete-user req)
  (let ((id (get-path-param req 'id)))
    (make-json-response
     (hash 'deleted #t 'id id))))

;; Setup router
(def router (make-router-instance))

(get! router "/users" list-users)
(get! router "/users/:id" get-user)
(post! router "/users" create-user)
(put! router "/users/:id" update-user)
(delete! router "/users/:id" delete-user)

;; Apply middleware and start server
(def handler ((standard-middleware-stack) (make-router-handler router)))
(def config (make-http-server-config-instance handler: handler))
(def server (start-http-server! config))
```

### API Versioning

```scheme
;; Create router
(def router (make-router-instance))

;; API v1
(group-routes! router "/api/v1"
  (lambda (r)
    (get! r "/users" list-users-v1)
    (get! r "/posts" list-posts-v1)))

;; API v2
(group-routes! router "/api/v2"
  (lambda (r)
    (get! r "/users" list-users-v2)
    (get! r "/posts" list-posts-v2)))
```

### Custom Middleware

```scheme
;; Authentication middleware
(def (auth-middleware handler)
  (lambda (req)
    (let ((token (get-request-header req "authorization")))
      (if (and token (valid-token? token))
          (handler req)
          (make-error-response "Unauthorized" status: 401)))))

;; Apply to specific routes
(def protected-handler (auth-middleware my-handler))
```

## Testing

```bash
# Start example server
gxi gerbil/server/example.ss

# Test with curl
curl http://localhost:8283/health
curl http://localhost:8283/api/v1/hello
curl -X POST http://localhost:8283/api/v1/echo \
  -H "Content-Type: application/json" \
  -d '{"message": "test"}'
```

## Architecture

```
gerbil/server/
├── http.ss          # HTTP server foundation
├── router.ss        # Routing system
├── middleware.ss    # Middleware functions
├── health.ss        # Health check endpoints
├── example.ss       # Complete example
└── README.md        # This file
```

## Integration with Project O

This HTTP server will be used for:
- Agent management API (Phase 2, Week 4)
- Message endpoints (Phase 3)
- Memory operations (Phase 4)
- Tool execution (Phase 5)
- Evolution monitoring (Phase 6)

## Roadmap

- [x] Basic HTTP server
- [x] Routing system
- [x] Middleware framework
- [x] Health checks
- [x] CORS support
- [x] Error handling
- [x] Rate limiting
- [ ] WebSocket support
- [ ] Server-Sent Events (SSE)
- [ ] Request/response streaming
- [ ] Authentication/Authorization
- [ ] API documentation generation
- [ ] OpenAPI/Swagger support

## Contributing

This is part of Project O's Phase 2, Week 3 implementation. See `docs/REVISED_ROADMAP.md` for the full roadmap.

## License

Part of Project O - Self-Evolving AI Agent Platform
