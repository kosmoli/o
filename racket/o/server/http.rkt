#lang racket

;;; server/http.rkt - HTTP Server Foundation
;;;
;;; This module provides the basic HTTP server functionality including
;;; request/response structures, server configuration, and request handling.

(provide (struct-out http-server-config)
         make-http-server-config-instance
         (struct-out http-request)
         (struct-out http-response)
         parse-http-request
         parse-query-string
         parse-headers
         make-json-response
         make-text-response
         make-html-response
         make-error-response
         serialize-http-response
         (struct-out http-server)
         start-http-server!
         stop-http-server!
         handle-request
         get-request-header
         get-query-param
         get-path-param
         request-json?
         make-http-request)

(require racket/hash
        racket/format
        json
        racket/string
        racket/list)

;;; ============================================================================
;;; Server Configuration
;;; ============================================================================

(struct http-server-config
  (host              ; server host (default: "0.0.0.0")
   port              ; server port (default: 8283)
   max-connections   ; max concurrent connections
   read-timeout      ; read timeout in seconds
   write-timeout     ; write timeout in seconds
   handler)          ; request handler function
  #:transparent)

(define (make-http-server-config-instance
         #:host [host "0.0.0.0"]
         #:port [port 8283]
         #:max-connections [max-connections 100]
         #:read-timeout [read-timeout 30]
         #:write-timeout [write-timeout 30]
         #:handler [handler #f])
  "Create HTTP server configuration

   Args:
     host: Server host address
     port: Server port number
     max-connections: Maximum concurrent connections
     read-timeout: Read timeout in seconds
     write-timeout: Write timeout in seconds
     handler: Request handler function

   Returns:
     http-server-config instance"
  (http-server-config
   host
   port
   max-connections
   read-timeout
   write-timeout
   handler))

;;; ============================================================================
;;; Request/Response Structures
;;; ============================================================================

(struct http-request
  (method        ; HTTP method (symbol: 'GET 'POST 'PUT 'DELETE 'PATCH)
   path          ; request path
   query-params  ; query parameters (hash)
   headers       ; request headers (hash)
   body          ; request body (string or #f)
   json          ; parsed JSON body (hash or #f)
   params)       ; path parameters (hash)
  #:transparent
  #:mutable)

(struct http-response
  (status        ; HTTP status code
   headers       ; response headers (hash)
   body)         ; response body (string)
  #:transparent)

;;; ============================================================================
;;; Request Parsing
;;; ============================================================================

(define (parse-http-request raw-request)
  "Parse raw HTTP request into http-request structure

   Args:
     raw-request: Raw HTTP request (hashtable-like object)

   Returns:
     http-request instance with parsed data

   Note: This is a simplified implementation. In production, you would
   parse actual HTTP request data from a web server's request object"
  (let* ([method (if (hash? raw-request)
                     (hash-ref raw-request 'method 'GET)
                     'GET)]
         [path (if (hash? raw-request)
                   (hash-ref raw-request 'path "/")
                   "/")]
         [query-params (if (hash? raw-request)
                          (hash-ref raw-request 'query-params (hash))
                          (hash))]
         [headers (if (hash? raw-request)
                      (hash-ref raw-request 'headers (hash))
                      (hash))]
         [body (if (hash? raw-request)
                   (hash-ref raw-request 'body #f)
                   #f)]
         [content-type (hash-ref headers "content-type" "")]
         [json (and body
                    (string-contains? content-type "application/json")
                    (with-handlers ([exn:fail? (lambda (e) #f)])
                      (string->jsexpr body)))])

    (http-request
     (if (string? method) (string->symbol (string-upcase method)) method)
     path
     query-params
     headers
     body
     json
     (hash))))

(define (parse-query-string query-str)
  "Parse query string into hash

   Args:
     query-str: Query string (e.g., \"key1=value1&key2=value2\")

   Returns:
     Hash of query parameters

   Example:
     (parse-query-string \"foo=bar&baz=qux\")
     ; => #hash((\"baz\" . \"qux\") (\"foo\" . \"bar\"))"
  (if (or (not query-str) (equal? query-str ""))
      (hash)
      (let ([pairs (string-split query-str "&")])
        (for/fold ([h (hash)])
                  ([pair (in-list pairs)])
          (define kv (string-split pair "="))
          (if (= (length kv) 2)
              (hash-set h (first kv) (second kv))
              (hash-set h (first kv) ""))))))

(define (parse-headers headers-list)
  "Parse headers list into hash

   Args:
     headers-list: List of header pairs ((\"name\" . \"value\") ...)

   Returns:
     Hash of headers with lowercase keys

   Example:
     (parse-headers '((\"Content-Type\" . \"application/json\")))
     ; => #hash((\"content-type\" . \"application/json\"))"
  (if (hash? headers-list)
      headers-list
      (for/fold ([h (hash)])
                ([h-pair (in-list headers-list)])
        (hash-set h
                  (string-downcase (car h-pair))
                  (cdr h-pair)))))

;;; ============================================================================
;;; Response Building
;;; ============================================================================

(define (make-json-response data #:status [status 200] #:headers [extra-headers (hash)])
  "Create JSON response

   Args:
     data: Data to serialize as JSON
     status: HTTP status code (default: 200)
     extra-headers: Additional headers to include

   Returns:
     http-response instance with JSON content type

   Example:
     (make-json-response (hash 'message 'Hello' 'status 'ok'))"
  (define base-headers
    (hash "Content-Type" "application/json"
          "Cache-Control" "no-cache"))
  (define all-headers (hash-union base-headers extra-headers))
  (http-response status all-headers (jsexpr->string data)))

(define (make-text-response text #:status [status 200] #:headers [extra-headers (hash)])
  "Create text response

   Args:
     text: Plain text content
     status: HTTP status code (default: 200)
     extra-headers: Additional headers to include

   Returns:
     http-response instance with text/plain content type"
  (define base-headers
    (hash "Content-Type" "text/plain"
          "Cache-Control" "no-cache"))
  (define all-headers (hash-union base-headers extra-headers))
  (http-response status all-headers text))

(define (make-html-response html #:status [status 200] #:headers [extra-headers (hash)])
  "Create HTML response

   Args:
     html: HTML content
     status: HTTP status code (default: 200)
     extra-headers: Additional headers to include

   Returns:
     http-response instance with text/html content type"
  (define base-headers
    (hash "Content-Type" "text/html; charset=utf-8"
          "Cache-Control" "no-cache"))
  (define all-headers (hash-union base-headers extra-headers))
  (http-response status all-headers html))

(define (make-error-response message #:status [status 500] #:details [details #f])
  "Create error response

   Args:
     message: Error message
     status: HTTP status code (default: 500)
     details: Optional error details

   Returns:
     http-response instance with error information as JSON

   Example:
     (make-error-response \"Not found\" #:status 404)"
  (make-json-response
   (if details
       (hash 'error message
             'details details
             'timestamp (current-seconds))
       (hash 'error message
             'timestamp (current-seconds)))
   #:status status))

;;; ============================================================================
;;; Response Serialization
;;; ============================================================================

(define (serialize-http-response response)
  "Convert http-response to raw HTTP response format

   Args:
     response: http-response instance

   Returns:
     Hash suitable for web server response

   Example:
     (serialize-http-response
       (make-json-response (hash 'ok #t)))
     ; => #hash((status . 200) (headers . ...) (body . ...))"
  (hash 'status (http-response-status response)
        'headers (http-response-headers response)
        'body (http-response-body response)))

;;; ============================================================================
;;; Server Management
;;; ============================================================================

(struct http-server
  (server-config  ; http-server-config
   listener       ; TCP listener or server instance
   running?)      ; server running status
  #:transparent
  #:mutable)

(define (start-http-server! config)
  "Start HTTP server with given configuration

   Args:
     config: http-server-config instance

   Returns:
     http-server instance

   Note: This is a simplified implementation. In production, you would
   use a proper web server like 'web-server/servlet' or 'racket/http'"
  (define host (http-server-config-host config))
  (define port (http-server-config-port config))
  (define handler (http-server-config-handler config))

  (displayln (format "HTTP server configured on ~a:~a" host port))
  (displayln (format "Note: This is a placeholder implementation."))
  (displayln (format "Use 'web-server/servlet' or 'racket/http' for production."))

  ;; Create server instance (placeholder)
  (http-server
   config
   #f   ; Would be actual listener in production
   #t))  ; running?

(define (stop-http-server! server)
  "Stop HTTP server

   Args:
     server: http-server instance

   Note: This will stop the server and close all connections"
  (when (http-server-running? server)
    ;; Close listener (placeholder)
    (set-http-server-running?! server #f)
    (displayln "HTTP server stopped")))

;;; ============================================================================
;;; Request Handling
;;; ============================================================================

(define (handle-request handler req)
  "Handle HTTP request with error handling

   Args:
     handler: Request handler function
     req: Raw HTTP request

   Returns:
     Serialized HTTP response

   This function wraps the handler with error handling,
   ensuring errors are converted to proper error responses"
  (with-handlers* ([exn:fail?
                    (lambda (e)
                      ;; Log error
                      (displayln (format "Request handling error: ~a" (exn-message e)))
                      ;; Return error response
                      (serialize-http-response
                       (make-error-response
                        "Internal server error"
                        #:status 500
                        #:details (exn-message e))))])
    ;; Parse request
    (define parsed-req (parse-http-request req))

    ;; Call handler
    (define response (handler parsed-req))

    ;; Serialize and return response
    (serialize-http-response response)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (get-request-header req header-name)
  "Get request header value (case-insensitive)

   Args:
     req: http-request instance
     header-name: Header name to look up

   Returns:
     Header value or #f if not found

   Example:
     (get-request-header req \"Content-Type\")"
  (define headers (http-request-headers req))
  (define lower-name (string-downcase header-name))
  (for/first ([(key val) (in-hash headers)]
              #:when (string=? (string-downcase key) lower-name))
    val))

(define (get-query-param req param-name)
  "Get query parameter value

   Args:
     req: http-request instance
     param-name: Parameter name to look up

   Returns:
     Parameter value or #f if not found

   Example:
     (get-query-param req \"page\")"
  (hash-ref (http-request-query-params req) param-name #f))

(define (get-path-param req param-name)
  "Get path parameter value

   Args:
     req: http-request instance
     param-name: Parameter name to look up

   Returns:
     Parameter value or #f if not found

   Example:
     (get-path-param req \"id\")"
  (hash-ref (http-request-params req) param-name #f))

(define (request-json? req)
  "Check if request has JSON content type

   Args:
     req: http-request instance

   Returns:
     #t if content type is JSON, #f otherwise

   Example:
     (request-json? req)"
  (define content-type (get-request-header req "content-type"))
  (and content-type
       (string-contains? content-type "application/json")))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (hash-union h1 h2)
  "Union of two hashes (h2 values override h1)

   Args:
     h1: Base hash
     h2: Override hash

   Returns:
     Combined hash

   Example:
     (hash-union (hash 'a 1) (hash 'b 2))
     ; => #hash((a . 1) (b . 2))"
  (for/fold ([h h1])
            ([(key val) (in-hash h2)])
    (hash-set h key val)))

(define (make-http-request
         #:method [method 'GET]
         #:path [path "/"]
         #:query-params [query-params (hash)]
         #:headers [headers (hash)]
         #:json [json #f]
         #:params [params (hash)]
         #:body [body ""])
  "Create HTTP request

   Args:
     method: HTTP method (symbol or string)
     path: Request path
     query-params: Query parameters hash
     headers: Headers hash
     json: Parsed JSON body
     params: Path parameters hash
     body: Request body string

   Returns:
     http-request instance

   Example:
     (make-http-request
       #:method 'POST
       #:path \"/api/users\"
       #:json (hash 'name 'John'))"
  (http-request
   (if (string? method) (string->symbol (string-upcase method)) method)
   path
   query-params
   headers
   body
   json
   params))

;;; ============================================================================
;;; Example Usage
;;; ============================================================================
;;;
;;; ;; Define a request handler
;;; (define (my-handler req)
;;;   (case (http-request-method req)
;;;     [(GET)
;;;      (make-json-response (hash 'message "Hello, World!")
;;;                          #:status 200)]
;;;
;;;     [(POST)
;;;      (if (request-json? req)
;;;          (make-json-response
;;;           (hash 'received (http-request-json req))
;;;           #:status 200)
;;;          (make-error-response "Expected JSON body"
;;;                              #:status 400))]
;;;
;;;     [else
;;;      (make-error-response "Method not allowed"
;;;                          #:status 405)]))
;;;
;;; ;; Create server configuration
;;; (define config
;;;   (make-http-server-config-instance
;;;    #:host "127.0.0.1"
;;;    #:port 8080
;;;    #:handler my-handler))
;;;
;;; ;; Start server
;;; (define server (start-http-server! config))
;;;
;;; ;; Stop server when done
;;; ;; (stop-http-server! server)
;;;
;;; ;;; ============================================================================
