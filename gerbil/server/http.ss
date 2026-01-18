;;; server/http.ss - HTTP Server Foundation
;;;
;;; This module provides the basic HTTP server functionality using Gerbil's
;;; standard library :std/net/httpd

(export #t)

(import
  :std/net/httpd
  :std/net/request
  :std/text/json
  :std/sugar
  :std/misc/hash
  :std/format
  :std/logger)

;;; ============================================================================
;;; Server Configuration
;;; ============================================================================

(defstruct http-server-config
  (host          ; server host (default: "0.0.0.0")
   port          ; server port (default: 8283)
   max-connections ; max concurrent connections
   read-timeout  ; read timeout in seconds
   write-timeout ; write timeout in seconds
   handler)      ; request handler function
  transparent: #t)

(def (make-http-server-config-instance . rest
      (host "0.0.0.0")
      (port 8283)
      (max-connections 100)
      (read-timeout 30)
      (write-timeout 30)
      (handler #f))
  "Create HTTP server configuration"
  (make-http-server-config
   host: host
   port: port
   max-connections: max-connections
   read-timeout: read-timeout
   write-timeout: write-timeout
   handler: handler))

;;; ============================================================================
;;; Request/Response Structures
;;; ============================================================================

(defstruct http-request
  (method        ; HTTP method (:GET :POST :PUT :DELETE :PATCH)
   path          ; request path
   query-params  ; query parameters (make-hash-table)
   headers       ; request headers (make-hash-table)
   body          ; request body (string or #f)
   json          ; parsed JSON body (make-hash-table)
   params)       ; path parameters (make-hash-table)
  transparent: #t)

(defstruct http-response
  (status        ; HTTP status code
   headers       ; response headers (make-hash-table)
   body)         ; response body (string)
  transparent: #t)

;;; ============================================================================
;;; Request Parsing
;;; ============================================================================

(def (parse-http-request req)
  "Parse raw HTTP request into http-request structure"
  (let* ((method (string->symbol (string-upcase (request-method req))))
         (path (request-path req))
         (query-params (parse-query-string (request-query req)))
         (headers (parse-headers (request-headers req)))
         (body (request-body req))
         (json (and body
                    (equal? (hash-ref headers "content-type" "") "application/json")
                    (try (json-object<-string body)
                         (catch (e) #f)))))

    (make-http-request
     method: method
     path: path
     query-params: query-params
     headers: headers
     body: body
     json: json
     params: (make-hash-table))))

(def (parse-query-string query-str)
  "Parse query string into hash"
  (if (or (not query-str) (equal? query-str ""))
      (make-hash-table)
      (let ((pairs (string-split query-str #\&)))
        (list->hash
         (map (lambda (pair)
                (let ((kv (string-split pair #\=)))
                  (if (= (length kv) 2)
                      (cons (car kv) (cadr kv))
                      (cons (car kv) ""))))
              pairs)))))

(def (parse-headers headers)
  "Parse headers list into hash"
  (list->hash
   (map (lambda (h)
          (cons (string-downcase (car h)) (cdr h)))
        headers)))

;;; ============================================================================
;;; Response Building
;;; ============================================================================

(def (make-json-response data . rest (status 200))
  "Create JSON response"
  (make-http-response
   status: status
   headers: (make-hash-table)
   body: (json-object->string data)))

(def (make-text-response text . rest (status 200))
  "Create text response"
  (make-http-response
   status: status
   headers: (make-hash-table)
   body: text))

(def (make-html-response html . rest (status 200))
  "Create HTML response"
  (make-http-response
   status: status
   headers: (make-hash-table)
   body: html))

(def (make-error-response message . rest (status 500) (details #f))
  "Create error response"
  (make-json-response
   (if details
       (let ((ht (make-hash-table)))
  (hash-put! ht 'error message)
  (hash-put! ht 'details details)
  ht)
       (let ((ht (make-hash-table)))
  (hash-put! ht 'error message)
  ht))
   status: status))

;;; ============================================================================
;;; Response Serialization
;;; ============================================================================

(def (serialize-http-response response)
  "Convert http-response to raw HTTP response"
  (let* ((status (http-response-status response))
         (headers (http-response-headers response))
         (body (http-response-body response))
         (header-list (hash->list headers)))

    ;; Build HTTP response
    (http-response-write
     status: status
     headers: header-list
     body: body)))

;;; ============================================================================
;;; Server Management
;;; ============================================================================

(defstruct http-server
  (config        ; http-server-config
   httpd         ; httpd instance
   running?)     ; server running status
  transparent: #t)

(def (start-http-server! config)
  "Start HTTP server with given configuration"
  (let* ((host (http-server-config-host config))
         (port (http-server-config-port config))
         (handler (http-server-config-handler config))

         ;; Create httpd instance
         (httpd (start-httpd!
                 host: host
                 port: port
                 handler: (lambda (req)
                            (handle-request handler req)))))

    (displayln (format "HTTP server started on ~a:~a" host port))

    (make-http-server
     config: config
     httpd: httpd
     running?: #t)))

(def (stop-http-server! server)
  "Stop HTTP server"
  (when (http-server-running? server)
    (stop-httpd! (http-server-httpd server))
    (set! (http-server-running? server) #f)
    (displayln "HTTP server stopped")))

;;; ============================================================================
;;; Request Handling
;;; ============================================================================

(def (handle-request handler req)
  "Handle HTTP request with error handling"
  (try
   (let* ((parsed-req (parse-http-request req))
          (response (handler parsed-req)))

     ;; Serialize and return response
     (serialize-http-response response))

   (catch (e)
     ;; Error handling
     (errorf "Request handling error: ~a" e)
     (serialize-http-response
      (make-error-response
       "Internal server error"
       status: 500
       details: (error-message e))))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def (get-request-header req header-name)
  "Get request header value"
  (hash-ref (http-request-headers req)
            (string-downcase header-name)
            #f))

(def (get-query-param req param-name)
  "Get query parameter value"
  (hash-ref (http-request-query-params req) param-name #f))

(def (get-path-param req param-name)
  "Get path parameter value"
  (hash-ref (http-request-params req) param-name #f))

(def (request-json? req)
  "Check if request has JSON content type"
  (let ((content-type (get-request-header req "content-type")))
    (and content-type
         (string-contains content-type "application/json"))))

;;; ============================================================================
;;; Example Usage (commented out)
;;; ============================================================================

#|
;; Simple HTTP server
(def (my-handler req)
  (case (http-request-method req)
    ((:GET)
     (make-json-response (let ((ht (make-hash-table)))
  (hash-put! ht 'message "Hello, World!")
  ht)))

    ((:POST)
     (if (http-request-json req)
         (make-json-response
          (let ((ht (make-hash-table)))
  (hash-put! ht 'received (http-request-json req))
  ht))
         (make-error-response "Expected JSON body" status: 400)))

    (else
     (make-error-response "Method not allowed" status: 405))))

(def config (make-http-server-config-instance
             host: "0.0.0.0"
             port: 8283
             handler: my-handler))

(def server (start-http-server! config))

;; Stop server when done
;; (stop-http-server! server)
|#
