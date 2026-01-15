# Gerbil Message Streaming

Server-Sent Events (SSE) implementation for streaming message responses in Project O.

## Overview

The message streaming module provides:
- **Server-Sent Events (SSE)** - Real-time streaming to clients
- **Chunk Buffering** - Efficient chunk aggregation
- **Stream Lifecycle** - Start, pause, resume, abort operations
- **LLM Integration** - Stream LLM responses with tool calls
- **HTTP Handlers** - Ready-to-use SSE HTTP handlers
- **Stream Management** - Track and manage active streams

## Architecture

```
Client (Browser/App)
    ↓ (HTTP SSE Connection)
HTTP Server
    ↓
message/stream.ss (SSE Handler)
    ↓
message/manager.ss (Message Manager)
    ↓
database/messages.ss (Persistence)
    ↓
PostgreSQL Database
```

## Installation

No installation required - part of Project O's Gerbil codebase.

## Usage

### Creating a Stream

```scheme
(import :gerbil/message/stream
        :gerbil/message/manager
        :gerbil/database/client)

;; Connect to database
(db-connect!)

;; Create message manager
(def manager (make-manager agent-id))

;; Create stream with callbacks
(def stream (make-stream agent-id manager
                         buffer-size: 10
                         on-chunk: (lambda (chunk)
                                    (displayln (format "Chunk: ~a" chunk)))
                         on-complete: (lambda ()
                                       (displayln "Stream complete"))
                         on-error: (lambda (error)
                                    (displayln (format "Error: ~a" error)))))
```

### Basic Streaming

```scheme
;; Start streaming
(stream-start! stream)

;; Send chunks
(stream-send-chunk! stream "Hello ")
(stream-send-chunk! stream "world!")
(stream-send-chunk! stream " How are you?")

;; Flush buffer (sends accumulated chunks)
(stream-flush-buffer! stream)

;; Complete stream
(stream-complete! stream)
```

### Server-Sent Events (SSE)

```scheme
;; Format SSE event
(def event (sse-format-event
            (hash 'message "Hello" 'timestamp 1705401600)
            event: "message"
            id: "123"
            retry: 3000))

;; Output:
;; event: message
;; id: 123
;; retry: 3000
;; data: {"message":"Hello","timestamp":1705401600}
;;
;; (blank line terminates event)

;; Send chunk via SSE
(def sse-chunk (sse-send-chunk stream "Hello world!"))

;; Send completion event
(def sse-complete (sse-send-complete stream))

;; Send error event
(def sse-error (sse-send-error stream "Connection failed"))
```

### Stream Lifecycle

```scheme
;; Start stream
(stream-start! stream)

;; Check if active
(if (stream-active? stream)
    (displayln "Stream is active")
    (displayln "Stream is inactive"))

;; Pause stream
(stream-pause! stream)

;; Resume stream
(stream-resume! stream)

;; Abort stream with reason
(stream-abort! stream "User cancelled")

;; Close stream
(stream-close! stream)
```

### Chunk Buffering

```scheme
;; Buffer chunks (automatically flushes when full)
(stream-buffer-chunk! stream "Hello ")
(stream-buffer-chunk! stream "world!")

;; Get current buffer
(def buffer (stream-get-buffer stream))
(displayln (format "Buffer: ~a" buffer))

;; Flush buffer manually
(stream-flush-buffer! stream)

;; Clear buffer without flushing
(stream-clear-buffer! stream)
```

### LLM Streaming Integration

```scheme
;; Stream LLM response chunks
(def llm-chunks (list "Hello" " " "world" "!" " How" " can" " I" " help?"))

(def result (stream-llm-response stream llm-chunks
                                 prompt-tokens: 10
                                 completion-tokens: 20))

;; Result: "Hello world! How can I help?"
;; Also creates assistant message in database

;; Stream LLM response with tool calls
(def tool-calls (list
                 (hash 'id "call-123"
                       'type "function"
                       'function (hash 'name "search"
                                      'arguments "{\"query\":\"test\"}"))))

(def result (stream-llm-with-tools stream llm-chunks tool-calls
                                   prompt-tokens: 15
                                   completion-tokens: 25))
```

### HTTP SSE Handler

```scheme
(import :gerbil/server/http
        :gerbil/message/stream)

;; Create SSE handler
(def handler (create-sse-handler
              (lambda (stream)
                ;; Send initial message
                (stream-send-chunk! stream "Starting processing...")

                ;; Simulate work
                (stream-send-chunk! stream "Step 1 complete")
                (stream-send-chunk! stream "Step 2 complete")
                (stream-send-chunk! stream "Step 3 complete")

                ;; Complete stream
                (stream-complete! stream))))

;; Register handler with HTTP server
(post! router "/v1/agents/:agent_id/stream" handler)
```

### Stream Management

```scheme
;; Get stream by ID
(def stream (get-stream stream-id))

;; Get all active streams
(def active (get-active-streams))
(displayln (format "Active streams: ~a" (length active)))

;; Count active streams
(def count (count-active-streams))

;; Close all streams
(close-all-streams!)

;; Cleanup inactive streams
(def cleaned (cleanup-inactive-streams!))
(displayln (format "Cleaned up ~a inactive streams" cleaned))
```

### Stream Statistics

```scheme
;; Get stream statistics
(def stats (stream-get-stats stream))

(displayln (format "Stream ID: ~a" (hash-ref stats 'stream_id)))
(displayln (format "Agent ID: ~a" (hash-ref stats 'agent_id)))
(displayln (format "Active: ~a" (hash-ref stats 'active)))
(displayln (format "Buffer size: ~a" (hash-ref stats 'buffer_size)))
(displayln (format "Max buffer: ~a" (hash-ref stats 'max_buffer_size)))
```

## API Reference

### Stream Creation

- `(make-stream agent-id manager #!key ...)` - Create new stream
  - `buffer-size` - Maximum buffer size (default: 10)
  - `on-chunk` - Chunk callback function
  - `on-complete` - Completion callback function
  - `on-error` - Error callback function

### Stream State

- `(stream-active? stream)` - Check if stream is active
- `(stream-close! stream)` - Close stream
- `(get-stream stream-id)` - Get stream by ID

### SSE Formatting

- `(sse-format-event data #!key ...)` - Format data as SSE event
  - `event` - Event type
  - `id` - Event ID
  - `retry` - Retry interval (milliseconds)

- `(sse-send-chunk stream chunk)` - Send chunk via SSE
- `(sse-send-complete stream)` - Send completion event
- `(sse-send-error stream error-msg)` - Send error event

### Chunk Buffering

- `(stream-buffer-chunk! stream chunk)` - Add chunk to buffer
- `(stream-flush-buffer! stream)` - Flush buffered chunks
- `(stream-get-buffer stream)` - Get current buffer contents
- `(stream-clear-buffer! stream)` - Clear buffer without flushing

### Stream Lifecycle

- `(stream-start! stream)` - Start streaming
- `(stream-pause! stream)` - Pause streaming
- `(stream-resume! stream)` - Resume streaming
- `(stream-abort! stream #!optional reason)` - Abort streaming

### Message Streaming

- `(stream-create-message! stream role content #!key ...)` - Create and stream message
- `(stream-send-chunk! stream content)` - Send content chunk
- `(stream-send-chunks! stream chunks)` - Send multiple chunks
- `(stream-complete! stream #!optional final-message)` - Complete streaming

### LLM Integration

- `(stream-llm-response stream llm-chunks #!key ...)` - Stream LLM response
  - `prompt-tokens` - Prompt token count
  - `completion-tokens` - Completion token count

- `(stream-llm-with-tools stream llm-chunks tool-calls #!key ...)` - Stream LLM with tools
  - `prompt-tokens` - Prompt token count
  - `completion-tokens` - Completion token count

### HTTP Handlers

- `(sse-response-headers)` - Get SSE response headers
- `(create-sse-handler stream-fn)` - Create SSE HTTP handler

### Stream Management

- `(get-active-streams)` - Get all active streams
- `(count-active-streams)` - Count active streams
- `(close-all-streams!)` - Close all active streams
- `(cleanup-inactive-streams!)` - Remove inactive streams

### Stream Utilities

- `(stream-get-stats stream)` - Get stream statistics

## Data Structures

### Message Stream

```scheme
(defstruct message-stream
  (id              ; Stream ID (UUID)
   agent-id        ; Agent ID
   manager         ; Message manager
   active?         ; Is stream active?
   buffer          ; Chunk buffer (list)
   buffer-size     ; Maximum buffer size
   on-chunk        ; Chunk callback
   on-complete     ; Completion callback
   on-error)       ; Error callback
  transparent: #t)
```

### SSE Event Format

```
event: message
id: 123
retry: 3000
data: {"type":"chunk","content":"Hello"}

```

## Features

### Server-Sent Events (SSE)

SSE provides real-time streaming from server to client:

- **Unidirectional** - Server pushes data to client
- **HTTP-based** - Works over standard HTTP
- **Automatic Reconnection** - Browser automatically reconnects
- **Event Types** - Support for different event types
- **Event IDs** - Track events for replay

### Chunk Buffering

Efficient chunk aggregation:

- **Automatic Flushing** - Flushes when buffer is full
- **Manual Control** - Flush or clear buffer manually
- **Configurable Size** - Set buffer size per stream
- **Memory Efficient** - Prevents excessive small writes

### Stream Lifecycle

Complete stream management:

- **Start/Stop** - Control stream execution
- **Pause/Resume** - Temporarily pause streaming
- **Abort** - Cancel stream with reason
- **Callbacks** - React to stream events

### LLM Integration

Seamless LLM streaming:

- **Chunk Accumulation** - Accumulate chunks into full response
- **Tool Call Support** - Stream responses with tool calls
- **Token Tracking** - Track prompt and completion tokens
- **Database Persistence** - Automatically save messages

## Client-Side Usage

### JavaScript/Browser

```javascript
// Connect to SSE endpoint
const eventSource = new EventSource('/v1/agents/agent-123/stream');

// Listen for messages
eventSource.addEventListener('message', (event) => {
  const data = JSON.parse(event.data);
  console.log('Chunk:', data.content);
});

// Listen for completion
eventSource.addEventListener('complete', (event) => {
  console.log('Stream complete');
  eventSource.close();
});

// Listen for errors
eventSource.addEventListener('error', (event) => {
  const data = JSON.parse(event.data);
  console.error('Error:', data.error);
  eventSource.close();
});

// Handle connection errors
eventSource.onerror = (error) => {
  console.error('Connection error:', error);
  eventSource.close();
};
```

### Python

```python
import requests
import json

# Connect to SSE endpoint
response = requests.get(
    'http://localhost:8283/v1/agents/agent-123/stream',
    stream=True
)

# Process events
for line in response.iter_lines():
    if line:
        line = line.decode('utf-8')

        # Parse SSE format
        if line.startswith('data: '):
            data = json.loads(line[6:])

            if data['type'] == 'chunk':
                print(f"Chunk: {data['content']}")
            elif data['type'] == 'complete':
                print("Stream complete")
                break
            elif data['type'] == 'error':
                print(f"Error: {data['error']}")
                break
```

## Performance

### Buffering Strategy

- **Default buffer size**: 10 chunks
- **Automatic flushing**: When buffer is full
- **Manual control**: Flush or clear anytime
- **Memory efficient**: Prevents excessive allocations

### Stream Management

- **Active stream tracking**: Hash table lookup
- **Automatic cleanup**: Remove inactive streams
- **Resource limits**: Configurable per stream
- **Connection pooling**: Via database client

### Optimization Tips

1. **Set Appropriate Buffer Size**: Balance latency vs throughput
2. **Flush Strategically**: Flush at natural boundaries
3. **Cleanup Inactive Streams**: Prevent memory leaks
4. **Use Callbacks**: React to events efficiently
5. **Monitor Active Streams**: Track resource usage

## Error Handling

All operations may throw errors. Use `try/catch` for error handling:

```scheme
(try
 (stream-send-chunk! stream "Hello")
 (catch (e)
   (stream-abort! stream (error-message e))
   (displayln (format "Error: ~a" (error-message e)))))
```

## Integration

### With Message Manager

```scheme
(import :gerbil/message/manager
        :gerbil/message/stream)

;; Create manager and stream
(def manager (make-manager agent-id))
(def stream (make-stream agent-id manager))

;; Stream uses manager for message persistence
(stream-create-message! stream "user" "Hello!")
```

### With LLM Clients

```scheme
(import :gerbil/llm/client
        :gerbil/message/stream)

;; Get LLM streaming response
(def llm-stream (llm-chat-completion-stream :openai "gpt-4" messages))

;; Stream to client
(stream-llm-response stream llm-stream
                     prompt-tokens: 100
                     completion-tokens: 200)
```

### With HTTP Server

```scheme
(import :gerbil/server/http
        :gerbil/server/router
        :gerbil/message/stream)

;; Create SSE endpoint
(def router (make-router-instance))

(post! router "/v1/agents/:agent_id/stream"
       (create-sse-handler
        (lambda (stream)
          ;; Your streaming logic here
          (stream-send-chunk! stream "Processing...")
          (stream-complete! stream))))

;; Start server
(start-http-server! (make-http-server-config
                     host: "0.0.0.0"
                     port: 8283
                     handler: (router-handler router)))
```

## Testing

```scheme
;; Test stream creation
(def stream (make-stream agent-id manager))
(assert (stream-active? stream))

;; Test chunk buffering
(stream-buffer-chunk! stream "Hello")
(stream-buffer-chunk! stream " world")
(def buffer (stream-get-buffer stream))
(assert (= (length buffer) 2))

;; Test SSE formatting
(def event (sse-format-event (hash 'message "Hello") event: "test"))
(assert (string-contains event "event: test"))
(assert (string-contains event "data: "))

;; Test stream lifecycle
(stream-pause! stream)
(assert (not (stream-active? stream)))
(stream-resume! stream)
(assert (stream-active? stream))
```

## Future Enhancements

- [ ] WebSocket support
- [ ] Binary streaming
- [ ] Stream compression
- [ ] Stream multiplexing
- [ ] Backpressure handling
- [ ] Stream prioritization
- [ ] Stream recording/replay
- [ ] Stream analytics

## License

Part of Project O - Self-Evolving AI Agent Platform
