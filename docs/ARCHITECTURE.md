# O - Self-Evolving Agent System

**Project O** is a self-evolving AI Agent system built with Lisp metaprogramming capabilities. The core principle is that the Agent can modify its own code, learn from experience, and upgrade itself autonomously.

---

## Table of Contents

1. [Overview](#overview)
2. [System Architecture](#system-architecture)
3. [Gerbil Agent Core](#gerbil-agent-core)
4. [Zig Dynamic Library Specification](#zig-dynamic-library-specification)
5. [Rust Dynamic Library Specification](#rust-dynamic-library-specification)
6. [Self-Modification Safety Mechanisms](#self-modification-safety-mechanisms)
7. [Git Embedded Integration](#git-embedded-integration)
8. [Development Roadmap](#development-roadmap)
9. [Project Structure](#project-structure)

---

## Overview

### Vision

Create an AI Agent that can:
- **Learn** from its own performance and user feedback
- **Modify** its own code based on learnings
- **Compile** new components using external compilers (Zig, Rust)
- **Upgrade** itself safely with rollback capabilities
- **Evolve** continuously without human intervention

### Technology Stack

| Layer | Technology | Responsibility | Portion |
|-------|-----------|----------------|---------|
| **Meta Layer** | Gerbil Scheme | Self-modification engine, code generation | 15% |
| **Application Layer** | Gerbil Scheme | Agent logic, DSL, memory, tools | 60% |
| **Infrastructure Layer** | Zig | HTTP, WebSocket, databases, search | 15% |
| **Compute Layer** | Rust | Vector operations, ML inference, encryption | 8% |
| **Foundation Layer** | C Libraries | PostgreSQL, SQLite, OpenSSL | 2% |

### Why Gerbil over Racket?

| Feature | Gerbil | Racket |
|---------|--------|--------|
| Compiled Macros | ✅ (AOT) | ❌ (interpreted) |
| Performance | ⭐⭐⭐⭐ | ⭐⭐⭐ |
| C FFI | Native (Gambit) | Additional layer |
| Module System | Single-instance (fast) | Multi-instance (flexible) |
| Production Ready | ✅ | ⚠️ (Educational focus) |

---

## System Architecture

### Layered Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                            META LAYER                                   │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │                    Self-Modification Engine                       │  │
│  │  ┌────────────┐  ┌────────────┐  ┌────────────┐  ┌───────────┐  │  │
│  │  │ Code Gen   │  │ Evaluator  │  │ Validator  │  │ Rollback  │  │  │
│  │  │ (macros)   │  │ (eval)     │  │ (tests)    │  │ (git)     │  │  │
│  │  └────────────┘  └────────────┘  └────────────┘  └───────────┘  │  │
│  └──────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                         APPLICATION LAYER                               │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │                       Gerbil Agent Core                            │  │
│  │  ┌────────────┐  ┌────────────┐  ┌────────────┐  ┌───────────┐  │  │
│  │  │ Agent DSL  │  │ Memory     │  │ Tool       │  │ Workflow  │  │  │
│  │  │ Engine     │  │ System     │  │ Framework  │  │ Engine    │  │  │
│  │  └────────────┘  └────────────┘  └────────────┘  └───────────┘  │  │
│  │                                                                      │
│  │  Dynamic Module Loading: (reload-module) (load-dll)                 │
│  └──────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼ FFI / DLL
┌─────────────────────────────────────────────────────────────────────────┐
│                      INFRASTRUCTURE LAYER                               │
│  ┌───────────────────┐  ┌───────────────────┐  ┌─────────────────────┐│
│  │   Zig DLL         │  │   Rust DLL        │  │   C Libraries       ││
│  │ ┌───────────────┐ │  │ ┌───────────────┐ │  │ ┌─────────────────┐││
│  │ │ HTTP Client   │ │  │ │ Vector Ops    │ │  │ │ libpq (Postgres)│││
│  │ │ HTTP Server   │ │  │ │ Matrix Ops    │ │  │ │ libsqlite3      │││
│  │ │ WebSocket     │ │  │ │ Embeddings    │ │  │ │ libcurl         │││
│  │ │ JSON Parser   │ │  │ │ ML Inference  │ │  │ │ OpenSSL         │││
│  │ │ PostgreSQL    │ │  │ │ Encryption   │ │  │ │ libpqvector     │││
│  │ │ SQLite        │ │  │ │ Compression  │ │  │ │                  │││
│  │ │ Search Index  │ │  │ │              │ │  │ │                  │││
│  │ └───────────────┘ │  │ └───────────────┘ │  │ └─────────────────┘││
│  └───────────────────┘  └───────────────────┘  └─────────────────────┘│
│                                                                          │
│  Hot-reload: Agent modifies source → calls compiler → loads new DLL     │
└─────────────────────────────────────────────────────────────────────────┘
```

### Evolution Flow

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        AGENT SELF-EVOLUTION CYCLE                        │
└─────────────────────────────────────────────────────────────────────────┘

1. PERCEPTION Phase
   Agent monitors: performance metrics, error rates, user feedback
   → Identifies improvement opportunities
   → Generates evolution hypothesis

2. PLANNING Phase
   Agent analyzes: bottleneck root cause, modification scope
   → Selects target layer (Gerbil/Zig/Rust)
   → Generates implementation plan

3. GENERATION Phase
   ┌───────────────────────────────────────────────────────────────────┐
   │ Target          │ Method                                        │
   ├───────────────────────────────────────────────────────────────────┤
   │ DSL/Logic       │ Gerbil macros generate new code                │
   │ Memory Strategy │ Gerbil eval modifies existing code              │
   │ HTTP Client     │ Generate Zig code → zig build-lib             │
   │ Vector Search   │ Generate Rust code → cargo clib               │
   │ Database Schema │ Generate migration SQL                        │
   └───────────────────────────────────────────────────────────────────┘

4. COMPILATION Phase
   Gerbil:  (compile-file "module.ss")     → "module.so"
   Zig:     (system "zig build-lib http.zig -dynamic -femit-bin=libhttp.so")
   Rust:    (system "cargo clib --release")

5. TESTING Phase
   → Unit tests (auto-generated)
   → Integration tests
   → Performance benchmarks
   → Rollback if regression detected

6. DEPLOYMENT Phase
   → Git commit (embedded)
   → Hot-load new module
   → Graceful traffic switch
   → Monitor metrics

7. VALIDATION Phase
   → Compare new vs old metrics
   → Keep if improved
   → Rollback if degraded
   → Learn from outcome
```

---

## Gerbil Agent Core

### Module Structure

```
gerbil/
├── agent/
│   ├── core.ss              # Agent core definitions
│   ├── dsl.ss               # Domain Specific Language
│   ├── state.ss             # State management
│   ├── memory.ss            # Memory system
│   ├── tools.ss             # Tool calling framework
│   ├── workflow.ss          # Workflow orchestration
│   ├── llm.ss               # LLM integration
│   └── meta.ss              # Self-modification engine
├── ffi/
│   ├── zig-ffi.ss           # Zig FFI bindings
│   ├── rust-ffi.ss          # Rust FFI bindings
│   └── c-ffi.ss             # C library bindings
├── utils/
│   ├── json.ss              # JSON utilities
│   ├── logger.ss            # Logging system
│   ├── config.ss            # Configuration
│   └── metrics.ss           # Metrics collection
└── build.ss                 # Build system
```

### Core Data Structures

```gerbil
;; core.ss - Core Agent definitions

(export #t agent make-agent agent? agent-ref)

;; Agent structure
(defstruct agent
  (id:           string                    ; Unique identifier
   name:         string                    ; Human-readable name
   version:      string                    ; Semantic version
   state:        agent-state               ; Current state
   memory:       memory-store              ; Memory subsystem
   tools:        tool-registry             ; Available tools
   llm:          llm-client                ; LLM connection
   dsl:          dsl-environment           ; DSL interpreter
   meta:         meta-config               ; Self-modification config
   created:      timestamp                 ; Creation time
   modified:     timestamp                 ; Last modification
   metrics:      metrics-store))           ; Performance metrics

;; Agent states
(deftuple agent-state
  (status:        symbol)     ; idle | thinking | acting | evolving
   context:       hash-table) ; Current context
   history:       list)       ; Conversation history

;; Create new agent
(def (make-agent id: id name: name
                  llm: llm
                  tools: tools
                  meta: meta-config)
  (struct agent
          id: id
          name: name
          version: "0.1.0"
          state: (make-agent-state 'idle (make-hash-table) '())
          memory: (make-memory-store)
          tools: (make-tool-registry tools)
          llm: llm
          dsl: (make-dsl-environment)
          meta: meta-config
          created: (current-time)
          modified: (current-time)
          metrics: (make-metrics-store)))
```

### DSL (Domain Specific Language)

```gerbil
;; dsl.ss - Agent DSL for defining behaviors

(export #t defagent deftool defworkflow defmemory when->)

;; Macro: Define an agent with high-level syntax
(defrules (defagent name version: (ver "0.1.0") doc: (doc #f) . body)
  [(defagent name version: ver doc: doc . body)
   (begin
     (def (,(string->symbol (format "~a-process" name)) msg context)
       ;; Agent processing logic
       (eval '(let () . ,body)))

     ;; Register agent
     (register-agent! 'name
                       (make-agent id: (symbol->string 'name)
                                    name: (symbol->string 'name)
                                    version: ver
                                    doc: doc
                                    process: (,(string->symbol (format "~a-process" name)))))))])

;; Example usage:
;; (defagent my-assistant
;;   version: "1.0.0"
;;   doc: "A helpful assistant"
;;   (when (has-tool? "search")
;;     (search-tool (get-arg "query"))
;;     (llm-respond context: context)))

;; Macro: Define conditional behavior
(defrules (when-> condition . actions)
  [(when-> condition . actions)
   (if (eval '(condition))
       (begin . ,actions)
       (void))])

;; Macro: Define a tool
(defrules (deftool name params doc: (doc "") . body)
  [(deftool name params doc: doc . body)
   (begin
     (def (,(string->symbol (format "tool-~a" name)) . params)
       . ,body)
     (register-tool! 'name
                      (make-tool name: '(name)
                                params: 'params
                                doc: doc
                                fn: (,(string->symbol (format "tool-~a" name)))))))])
```

### Memory System

```gerbil
;; memory.ss - Memory subsystem with blocks and vector search

(export #t make-memory memory-add! memory-search memory-recall
         memory-block memory-block? block-text block-embedding)

;; Memory store structure
(defstruct memory-store
  (blocks:        (box '()))        ; List of memory blocks
  embeddings:    (box #f)        ; Vector embeddings (from DLL)
  block-size:    512             ; Max characters per block
  overlap:       50              ; Overlap between blocks
  vector-dim:    1536            ; Embedding dimension
  )

;; Memory block
(defstruct memory-block
  (id:           string          ; Unique ID
   text:          string          ; Block content
   embedding:     (or float[] #f) ; Vector embedding
   metadata:      hash-table      ; Additional metadata
   importance:    float           ; Importance score (0-1)
   access-count:  integer         ; Access frequency
   created:       timestamp))

;; Add content to memory
(def (memory-add! memory content
                       metadata: (metadata (make-hash-table))
                       importance: (importance 0.5))
  (let* ((blocks (split-into-blocks content
                                         (memory-store-block-size memory)
                                         (memory-store-overlap memory)))
         (new-blocks (map (lambda (text)
                           (make-memory-block
                            id: (generate-uuid)
                            text: text
                            embedding: #f  ; Will be computed
                            metadata: metadata
                            importance: importance
                            access-count: 0
                            created: (current-time)))
                         blocks)))
    ;; Compute embeddings via Zig/Rust DLL
    (let ((embeddings (vector-embed (map memory-block-text new-blocks))))
      (for-each (lambda (block emb)
                  (set! (memory-block-embedding block) emb))
                new-blocks
                embeddings))
    ;; Store blocks
    (set! (memory-store-blocks memory)
          (append (unbox (memory-store-blocks memory)) new-blocks))
    new-blocks))

;; Semantic search
(def (memory-search memory query top-k: (top-k 5))
  (let* ((query-embedding (vector-embed query))
         (blocks (unbox (memory-store-blocks memory)))
         (similarities (map (lambda (block)
                             (cons block
                                   (cosine-similarity query-embedding
                                                      (memory-block-embedding block))))
                           blocks))
         (sorted (sort similarities > (lambda (a b) (< (cdr a) (cdr b))))))
    (map car (take sorted top-k))))
```

### Tool Calling Framework

```gerbil
;; tools.ss - Tool calling framework

(export #t make-tool tool-registry register-tool! call-tool)

;; Tool structure
(defstruct tool
  (name:         symbol          ; Tool name
   params:       (list symbol)  ; Parameter names
   fn:           procedure      ; Implementation function
   doc:          string          ; Documentation
   unsafe:       bool            ; Whether tool requires sandbox
   timeout:      (or integer #f) ; Timeout in seconds
   )

;; Tool registry
(defstruct tool-registry
  (tools:        (box (make-hash-table))  ; Registered tools
   history:      (box '())                 ; Tool call history
   sandboxed:    (box #f)                 ; Sandboxed tools))

;; Register a tool
(def (register-tool! registry tool)
  (hash-set! (unbox (tool-registry-tools registry))
             (tool-name tool)
             tool))

;; Parse tool call from LLM response
(def (parse-tool-call response)
  ;; Parse: "(tool-name arg1: value1 arg2: value2)"
  (let* ((call-regex #rx"\\(([\\w-]+)\\s+([^)]+)\\)")
         (match (regexp-match call-regex response)))
    (when match
      (let* ((tool-name (string->symbol (cadr match)))
             (args-str (caddr match))
             (args (parse-args args-str)))
        (cons tool-name args)))))

;; Execute tool with safety checks
(def (call-tool registry tool-name . args)
  (let ((tool (hash-ref (unbox (tool-registry-tools registry)) tool-name)))
    (unless tool
      (error "Tool not found" tool: tool-name))

    ;; Check if sandbox needed
    (when (and (tool-unsafe tool)
               (not (unbox (tool-registry-sandboxed registry))))
      (error "Unsafe tool requires sandbox" tool: tool-name))

    ;; Apply with timeout
    (let ((result (apply (tool-fn tool) args)))
      ;; Record in history
      (set! (tool-registry-history registry)
            (cons (list tool-name args result (current-time))
                  (unbox (tool-registry-history registry))))
      result)))
```

### LLM Integration

```gerbil
;; llm.ss - LLM client with streaming and multiple providers

(export #t make-llm llm-chat llm-stream llm-embed)

;; LLM client structure
(defstruct llm-client
  (provider:      symbol)    ; :openai :anthropic :ollama :local
  model:         string
  api-key:       (or string #f)
  endpoint:      string
  timeout:       integer
  max-tokens:    integer
  temperature:   float
  stream:        bool)

;; Chat completion
(def (llm-chat llm messages
               system: (system #f)
               tools: (tools #f)
               stream: (stream #f))
  (let* ((headers (build-headers llm))
         (body (build-chat-body llm messages system: system tools: tools))
         (response (http-post (llm-client-endpoint llm)
                               headers: headers
                               data: (json-encode body)
                               timeout: (llm-client-timeout llm))))
    (if (= (response-status response) 200)
        (let* ((json (response-json response))
               (content (extract-content json llm)))
          ;; Check for tool calls
          (let ((tool-calls (extract-tool-calls json)))
            (if tool-calls
                (handle-tool-calls llm tool-calls messages)
                content)))
        (error "LLM request failed" status: (response-status response)))))

;; Streaming chat (SSE)
(def (llm-stream llm messages on-chunk)
  (let* ((headers (hash-update (build-headers llm)
                               'Accept' "text/event-stream"))
         (body (build-chat-body llm messages stream: #t)))
    (http-post-stream (llm-client-endpoint llm)
                       headers: headers
                       data: (json-encode body)
                       on-chunk: on-chunk
                       timeout: (llm-client-timeout llm))))
```

### Self-Modification Engine

```gerbil
;; meta.ss - Self-modification engine

(export #t make-meta-engine
         evolve-agent!
         generate-code
         validate-modification
         rollback!)

;; Meta configuration
(defstruct meta-config
  (enabled:       bool)              ; Enable self-modification
  git-repo:      string)             ; Git repository path
  sandbox:       bool)               ; Require sandbox for new code

;; Meta engine
(defstruct meta-engine
  (config:        meta-config)
  history:        (box '()))          ; Modification history
  current:        (box #f))           ; Current version

;; Evolution trigger
(def (evolve-agent! agent trigger-reason)
  (let ((meta (agent-meta agent))
        (engine (make-meta-engine meta)))

    ;; 1. Analyze current state
    (let ((analysis (analyze-performance agent)))

      ;; 2. Generate improvement hypothesis
      (when (analysis-needs-improvement? analysis)
        (let ((hypothesis (generate-hypothesis analysis)))

          ;; 3. Generate new code
          (let ((new-code (generate-code hypothesis)))

            ;; 4. Validate before applying
            (when (validate-modification new-code)

              ;; 5. Git commit current version
              (git-commit! (meta-config-git-repo meta)
                           message: (format "Before evolution: ~a" trigger-reason))

              ;; 6. Apply modification
              (apply-modification! agent new-code)

              ;; 7. Test new version
              (let ((test-results (test-agent agent)))
                (if (test-passed? test-results)
                    ;; Success - keep new version
                    (begin
                      (git-commit! (meta-config-git-repo meta)
                                   message: (format "Evolution: ~a" trigger-reason))
                      (update-version! agent)
                      (log-success evolution))
                    ;; Failure - rollback
                    (begin
                      (rollback! agent)
                      (log-failure evolution)))))))))))

;; Code generation
(def (generate-code hypothesis)
  (match (hypothesis-target hypothesis)
    ('gerbil-core
     (generate-gerbil-code hypothesis))
    ('zig-http
     (generate-zig-code hypothesis))
    ('rust-vector
     (generate-rust-code hypothesis))
    (else
     (error "Unknown target" target: (hypothesis-target hypothesis)))))

;; Generate Gerbil code
(def (generate-gerbil-code hypothesis)
  (let ((template (hypothesis-template hypothesis))
        (params (hypothesis-parameters hypothesis)))
    ;; Use macro expansion
    (eval `(let () ,template))
    ;; Or generate string and eval
    (compile-and-eval (fill-template template params))))

;; Generate Zig code
(def (generate-zig-code hypothesis)
  (let* ((code (fill-zig-template (hypothesis-template hypothesis)
                                   (hypothesis-parameters hypothesis)))
         (filename (format "zig/~a.zig" (hypothesis-name hypothesis))))
    ;; Write file
    (with-output (file filename)
      (display code))
    ;; Compile
    (system (format "zig build-lib ~a -dynamic -femit-bin=lib~a.so"
                    filename
                    (hypothesis-name hypothesis)))
    ;; Return DLL path
    (format "lib~a.so" (hypothesis-name hypothesis))))
```

---

## Zig Dynamic Library Specification

### Zig Module Interface

All Zig dynamic libraries must expose a standardized C ABI interface:

```zig
// o_zig.h - Standard header for all Zig DLLs

#ifndef O_ZIG_H
#define O_ZIG_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Standard error codes
#define O_SUCCESS 0
#define O_ERROR -1
#define O_ERROR_NULL_PTR -2
#define O_ERROR_INVALID_PARAM -3
#define O_ERROR_TIMEOUT -4

// Error handling
typedef const char* o_error_t;

// Version info
typedef struct {
    uint32_t major;
    uint32_t minor;
    uint32_t patch;
    const char* git_hash;
} o_version_t;

// Context structure (opaque)
typedef void* o_context_t;

// Standard functions every DLL must implement
const char* o_get_name(void);
const char* o_get_version(void);
o_version_t o_get_version_struct(void);
int o_init(o_context_t* ctx);
int o_shutdown(o_context_t* ctx);
const char* o_get_last_error(o_context_t* ctx);

// Optional: hot-reload support
int o_reload(o_context_t* ctx);
bool o_can_reload(void);

#ifdef __cplusplus
}
#endif

#endif // O_ZIG_H
```

### HTTP Client Module

```zig
// http_client.zig - HTTP client implementation

const std = @import("std");
const o_zig = @import("o_zig");

pub export fn o_get_name() [*:0]const u8 {
    return "o_http_client";
}

pub export fn o_get_version() [*:0]const u8 {
    return "0.1.0";
}

pub export fn o_get_version_struct() o_zig.o_version_t {
    return .{ .major = 0, .minor = 1, .patch = 0, .git_hash = o GIT_HASH };
}

// Context structure
const HttpClient = struct {
    allocator: std.mem.Allocator,
    timeout_ms: u32,
    client: std.http.Client,
};

pub export fn o_init(ctx: *?*anyopaque) callconv(.C) c_int {
    const allocator = std.heap.c_allocator;
    const client = allocator.create(HttpClient) catch return o_zig.O_ERROR;
    client.* = .{
        .allocator = allocator,
        .timeout_ms = 30000,
        .client = std.http.Client{ .allocator = allocator },
    };
    ctx.* = @ptrCast(client);
    return o_zig.O_SUCCESS;
}

pub export fn o_shutdown(ctx: *?*anyopaque) callconv(.C) c_int {
    const client: *HttpClient = @ptrCast(@alignCast(ctx));
    client.allocator.destroy(client);
    return o_zig.O_SUCCESS;
}

// HTTP GET request
pub export fn o_http_get(
    ctx: *?*anyopaque,
    url: [*:0]const u8,
    headers: [*:0]const u8, // JSON string of headers
    response_body: *[*]u8,
    response_size: *usize,
) callconv(.C) c_int {
    const client: *HttpClient = @ptrCast(@alignCast(ctx));

    // Parse URL and make request
    const uri = std.Uri.parse(url) catch return o_zig.O_ERROR_INVALID_PARAM;
    var req = std.http.Client.Request.init(client.allocator);
    req.uri = uri;

    // Add headers if provided
    if (headers[0] != 0) {
        const parsed = std.json.parse(std.HashMap([]const u8, []const u8), headers) catch return o_zig.O_ERROR_INVALID_PARAM;
        iter(parsed) | |entry| {
            req.headers.append(entry.key_ptr.*, entry.value_ptr.*) catch {};
        };
    }

    // Send request
    const response = req.fetch(client.client, .{ .timeout_ms = client.timeout_ms }) catch {
        return o_zig.O_ERROR;
    };

    // Copy response body
    const body = response.body.allocator.dupe(client.allocator) catch return o_zig.O_ERROR;
    response_body.* = body.ptr;
    response_size.* = body.len;

    return o_zig.O_SUCCESS;
}

// HTTP POST request with streaming support
pub export fn o_http_post(
    ctx: *?*anyopaque,
    url: [*:0]const u8,
    body: [*]const u8,
    body_size: usize,
    content_type: [*:0]const u8,
    on_chunk: ?*const fn (data: [*]const u8, size: usize, ud: ?*anyopaque) callconv(.C) void,
    user_data: ?*anyopaque,
) callconv(.C) c_int {
    // Implementation similar to GET, with streaming callback
}

// Cleanup response
pub export fn o_free_response(ctx: *?*anyopaque, ptr: [*]u8) callconv(.C) void {
    const client: *HttpClient = @ptrCast(@alignCast(ctx));
    client.allocator.free(ptr);
}
```

### Build System Integration

```zig
// build.zig - Build script

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // HTTP client module
    const http_lib = b.addSharedLibrary(.{
        .name = "o_http_client",
        .root_source_file = .{ .path = "http_client.zig" },
        .target = target,
        .optimize = optimize,
    });

    // Link against system libraries if needed
    http_lib.linkSystemLibrary("ssl", .{});
    http_lib.linkSystemLibrary("crypto", .{});

    b.installArtifact(http_lib);
}
```

### Module Manifest

Every Zig module should have a `module.json` manifest:

```json
{
    "name": "o_http_client",
    "version": "0.1.0",
    "description": "HTTP client with streaming support",
    "dependencies": [],
    "system_libs": ["ssl", "crypto"],
    "exports": [
        "o_http_get",
        "o_http_post",
        "o_http_stream",
        "o_websocket_connect"
    ],
    "hot_reload": true,
    "zig_version": "0.13.0"
}
```

---

## Rust Dynamic Library Specification

### Rust Module Interface

```rust
// o_rust.rs - Standard header for all Rust DLLs

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Standard error codes
#[repr(C)]
pub enum OError {
    Success = 0,
    Error = -1,
    NullPtr = -2,
    InvalidParam = -3,
    Timeout = -4,
}

/// Version information
#[repr(C)]
pub struct OVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub git_hash: *const u8,
}

/// Context (opaque pointer)
pub type OContext = *mut std::ffi::c_void;

/// Module trait that all modules must implement
pub trait OModule {
    fn name(&self) -> &str;
    fn version(&self) -> &str;
    fn init(&mut self) -> Result<(), OError>;
    fn shutdown(&mut self) -> Result<(), OError>;
    fn can_reload(&self) -> bool;
}

// Standard exports
extern "C" {
    pub fn o_get_name() -> *const u8;
    pub fn o_get_version() -> *const u8;
    pub fn o_get_version_struct() -> OVersion;
    pub fn o_init(ctx: *mut OContext) -> i32;
    pub fn o_shutdown(ctx: OContext) -> i32;
    pub fn o_reload(ctx: OContext) -> i32;
    pub fn o_get_last_error(ctx: OContext) -> *const u8;
}
```

### Vector Operations Module

```rust
// vector_ops.rs - Vector operations for embeddings

use std::simd::{f32x4, num::Float};

#[repr(C)]
pub struct VectorConfig {
    pub dim: usize,
    pub capacity: usize,
}

#[repr(C)]
pub struct VectorModule {
    config: VectorConfig,
    vectors: Vec<Vec<f32>>,
    last_error: String,
}

impl VectorModule {
    fn new(config: VectorConfig) -> Self {
        Self {
            config,
            vectors: Vec::with_capacity(config.capacity),
            last_error: String::new(),
        }
    }

    // Cosine similarity (SIMD optimized)
    fn cosine_similarity(&self, a: &[f32], b: &[f32]) -> f32 {
        assert_eq!(a.len(), b.len());

        let mut dot_product = 0.0;
        let mut norm_a = 0.0;
        let mut norm_b = 0.0;

        // Process 4 floats at a time using SIMD
        let chunks = a.len() / 4;
        for i in 0..chunks {
            let a_simd = f32x4::from_array([
                a[i * 4],
                a[i * 4 + 1],
                a[i * 4 + 2],
                a[i * 4 + 3],
            ]);
            let b_simd = f32x4::from_array([
                b[i * 4],
                b[i * 4 + 1],
                b[i * 4 + 2],
                b[i * 4 + 3],
            ]);

            let prod = a_simd * b_simd;
            dot_product += prod.reduce_sum();

            norm_a += (a_simd * a_simd).reduce_sum();
            norm_b += (b_simd * b_simd).reduce_sum();
        }

        // Handle remaining elements
        for i in (chunks * 4)..a.len() {
            dot_product += a[i] * b[i];
            norm_a += a[i] * a[i];
            norm_b += b[i] * b[i];
        }

        dot_product / (norm_a.sqrt() * norm_b.sqrt())
    }

    // Batch similarity search
    fn search(&self, query: &[f32], top_k: usize) -> Vec<(usize, f32)> {
        self.vectors
            .iter()
            .enumerate()
            .map(|(i, v)| (i, self.cosine_similarity(query, v)))
            .filter(|(_, s)| !s.is_nan())
            .sorted_by(|a, b| b.1.partial_cmp(&a.1).unwrap())
            .take(top_k)
            .collect()
    }
}

// C API exports
#[no_mangle]
pub extern "C" fn o_get_name() -> *const u8 {
    b"o_vector_ops\0".as_ptr()
}

#[no_mangle]
pub extern "C" fn o_get_version() -> *const u8 {
    b"0.1.0\0".as_ptr()
}

#[no_mangle]
pub extern "C" fn o_init(ctx: *mut OContext) -> i32 {
    let config = VectorConfig {
        dim: 1536,
        capacity: 10000,
    };
    let module = Box::new(VectorModule::new(config));
    unsafe {
        *ctx = Box::into_raw(module) as OContext;
    }
    0
}

#[no_mangle]
pub extern "C" fn o_vector_search(
    ctx: OContext,
    query: *const f32,
    query_len: usize,
    results: *mut usize,
    scores: *mut f32,
    top_k: usize,
) -> i32 {
    let module = unsafe { &*(ctx as *const VectorModule) };
    let query_slice = unsafe { std::slice::from_raw_parts(query, query_len) };

    let search_results = module.search(query_slice, top_k);

    for (i, (idx, score)) in search_results.iter().enumerate() {
        unsafe {
            *results.add(i) = *idx;
            *scores.add(i) = *score;
        }
    }

    search_results.len() as i32
}

// Cleanup
#[no_mangle]
pub extern "C" fn o_shutdown(ctx: OContext) -> i32 {
    unsafe {
        let _ = Box::from_raw(ctx as *mut VectorModule);
    }
    0
}
```

### Cargo Build Configuration

```toml
# Cargo.toml

[package]
name = "o_vector_ops"
version = "0.1.0"
edition = "2021"

[lib]
name = "o_vector_ops"
crate-type = ["cdylib"]

[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

[profile.release]
opt-level = 3
lto = true
codegen-units = 1
strip = true
```

### Build Script

```rust
// build.rs

use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();

    // Generate version info
    println!("cargo:rerun-if-env=VCS_SHORT_HASH");
    let git_hash = env::var("VCS_SHORT_HASH").unwrap_or_else(|_| "unknown".to_string());

    // Write version to file
    let version_path = Path::new(&out_dir).join("version.txt");
    fs::write(&version_path, git_hash).unwrap();
}
```

---

## Self-Modification Safety Mechanisms

### Safety Principles

1. **Sandboxing** - All new code runs in isolated environment
2. **Version Control** - Every change is committed to embedded Git
3. **Validation** - Code must pass tests before activation
4. **Rollback** - Automatic rollback on failure
5. **Monitoring** - Continuous health checks
6. **Gradual Rollout** - Canary deployments

### Sandboxing

```gerbil
;; sandbox.ss - Sandbox system for untrusted code

(export #t make-sandbox sandbox-eval sandbox-exec)

;; Sandbox configuration
(defstruct sandbox
  (enabled:       bool
   timeout:       integer         ; Execution timeout
   memory-limit:  integer         ; Memory limit in bytes
   allowed-ops:   (list symbol)  ; Allowed operations
   denied-ops:    (list symbol))  ; Denied operations

;; Create sandbox
(def (make-sandbox
         timeout: (timeout 30)
         memory-limit: (memory-limit (* 100 1024 1024))  ; 100MB
         allowed-ops: (allowed-ops '()))
  (struct sandbox
          enabled: #t
          timeout: timeout
          memory-limit: memory-limit
          allowed-ops: allowed-ops
          denied-ops: '(eval-when-compile compile-file)))

;; Evaluate code in sandbox
(def (sandbox-eval sandbox code)
  (unless (sandbox-enabled sandbox)
    (eval (read (open-input-string code))))

  (let ((result
         (with-timeout (sandbox-timeout sandbox)
           (with-limited-memory (sandbox-memory-limit sandbox)
             (let ((ops (sandbox-denied-ops sandbox))
                   (safe-code (sanitize-code code ops)))
               (eval (read (open-input-string safe-code))))))))
    result))

;; Sanitize code - remove dangerous operations
(def (sanitize-code code denied-ops)
  (let ((parsed (read (open-input-string code))))
    (remove-dangerous-forms parsed denied-ops)))

;; Remove dangerous forms
(def (remove-dangerous-forms expr denied-ops)
  (match expr
    ((and (or 'system 'compile-file 'eval 'load-dll) . ,_)
     (when (member (car expr) denied-ops)
       '(void))
     expr)
    ((,head . ,tail)
     (cons (remove-dangerous-forms head denied-ops)
           (map (lambda (e) (remove-dangerous-forms e denied-ops)) tail)))
    (else expr)))
```

### Validation Pipeline

```gerbil
;; validation.ss - Code validation before applying modifications

(export #t validate-modification run-tests check-performance)

;; Validation result
(defstruct validation-result
  (passed:        bool
   errors:        (list string)
   warnings:      (list string)
   test-coverage: float
   perf-diff:     (or float #f)))

;; Complete validation pipeline
(def (validate-modification new-code)
  (let* ((syntax-check (check-syntax new-code))
         (type-check (check-types new-code))
         (security-check (check-security new-code))
         (tests (run-tests new-code))
         (benchmarks (run-benchmarks new-code))
         (coverage (calculate-coverage new-code)))
    (make-validation-result
     passed: (and (validation-result-passed syntax-check)
                  (validation-result-passed type-check)
                  (validation-result-passed security-check)
                  (validation-result-passed tests))
     errors: (append (validation-result-errors syntax-check)
                     (validation-result-errors type-check)
                     (validation-result-errors security-check)
                     (validation-result-errors tests))
     warnings: (validation-result-warnings security-check)
     test-coverage: coverage
     perf-diff: (benchmark-diff benchmarks))))

;; Syntax check
(def (check-syntax code)
  (try
   (begin
     (read (open-input-string code))
     (make-validation-result passed: #t errors: '() warnings: '()))
   (catch (e)
     (make-validation-result
      passed: #f
      errors: (list (format "Syntax error: ~a" e))
      warnings: '()))))

;; Security check
(def (check-security code)
  (let* ((dangerous-patterns '(
                               "system.*rm.*-rf"
                               "eval.*untrusted"
                               "load-dll.*untrusted"
                               "shutdown.*force"))
         (matches (filter (lambda (p)
                           (regexp-match? (regexp p) code))
                         dangerous-patterns)))
    (if (null? matches)
        (make-validation-result
         passed: #t
         errors: '()
         warnings: '())
        (make-validation-result
         passed: #f
         errors: (map (lambda (m) (format "Dangerous pattern: ~a" m)) matches)
         warnings: '()))))

;; Test suite generation and execution
(def (run-tests code)
  ;; Generate tests from code
  (let ((tests (generate-tests code)))
    (for-each
     (lambda (test)
       (let ((result (eval test)))
         (unless result
           (error "Test failed" test: test))))
     tests)
    (make-validation-result
     passed: #t
     errors: '()
     warnings: '())))

;; Performance regression check
(def (check-performance new-code old-code)
  (let* ((new-time (benchmark new-code))
         (old-time (benchmark old-code))
         (diff (/ (- new-time old-time) (max old-time 1))))
    (if (> diff 0.2)  # 20% slower
        (make-validation-result
         passed: #f
         errors: (list (format "Performance regression: ~a%" (* diff 100)))
         warnings: '())
        (make-validation-result
         passed: #t
         errors: '()
         warnings: '()
         perf-diff: diff))))
```

### Rollback Mechanism

```gerbil
;; rollback.ss - Rollback system for failed modifications

(export #t save-state! rollback! list-states)

;; State snapshot
(defstruct state-snapshot
  (id:           string          ; UUID
   timestamp:    timestamp
   description:  string
   gerbil-hash:  string          ; Git hash for Gerbil code
   zig-versions: (list string)   ; DLL versions
   rust-versions: (list string)
   metrics:      hash-table))

;; State history
(def *state-history* (make-hash-table))

;; Save current state before modification
(def (save-state! agent description)
  (let* ((state-id (generate-uuid))
         (snapshot (make-state-snapshot
                    id: state-id
                    timestamp: (current-time)
                    description: description
                    gerbil-hash: (git-current-hash)
                    zig-versions: (get-loaded-dll-versions "zig")
                    rust-versions: (get-loaded-dll-versions "rust")
                    metrics: (get-current-metrics agent))))
    (hash-set! *state-history* state-id snapshot)

    ;; Git commit
    (git-commit! (agent-git-repo agent)
                 message: (format "Snapshot: ~a" description)
                 tag: state-id)

    state-id))

;; Rollback to previous state
(def (rollback! agent state-id)
  (let ((snapshot (hash-ref *state-history* state-id)))
    (unless snapshot
      (error "State not found" state: state-id))

    ;; Checkout Gerbil code
    (git-checkout! (agent-git-repo agent)
                  (state-snapshot-gerbil-hash snapshot))

    ;; Reload DLLs to specific versions
    (for-each
     (lambda (dll-ver)
       (reload-dll-to-version (car dll-ver) (cdr dll-ver)))
     (append (state-snapshot-zig-versions snapshot)
             (state-snapshot-rust-versions snapshot)))

    ;; Restore metrics
    (restore-metrics! agent (state-snapshot-metrics snapshot))

    ;; Reload Gerbil modules
    (reload-all-modules! agent)

    (log-info "Rolled back to state" state: state-id
              time: (state-snapshot-timestamp snapshot))))

;; List available states
(def (list-states)
  (sort (hash-values *state-history*)
        > (lambda (a b)
            (< (state-snapshot-timestamp a)
               (state-snapshot-timestamp b)))))
```

### Canary Deployment

```gerbil
;; canary.ss - Gradual rollout mechanism

(export #t canary-deploy! canary-status canary-promote! canary-rollback!)

;; Canary deployment
(defstruct canary
  (id:               string
   new-state-id:     string
   old-state-id:     string
   traffic-percent:   float      ; Percentage to new version
   start-time:       timestamp
   metrics-new:      metrics-store
   metrics-old:      metrics-store
   status:           symbol))    ; :monitoring :success :rollback

(def *active-canary* #f)

;; Start canary deployment
(def (canary-deploy! agent new-state-id
                        traffic: (traffic 0.1)  ; Start with 10%
                        duration: (duration 300))  ; Monitor for 5 minutes
  (let* ((old-state-id (get-current-state-id agent))
         (canary (make-canary
                  id: (generate-uuid)
                  new-state-id: new-state-id
                  old-state-id: old-state-id
                  traffic-percent: traffic
                  start-time: (current-time)
                  metrics-new: (make-metrics-store)
                  metrics-old: (make-metrics-store)
                  status: :monitoring)))

    ;; Deploy new version to canary traffic
    (deploy-state-with-split! agent new-state-id old-state-id traffic)

    ;; Start monitoring
    (set! *active-canary* canary)

    ;; Schedule evaluation
    (schedule-canary-evaluation! canary duration)

    (canary-id canary)))

;; Evaluate canary results
(def (evaluate-canary canary)
  (let* ((metrics-new (canary-metrics-new canary))
         (metrics-old (canary-metrics-old canary))
         (comparison (compare-metrics metrics-new metrics-old)))

    (cond
     ;; Check if new version is significantly better
     ((and (> (metric-diff comparison 'latency) -0.1)  # 10% faster
           (> (metric-diff comparison 'error-rate) -0.2))  # 20% fewer errors
      ;; Promote to full traffic
      (canary-promote! (canary-id canary)))

     ;; Check if new version is worse
     ((or (> (metric-diff comparison 'latency) 0.2)  # 20% slower
          (> (metric-diff comparison 'error-rate) 0.1))  # 10% more errors
      ;; Rollback
      (canary-rollback! (canary-id canary)))

     ;; Inconclusive - extend canary
     (else
      (extend-canary! canary)))))

;; Promote canary to full deployment
(def (canary-promote! canary-id)
  (let ((canary (get-canary canary-id)))
    (unless canary
      (error "Canary not found" id: canary-id))

    ;; Switch all traffic to new version
    (switch-to-state! (canary-new-state-id canary))
    (set! (canary-status canary) :success)

    ;; Clean up
    (set! *active-canary* #f)
    (log-info "Canary promoted" canary: canary-id)))

;; Rollback canary
(def (canary-rollback! canary-id)
  (let ((canary (get-canary canary-id)))
    (unless canary
      (error "Canary not found" id: canary-id))

    ;; Switch back to old version
    (switch-to-state! (canary-old-state-id canary))
    (set! (canary-status canary) :rollback)

    ;; Clean up
    (set! *active-canary* #f)
    (log-warning "Canary rolled back" canary: canary-id)))
```

---

## Git Embedded Integration

### Embedded Git Library

```gerbil
;; git-embedded.ss - Embedded Git for version control

(export #t git-init! git-add! git-commit! git-status
         git-current-hash git-checkout! git-log git-diff)

;; Use libgit2 via FFI
(extern
  (c-include "git2.h")

  ;; Git initialization
  (c-libgit2_init::int (path::string))

  ;; Git operations
  (c-libgit2_add::int (repo::void* path::string))
  (c-libgit2_commit::int (repo::void* msg::string oid::void*))
  (c-libgit2_status::int (repo::void* status::void*))
  (c-libgit2_checkout::int (repo::void* oid::void*))
)

;; Git repository wrapper
(defstruct git-repo
  (path:        string
   repo:        (or c-pointer #f)  ; libgit2 repository pointer
   branch:      string            ; Current branch
   head:        string))          ; Current HEAD

;; Initialize repository
(def (git-init! path)
  (unless (directory-exists? path)
    (create-directory path #t))

  (let ((result (c-libgit2_init path)))
    (unless (zero? result)
      (error "Git init failed" path: path))

    (make-git-repo
     path: path
     repo: #f  ; Will be opened on first operation
     branch: "main"
     head: "HEAD")))

;; Open existing repository
(def (git-open repo)
  (let ((result (c-libgit2_repository_open (git-repo-path repo))))
    (set! (git-repo-repo repo) result)
    repo))

;; Add file to staging
(def (git-add! repo file-path)
  (let ((repo-ptr (ensure-git-open! repo)))
    (c-libgit2_add repo-ptr file-path)))

;; Commit changes
(def (git-commit! repo
                       message: (message "")
                       author: (author "O System <o@system.local>")
                       tag: (tag #f))
  (let ((repo-ptr (ensure-git-open! repo))
         (oid (make-byte-array 20)))
    (let ((result (c-libgit2_commit repo-ptr message oid)))
      (unless (zero? result)
        (error "Git commit failed"))

      ;; Add tag if specified
      (when tag
        (git-tag! repo tag oid))

      (bytes->string oid))))

;; Get current HEAD hash
(def (git-current-hash repo)
  (let ((repo-ptr (ensure-git-open! repo)))
    ;; Get HEAD oid
    (let ((oid (c-libgit2_reference_name_to_id repo-ptr "HEAD")))
      (oid->string oid))))

;; Checkout to specific commit or tag
(def (git-checkout! repo ref)
  (let ((repo-ptr (ensure-git-open! repo))
         (oid (resolve-ref repo ref)))
    (let ((result (c-libgit2_checkout repo-ptr oid)))
      (unless (zero? result)
        (error "Git checkout failed" ref: ref))

      ;; Update repo state
      (set! (git-repo-head repo) ref))))

;; Get git log
(def (git-log repo max: (max 10))
  (let ((repo-ptr (ensure-git-open! repo)))
    (c-libgit2_log repo-ptr max)))

;; Get diff between commits
(def (git-diff repo old new)
  (let ((repo-ptr (ensure-git-open! repo)))
    (c-libgit2_diff repo-ptr (resolve-ref repo old) (resolve-ref repo new))))

;; Helper: Ensure repo is open
(def (ensure-git-open! repo)
  (unless (git-repo-repo repo)
    (git-open repo))
  (git-repo-repo repo))

;; Helper: Resolve ref to oid
(def (resolve-ref repo ref)
  (if (string-length ref = 40)  # Assume it's already an oid
      ref
      (c-libgit2_reference_name_to_id (git-repo-repo repo) ref)))
```

### Automatic Version Management

```gerbil
;; version.ss - Semantic versioning for self-evolving agents

(export #t current-version bump-version! version-compare)

;; Version structure
(defstruct version
  (major: integer
   minor: integer
   patch: integer
   pre-release: (or string #f)
   build: (or string #f))

;; Parse version string
(def (string->version str)
  (let ((parts (string-split str ".")))
    (if (>= (length parts) 3)
        (make-version
         major: (string->number (car parts))
         minor: (string->number (cadr parts))
         patch: (string->number (caddr parts))
         pre-release: #f
         build: #f)
        (error "Invalid version string" str))))

;; Format version to string
(def (version->string ver)
  (string-join (list (number->string (version-major ver))
                     (number->string (version-minor ver))
                     (number->string (version-patch ver)))
               "."))

;; Bump version based on change type
(def (bump-version! ver type)
  (case type
    ((major) (make-version
               major: (+ 1 (version-major ver))
               minor: 0
               patch: 0))
    ((minor) (make-version
               major: (version-major ver)
               minor: (+ 1 (version-minor ver))
               patch: 0))
    ((patch) (make-version
               major: (version-major ver)
               minor: (version-minor ver)
               patch: (+ 1 (version-patch ver))))
    (else (error "Invalid version bump type" type: type))))

;; Compare versions
;; Returns: -1 if v1 < v2, 0 if v1 = v2, 1 if v1 > v2
(def (version-compare v1 v2)
  (cond
   ((< (version-major v1) (version-major v2)) -1)
   ((> (version-major v1) (version-major v2)) 1)
   ((< (version-minor v1) (version-minor v2)) -1)
   ((> (version-minor v1) (version-minor v2)) 1)
   ((< (version-patch v1) (version-patch v2)) -1)
   ((> (version-patch v1) (version-patch v2)) 1)
   (else 0)))
```

### Change Log

```gerbil
;; changelog.ss - Track evolution history

(export #t log-change! get-changelog)

;; Change entry
(defstruct change-entry
  (version:      version
   timestamp:    timestamp
   type:         symbol      ; :major :minor :patch
   description:  string
   author:       string      ; Agent ID or "human"
   git-hash:     string
   changes:      (list string)  ; List of modified files/modules
   metrics:      (list (pair symbol float))))  ; Performance changes

;; Changelog storage
(def *changelog* (make-hash-table)

;; Log a change
(def (log-change! agent change-type description changes)
  (let* ((current-ver (agent-version agent))
         (new-ver (bump-version! current-ver change-type))
         (entry (make-change-entry
                 version: new-ver
                 timestamp: (current-time)
                 type: change-type
                 description: description
                 author: (agent-id agent)
                 git-hash: (git-current-hash (agent-git-repo agent))
                 changes: changes
                 metrics: (get-metrics-delta agent))))

    (hash-set! *changelog* (version->string new-ver) entry)

    ;; Update agent version
    (set! (agent-version agent) (version->string new-ver))

    ;; Write to CHANGELOG.md
    (write-changelog-entry! entry)

    new-ver))

;; Get full changelog
(def (get-changelog)
  (sort (hash-values *changelog*)
        > (lambda (a b)
            (< (change-entry-timestamp a)
               (change-entry-timestamp b)))))

;; Write changelog to file
(def (write-changelog-entry! entry)
  (let* ((path (string-append (get-project-root) "/CHANGELOG.md"))
         (content (format-changelog-entry entry))
         (existing (if (file-exists? path)
                       (file->string path)
                       "# Changelog\n\n")))
    (display-to-file (string-append existing content "\n" path)
                     #:exists 'append)))
```

---

## Development Roadmap

### Phase 1: Foundation (Weeks 1-4)

**Goal:** Set up project infrastructure and basic Gerbil agent

| Week | Tasks | Deliverables |
|------|-------|--------------|
| 1 | Project setup, build system, basic structures | `o/` directory structure |
| 2 | Agent core, state management, basic DSL | `agent/core.ss`, `agent/dsl.ss` |
| 3 | Memory system, LLM integration | `agent/memory.ss`, `agent/llm.ss` |
| 4 | Tool framework, first working agent | `agent/tools.ss`, example agent |

### Phase 2: Infrastructure (Weeks 5-8)

**Goal:** Zig dynamic libraries and FFI

| Week | Tasks | Deliverables |
|------|-------|--------------|
| 5 | Zig HTTP client module | `zig/http_client.zig` → `libo_http_client.so` |
| 6 | Zig database modules (PostgreSQL, SQLite) | `zig/postgres.zig`, `zig/sqlite.zig` |
| 7 | Gerbil FFI layer, hot-reload mechanism | `ffi/zig-ffi.ss`, `ffi/reload.ss` |
| 8 | Integration testing, performance benchmarks | Test suite, benchmarks |

### Phase 3: Self-Modification (Weeks 9-12)

**Goal:** Basic self-evolution capabilities

| Week | Tasks | Deliverables |
|------|-------|--------------|
| 9 | Code generation engine, eval sandbox | `meta/codegen.ss`, `meta/sandbox.ss` |
| 10 | Git embedded integration, version management | `git-embedded/`, `version.ss` |
| 11 | Validation pipeline, rollback mechanism | `meta/validation.ss`, `meta/rollback.ss` |
| 12 | First self-modifying agent demo | Working demo |

### Phase 4: Advanced Features (Weeks 13-16)

**Goal:** Production-ready features

| Week | Tasks | Deliverables |
|------|-------|--------------|
| 13 | Rust vector operations module | `rust/vector_ops/` → `libo_vector_ops.so` |
| 14 | Canary deployment, gradual rollout | `canary.ss` |
| 15 | Monitoring, observability, metrics | `utils/metrics.ss`, `utils/logger.ss` |
| 16 | Documentation, examples, tutorials | Full documentation |

### Phase 5: Evolution (Weeks 17+)

**Goal:** Agent starts evolving itself

| Milestone | Description |
|-----------|-------------|
| M1 | Agent modifies its own DSL |
| M2 | Agent generates and compiles new Zig module |
| M3 | Agent optimizes its own memory strategy |
| M4 | Agent learns from user feedback |
| M5 | Agent discovers and fixes its own bugs |

---

## Project Structure

```
o/
├── README.md                          # Project overview
├── LICENSE                             # License file
├── CHANGELOG.md                        # Evolution history
├── docs/
│   ├── ARCHITECTURE.md                 # This document
│   ├── API.md                          # API documentation
│   ├── TUTORIAL.md                     # Tutorial
│   └── EVOLUTION.md                    # Evolution guide
├── gerbil/
│   ├── agent/                          # Agent core
│   │   ├── core.ss                     # Core definitions
│   │   ├── dsl.ss                      # DSL implementation
│   │   ├── state.ss                    # State management
│   │   ├── memory.ss                   # Memory system
│   │   ├── tools.ss                    # Tool framework
│   │   ├── workflow.ss                 # Workflow engine
│   │   ├── llm.ss                      # LLM integration
│   │   └── meta/                       # Self-modification
│   │       ├── codegen.ss              # Code generation
│   │       ├── sandbox.ss              # Sandboxing
│   │       ├── validation.ss           # Validation
│   │       ├── rollback.ss             # Rollback
│   │       └── evolve.ss               # Evolution orchestration
│   ├── ffi/                            # FFI bindings
│   │   ├── zig-ffi.ss                 # Zig FFI
│   │   ├── rust-ffi.ss                # Rust FFI
│   │   └── c-ffi.ss                   # C library bindings
│   ├── utils/                          # Utilities
│   │   ├── json.ss                     # JSON handling
│   │   ├── logger.ss                   # Logging
│   │   ├── config.ss                   # Configuration
│   │   ├── metrics.ss                  # Metrics
│   │   └── version.ss                 # Version management
│   ├── git-embedded/                   # Embedded Git
│   │   ├── git.ss                      # Git operations
│   │   ├── changelog.ss                # Changelog
│   │   └── libgit2/                    # libgit2 bindings
│   ├── build.ss                        # Build system
│   └── main.ss                         # Entry point
├── zig/
│   ├── http_client.zig                 # HTTP client
│   ├── http_server.zig                 # HTTP server
│   ├── websocket.zig                   # WebSocket
│   ├── postgres.zig                    # PostgreSQL client
│   ├── sqlite.zig                      # SQLite client
│   ├── json.zig                        # JSON parser
│   ├── search.zig                      # Search index
│   ├── build.zig                       # Build configuration
│   └── o_zig.zig                       # Common definitions
├── rust/
│   ├── vector_ops/                     # Vector operations
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── similarity.rs
│   │   │   └── search.rs
│   │   └── build.rs
│   ├── ml_inference/                   # ML inference
│   │   ├── Cargo.toml
│   │   └── src/
│   │       └── lib.rs
│   ├── crypto/                         # Cryptography
│   │   ├── Cargo.toml
│   │   └── src/
│   │       └── lib.rs
│   └── compression/                    # Compression
│       ├── Cargo.toml
│       └── src/
│           └── lib.rs
├── tests/
│   ├── integration/                    # Integration tests
│   ├── evolution/                      # Evolution tests
│   └── benchmarks/                     # Performance benchmarks
├── examples/
│   ├── simple_agent/                   # Simple agent example
│   ├── web_scraper/                    # Web scraping agent
│   └── research_assistant/             # Research assistant
└── scripts/
    ├── setup.sh                        # Setup script
    ├── build.sh                        # Build script
    ├── dev.sh                          # Development script
    └── evolve.sh                       # Trigger evolution
```

---

## Appendix

### A. Gerbil Scheme Quick Reference

| Feature | Syntax | Example |
|---------|--------|---------|
| Define function | `(def (name args) body)` | `(def (add x y) (+ x y))` |
| Define macro | `(defrules (name) ...)` | `(defrules (when c . body) ...)` |
| Struct | `(defstruct name fields...)` | `(defstruct point (x y))` |
| Hash table | `(make-hash-table)` | `(hash-set! h 'key 'val)` |
| Eval | `(eval expr)` | `(eval '(+ 1 2))` |
| Foreign function | `(extern (fn ...))` | `(extern (c-func::int ...))` |
| Compile | `(compile-file path)` | `(compile-file "agent.ss")` |

### B. Zig FFI Quick Reference

| Operation | Gerbil | Zig Export |
|-----------|--------|------------|
| Load DLL | `(load-dll "libmodule.so")` | N/A |
| Get function | `(get-ffi-obj "name" lib _fun)` | `pub export fn` |
| Call function | `(c-func arg1 arg2)` | Direct call |
| Free DLL | `(unload-dll lib)` | N/A |

### C. Rust FFI Quick Reference

| Operation | Gerbil | Rust Export |
|-----------|--------|------------|
| Load DLL | `(load-dll "libmodule.so")` | N/A |
| Get function | `(get-ffi-obj "name" lib _fun)` | `#[no_mangle] pub extern "C"` |
| Call function | `(c-func arg1 arg2)` | Direct call |
| Free DLL | `(unload-dll lib)` | N/A |

### D. Conventions

- **Naming**: `o_<module>` for public symbols
- **Versioning**: Semantic versioning with Git hashes
- **Errors**: Return integer error codes, `o_get_last_error()` for details
- **Memory**: Caller owns returned pointers, must call appropriate free function
- **Logging**: Use structured logging with timestamps and context

---

## Conclusion

This architecture enables:
1. **Self-modification** through Lisp metaprogramming
2. **Performance** through compiled Zig/Rust modules
3. **Safety** through sandboxing and validation
4. **Reliability** through Git versioning and rollback
5. **Extensibility** through modular DLL design

The key innovation is that the Agent can evolve all parts of itself:
- **Gerbil code** - Directly modify and eval
- **Zig code** - Generate, compile, and load new DLL
- **Rust code** - Generate, compile, and load new DLL

This creates a truly self-evolving system.
