# LLM Client Library

Unified LLM client library for Project O, supporting multiple providers with a consistent interface.

## Supported Providers

- **OpenAI** - GPT-4, GPT-3.5, etc.
- **Anthropic** - Claude 3.5 Sonnet, Claude 3 Opus, etc.
- **Groq** - Fast inference with Llama, Mixtral, etc.
- **Ollama** - Local LLM inference

## Features

- ✅ Unified interface across all providers
- ✅ Tool calling support (function calling)
- ✅ Streaming responses (planned)
- ✅ Type-safe message structures
- ✅ Error handling and validation
- ✅ Configuration management
- ✅ Multi-turn conversations

## Installation

```bash
# Set up environment variables
export OPENAI_API_KEY="your-openai-key"
export ANTHROPIC_API_KEY="your-anthropic-key"
export GROQ_API_KEY="your-groq-key"

# For Ollama (local)
# Install Ollama from https://ollama.ai
ollama pull llama3.2:latest
```

## Quick Start

### Simple Chat

```scheme
(import :gerbil/llm/client)

;; OpenAI
(def response (llm-simple :openai "Why is the sky blue?"))
(displayln response)

;; Anthropic
(def response (llm-simple :anthropic "Explain quantum computing."))
(displayln response)

;; Groq (fast inference)
(def response (llm-simple :groq "What is 2+2?"))
(displayln response)

;; Ollama (local)
(def response (llm-simple :ollama "Hello, world!"))
(displayln response)
```

### Multi-turn Conversation

```scheme
(import :gerbil/llm/client :gerbil/llm/types)

(def messages
  (list
   (make-user-message "My name is Alice.")
   (make-assistant-message "Hello Alice! Nice to meet you.")
   (make-user-message "What is my name?")))

(def response (llm-chat-completion :openai messages))
(displayln (extract-text-content (llm-response-message response)))
;; Output: "Your name is Alice."
```

### Tool Calling (Function Calling)

```scheme
(import :gerbil/llm/client :gerbil/llm/types)

;; Define a tool
(def calculator-tool
  (make-tool-definition-instance
   "calculate"
   "Perform a mathematical calculation"
   (hash
    'type "object"
    'properties (hash
                 'expression (hash
                              'type "string"
                              'description "Mathematical expression to evaluate"))
    'required '("expression"))))

;; Call LLM with tool
(def response (llm-with-tools
               :openai
               "What is 25 * 4?"
               (list calculator-tool)))

;; Check for tool calls
(when (has-tool-calls? (llm-response-message response))
  (displayln "Tool calls requested:")
  (for-each
   (lambda (tc)
     (displayln (format "  - ~a(~a)"
                       (function-call-name (tool-call-function tc))
                       (function-call-arguments (tool-call-function tc)))))
   (llm-message-tool-calls (llm-response-message response))))
```

### Using Configuration Objects

```scheme
(import :gerbil/llm/client :gerbil/llm/types)

;; Create a configuration
(def config (make-llm-config-instance
             :openai
             model: "gpt-4"
             temperature: 0.5
             max-tokens: 1000))

;; Use configuration for chat
(def messages (list (make-user-message "Hello!")))
(def response (llm-chat-with-config config messages))
```

## API Reference

### Core Functions

#### `llm-simple`

Simple chat interface for all providers.

```scheme
(llm-simple provider prompt
            #!key
            (model #f)
            (system-prompt "You are a helpful assistant.")
            (api-key #f)
            (endpoint #f))
```

**Parameters:**
- `provider` - `:openai`, `:anthropic`, `:groq`, or `:ollama`
- `prompt` - User prompt string
- `model` - Model name (uses default if not specified)
- `system-prompt` - System prompt
- `api-key` - API key (uses env var if not specified)
- `endpoint` - Custom endpoint (for Ollama)

**Returns:** String content of response

#### `llm-chat-completion`

Full-featured chat completion interface.

```scheme
(llm-chat-completion provider messages
                     #!key
                     (model #f)
                     (api-key #f)
                     (temperature 0.7)
                     (max-tokens #f)
                     (system #f)
                     (tools #f)
                     (tool-choice #f)
                     (endpoint #f))
```

**Parameters:**
- `provider` - Provider symbol
- `messages` - List of `llm-message` structures
- `model` - Model name
- `api-key` - API key
- `temperature` - Sampling temperature (0-2)
- `max-tokens` - Maximum tokens to generate
- `system` - System prompt (for Anthropic)
- `tools` - List of `tool-definition` structures
- `tool-choice` - Tool choice strategy
- `endpoint` - Custom endpoint (for Ollama)

**Returns:** `llm-response` structure

#### `llm-with-tools`

Tool-enabled chat interface.

```scheme
(llm-with-tools provider prompt tools
                #!key
                (model #f)
                (system-prompt "You are a helpful assistant.")
                (api-key #f)
                (endpoint #f))
```

**Returns:** `llm-response` structure (check for tool calls)

### Type Constructors

#### Messages

```scheme
;; Create messages
(make-user-message "Hello!")
(make-assistant-message "Hi there!")
(make-system-message "You are a helpful assistant.")
(make-tool-message "Result: 42" "call-123")
```

#### Tool Definitions

```scheme
;; Create a tool definition
(make-tool-definition-instance
 "function_name"
 "Function description"
 (hash
  'type "object"
  'properties (hash
               'param1 (hash 'type "string" 'description "Parameter 1")
               'param2 (hash 'type "number" 'description "Parameter 2"))
  'required '("param1")))
```

#### Configuration

```scheme
;; Create LLM configuration
(make-llm-config-instance
 :openai
 model: "gpt-4"
 temperature: 0.5
 max-tokens: 1000)
```

### Utility Functions

```scheme
;; Extract text content from message
(extract-text-content msg)

;; Check if message has tool calls
(has-tool-calls? msg)

;; Provider capabilities
(provider-supports-tools? :openai)        ;; => #t
(provider-requires-max-tokens? :anthropic) ;; => #t
(provider-supports-streaming? :groq)       ;; => #t
```

## Data Structures

### `llm-message`

```scheme
(defstruct llm-message
  (role         ; :user :assistant :system :tool
   content      ; string or list of content blocks
   name         ; optional name (for tool messages)
   tool-calls   ; optional list of tool-call structures
   tool-call-id) ; optional tool call ID
  transparent: #t)
```

### `llm-response`

```scheme
(defstruct llm-response
  (id            ; response ID
   model         ; model used
   message       ; llm-message
   finish-reason ; :stop :tool-calls :length :content-filter
   usage         ; usage-stats structure
   created       ; timestamp
   provider)     ; provider name
  transparent: #t)
```

### `tool-call`

```scheme
(defstruct tool-call
  (id        ; unique identifier
   type      ; "function"
   function) ; function-call structure
  transparent: #t)
```

### `tool-definition`

```scheme
(defstruct tool-definition
  (type      ; "function"
   function) ; function-definition structure
  transparent: #t)
```

## Provider-Specific Notes

### OpenAI

- Default model: `gpt-4`
- Supports streaming: Yes
- Supports tools: Yes
- System messages: Included in messages array

### Anthropic

- Default model: `claude-3-5-sonnet-20241022`
- Supports streaming: Yes
- Supports tools: Yes
- System messages: Separate `system` parameter
- **Requires** `max-tokens` parameter

### Groq

- Default model: `llama-3.3-70b-versatile`
- Supports streaming: Yes
- Supports tools: Yes
- OpenAI-compatible API
- Very fast inference

### Ollama

- Default model: `llama3.2:latest`
- Supports streaming: Yes
- Supports tools: Limited (model-dependent)
- Runs locally
- Default endpoint: `http://localhost:11434`

## Testing

```bash
# Run all tests
gxi gerbil/llm/test/run-tests.ss

# Run unit tests only
gxi gerbil/llm/test/types-test.ss

# Run integration tests only
gxi gerbil/llm/test/client-test.ss
```

**Note:** Integration tests require API keys to be set in environment variables.

## Examples

See the commented-out examples at the end of each module:
- `gerbil/llm/openai.ss`
- `gerbil/llm/anthropic.ss`
- `gerbil/llm/groq.ss`
- `gerbil/llm/ollama.ss`
- `gerbil/llm/client.ss`

## Error Handling

All API errors are returned as `llm-error` structures:

```scheme
(defstruct llm-error
  (type      ; :api-error :rate-limit :invalid-request :timeout
   message   ; error message
   status    ; HTTP status code
   provider  ; provider name
   details)  ; additional error details
  transparent: #t)
```

Check for errors:

```scheme
(def response (llm-simple :openai "Hello"))

(if (llm-error? response)
    (displayln (format "Error: ~a" (llm-error-message response)))
    (displayln response))
```

## Architecture

```
gerbil/llm/
├── types.ss          # Unified type definitions
├── openai.ss         # OpenAI client
├── anthropic.ss      # Anthropic client
├── groq.ss           # Groq client
├── ollama.ss         # Ollama client
├── client.ss         # Unified interface
└── test/
    ├── types-test.ss    # Unit tests
    ├── client-test.ss   # Integration tests
    └── run-tests.ss     # Test runner
```

## Roadmap

- [x] OpenAI client
- [x] Anthropic client
- [x] Groq client
- [x] Ollama client
- [x] Unified interface
- [x] Tool calling support
- [x] Unit tests
- [x] Integration tests
- [ ] Streaming support
- [ ] Embeddings API
- [ ] Image generation
- [ ] Vision support
- [ ] Audio support

## Contributing

This is part of Project O's Phase 2, Week 1 implementation. See `docs/REVISED_ROADMAP.md` for the full roadmap.

## License

Part of Project O - Self-Evolving AI Agent Platform
