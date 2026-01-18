;;; Gerbil build script for Project O

(import :std/build-script)

(define build-spec
  (add-spec (static-library "o")
    (sources
     "agent/types.ss"
     "agent/state.ss"
     "agent/dsl.ss"
     "agent/core.ss"
     "agent/memory.ss"
     "agent/tools.ss"
     "agent/executor.ss"
     "agent/context.ss"
     "agent/streaming.ss"
     "agent/elixir-bridge.ss"
     "llm/types.ss"
     "llm/openai.ss"
     "llm/anthropic.ss"
     "llm/groq.ss"
     "llm/ollama.ss"
     "llm/client.ss"
     "memory/types.ss"
     "memory/blocks.ss"
     "memory/core.ss"
     "memory/archival.ss"
     "memory/semantic.ss"
     "message/types.ss"
     "message/manager.ss"
     "message/stream.ss"
     "tools/types.ss"
     "tools/core.ss"
     "tools/sandbox.ss"
     "tools/rules.ss"
     "tools/memory.ss"
     "database/client.ss"
     "database/agents.ss"
     "database/messages.ss")
    (options (-vvv -Xref))))

(build-spec)
