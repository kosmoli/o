# Gerbil Memory System

Advanced memory management system for Project O, implementing memos-compatible memory blocks with core memory, archival memory, and recall memory.

## Overview

The memory system provides:
- **Memory Blocks** - Structured memory blocks (persona, human, custom)
- **Core Memory** - Stable identity with editable blocks
- **Core Memory Operations** - Append, replace, patch operations with history tracking
- **Memory Rollback** - Undo changes and restore previous states
- **Memory Constraints** - Validation rules and size limits
- **Archival Memory** - Long-term storage with semantic search
- **Recall Memory** - Short-term context management
- **Memory Validation** - Validate memory parameters and consistency
- **Memory Statistics** - Track memory usage and counts
- **Export/Import** - Export memory to JSON/text formats

## Architecture

```
Application
    ↓
memory/core.ss (Core memory operations with history)
    ↓
memory/blocks.ss (High-level block operations)
    ↓
memory/types.ss (Type definitions and validation)
    ↓
database/client.ss (Low-level database client)
    ↓
PostgreSQL Database
```

## Installation

No installation required - part of Project O's Gerbil codebase.

## Usage

### Creating a Block Manager

```scheme
(import :gerbil/memory/blocks
        :gerbil/memory/types
        :gerbil/database/client)

;; Connect to database
(db-connect!)

;; Create block manager for an agent
(def manager (make-block-manager agent-id cache-enabled: #t))
```

### Initializing Core Memory

```scheme
;; Initialize with default templates
(core-memory-initialize! manager)

;; Initialize with custom values
(core-memory-initialize! manager
                        persona: "You are a helpful AI assistant."
                        human: "User prefers concise responses.")
```

### Creating Memory Blocks

```scheme
;; Create standard block (persona)
(block-create! manager "persona"
              "You are a helpful AI assistant."
              is-template: #t
              is-readonly: #f)

;; Create custom block
(block-create! manager "preferences"
              "User likes technical details."
              is-readonly: #f)

;; Create from template
(block-create-from-template! manager "human")

;; Ensure block exists (create if not)
(block-ensure-exists! manager "persona")
```

### Retrieving Memory Blocks

```scheme
;; Get block by label
(def block (block-get manager "persona"))
(displayln (format "Persona: ~a" (hash-ref block 'value)))

;; Get block value directly
(def value (block-get-value manager "persona"))

;; Get all blocks
(def all-blocks (block-get-all manager))

;; Get standard blocks (persona, human)
(def standard (block-get-standard manager))

;; Get custom blocks
(def custom (block-get-custom manager))

;; Check if block exists
(if (block-exists? manager "preferences")
    (displayln "Preferences block exists")
    (displayln "Preferences block not found"))
```

### Updating Memory Blocks

```scheme
;; Update block value
(block-update! manager "persona"
              "You are a very helpful AI assistant.")

;; Append to block
(block-append! manager "human"
              "User is a software engineer."
              separator: "\n")

;; Replace text in block
(block-replace! manager "persona"
               "helpful" "extremely helpful")

;; Set read-only status
(block-set-readonly! manager "persona" #t)
```

### Deleting Memory Blocks

```scheme
;; Delete custom block (cannot delete standard blocks)
(block-delete! manager "preferences")

;; Note: Cannot delete standard blocks (persona, human)
;; Note: Cannot delete read-only blocks
```

### Core Memory Operations

```scheme
;; Get core memory structure
(def memory (core-memory-get manager))
(displayln (format "Persona: ~a" (core-memory-persona memory)))
(displayln (format "Human: ~a" (core-memory-human memory)))

;; Access custom blocks
(def custom-blocks (core-memory-custom memory))
(hash-for-each
 (lambda (label value)
   (displayln (format "~a: ~a" label value)))
 custom-blocks)

;; Set core memory structure
(def new-memory (make-core-memory
                 persona: "New persona"
                 human: "New human info"
                 custom: (hash "preferences" "New preferences")))
(core-memory-set! manager new-memory)

;; Get specific block from core memory
(def persona (core-memory-get-block memory "persona"))

;; Set specific block in core memory
(core-memory-set-block! memory "persona" "Updated persona")

;; Check if block exists in core memory
(if (core-memory-has-block? memory "preferences")
    (displayln "Preferences block exists")
    (displayln "Preferences block not found"))

;; List all block labels
(def labels (core-memory-list-blocks memory))
(displayln (format "Blocks: ~a" labels))
```

### Core Memory Operations

#### Creating a Core Memory Manager

```scheme
(import :gerbil/memory/core
        :gerbil/database/client)

;; Connect to database
(db-connect!)

;; Create core memory manager with history tracking
(def core-manager (make-core-memory-manager agent-id
                                            cache-enabled: #t
                                            max-history: 100))

;; Initialize core memory
(core-memory-initialize! (core-memory-manager-block-manager core-manager)
                        persona: "You are a helpful AI assistant."
                        human: "User prefers concise responses.")
```

#### Core Memory Append

```scheme
;; Append to persona block
(core-memory-append! core-manager "persona"
                    "I am knowledgeable about AI and machine learning.")

;; Append with custom separator
(core-memory-append! core-manager "human"
                    "User is a software engineer."
                    separator: " | ")

;; Append to empty block (no separator added)
(core-memory-clear-block! core-manager "persona")
(core-memory-append! core-manager "persona" "New content.")
```

#### Core Memory Replace

```scheme
;; Replace text in block
(core-memory-replace! core-manager "persona"
                     "helpful" "very helpful")

;; Case-insensitive replace
(core-memory-replace! core-manager "persona"
                     "AI" "artificial intelligence"
                     case-sensitive?: #f)

;; Replace records history automatically
(def history (core-memory-manager-history core-manager))
(def changes (history-get-changes history limit: 5))
```

#### Memory Patch Operations

```scheme
;; Apply single append patch
(memory-apply-patch! core-manager "persona"
                    (hash 'op "append" 'value "I am helpful."))

;; Apply replace patch
(memory-apply-patch! core-manager "persona"
                    (hash 'op "replace"
                          'old "helpful"
                          'new "very helpful"))

;; Set block value directly
(memory-apply-patch! core-manager "persona"
                    (hash 'op "set" 'value "New persona content."))

;; Clear block
(memory-apply-patch! core-manager "persona"
                    (hash 'op "clear"))

;; Prepend to block
(memory-apply-patch! core-manager "persona"
                    (hash 'op "prepend" 'value "Prefix: "))

;; Apply multiple patches sequentially
(memory-apply-patch! core-manager "human"
                    (list (hash 'op "append" 'value "User is a developer.")
                          (hash 'op "append" 'value "User prefers technical details.")))
```

#### Memory Rollback

```scheme
;; Rollback single change
(core-memory-append! core-manager "persona" "Temporary change.")
(memory-rollback! core-manager "persona" steps: 1)

;; Rollback multiple changes
(core-memory-append! core-manager "persona" "Change 1.")
(core-memory-append! core-manager "persona" "Change 2.")
(core-memory-append! core-manager "persona" "Change 3.")
(memory-rollback! core-manager "persona" steps: 3)

;; Rollback to specific timestamp
(def checkpoint-time (current-seconds))
(core-memory-append! core-manager "persona" "After checkpoint.")
(memory-rollback-to-timestamp! core-manager "persona" checkpoint-time)
```

#### Memory Constraints

```scheme
;; Create custom constraints
(def constraints (make-memory-constraints
                  max-block-size: 5000
                  min-block-size: 10
                  readonly-blocks: '("persona")
                  required-blocks: '("persona" "human")
                  custom-validators: (list my-validator)))

;; Create manager with constraints
(def core-manager (make-core-memory-manager agent-id
                                            constraints: constraints))

;; Constraints are validated automatically
(try
 (core-memory-append! core-manager "persona" (make-string 10000 #\a))
 (catch (e)
   (displayln "Constraint violation: Block too large")))

;; Custom validator example
(def no-profanity-validator
  (lambda (label value)
    (if (string-contains value "badword")
        (cons #f (list "Profanity not allowed"))
        (cons #t #f))))
```

#### Memory Validation

```scheme
;; Validate entire core memory
(def result (validate-core-memory core-manager))
(if (car result)
    (displayln "Core memory is valid")
    (displayln (format "Validation errors: ~a" (cdr result))))

;; Check required blocks exist
;; Check block size constraints
;; Run custom validators
```

#### Memory History

```scheme
;; Get recent changes
(def history (core-memory-manager-history core-manager))
(def recent-changes (history-get-changes history limit: 10))

(for-each
 (lambda (change)
   (displayln (format "~a: ~a on ~a"
                     (memory-change-timestamp change)
                     (memory-change-operation change)
                     (memory-change-block-label change))))
 recent-changes)

;; Get changes for specific block
(def persona-changes (history-get-changes-for-block history "persona"))

;; Each change includes:
;; - timestamp: When the change occurred
;; - operation: append, replace, patch, rollback
;; - block-label: Which block was changed
;; - old-value: Previous value
;; - new-value: New value
;; - metadata: Additional operation-specific data
```

#### Convenience Functions

```scheme
;; Set block value directly
(core-memory-set-block! core-manager "persona" "New content.")

;; Clear block
(core-memory-clear-block! core-manager "persona")

;; Prepend to block
(core-memory-prepend! core-manager "persona" "Prefix: " separator: " ")
```

### Archival Memory Operations

#### Creating an Archival Manager

```scheme
(import :gerbil/memory/archival
        :gerbil/database/client)

;; Connect to database
(db-connect!)

;; Create archival manager with embedding support
(def archival-mgr (make-archival-manager agent-id
                                         llm-provider: :openai
                                         llm-model: "text-embedding-3-small"
                                         cache-enabled: #t))
```

#### Inserting Archival Entries

```scheme
;; Insert entry with embedding generation
(archival-insert! archival-mgr
                 "User prefers dark mode and technical documentation."
                 importance: 0.8
                 tags: '("preferences" "ui")
                 generate-embedding?: #t)

;; Insert without embedding (faster)
(archival-insert! archival-mgr
                 "User mentioned liking coffee."
                 importance: 0.5
                 tags: '("personal")
                 generate-embedding?: #f)

;; Insert batch entries
(archival-insert-batch! archival-mgr
                       (list (hash 'content "Entry 1"
                                  'importance 0.7
                                  'tags '("batch")
                                  'generate_embedding #t)
                             (hash 'content "Entry 2"
                                  'importance 0.6
                                  'tags '("batch")
                                  'generate_embedding #f)))
```

#### Retrieving Archival Entries

```scheme
;; Get entry by ID
(def entry (archival-get archival-mgr entry-id))

;; Get all entries with pagination
(def all-entries (archival-get-all archival-mgr limit: 100 offset: 0))

;; Get recent entries
(def recent (archival-get-recent archival-mgr 10))
```

#### Searching Archival Memory

```scheme
;; Text-based search
(def results (archival-search archival-mgr "dark mode" limit: 5))
(displayln (format "Found ~a entries" (length results)))

;; Search by tags
(def tagged (archival-search-by-tags archival-mgr '("preferences" "ui") limit: 10))

;; Search by importance threshold
(def important (archival-search-by-importance archival-mgr 0.7 limit: 10))
```

#### Archival Pagination

```scheme
;; Get paginated results
(def page (archival-get-page archival-mgr 0 20))
(displayln (format "Page ~a of ~a total entries"
                   (archival-page-page page)
                   (archival-page-total page)))

(displayln (format "Has next: ~a" (archival-page-has-next? page)))
(displayln (format "Has prev: ~a" (archival-page-has-prev? page)))

;; Navigate pages
(when (archival-page-has-next? page)
  (def next-page-num (archival-get-next-page page))
  (def next-page (archival-get-page archival-mgr next-page-num 20)))
```

#### Updating Archival Entries

```scheme
;; Update entry content
(archival-update! archival-mgr entry-id
                 (hash 'content "Updated content"
                       'importance 0.9))

;; Update importance only
(archival-update-importance! archival-mgr entry-id 0.95)

;; Add tags to entry
(archival-add-tags! archival-mgr entry-id '("new-tag" "another-tag"))
```

#### Deleting Archival Entries

```scheme
;; Delete single entry
(archival-delete! archival-mgr entry-id)

;; Delete batch entries
(archival-delete-batch! archival-mgr (list id1 id2 id3))

;; Clear all archival memory
(archival-clear! archival-mgr)
```

#### Archival Statistics

```scheme
;; Count entries
(def count (archival-count archival-mgr))

;; Calculate total size
(def size (archival-total-size archival-mgr))

;; Get comprehensive statistics
(def stats (archival-get-stats archival-mgr))
(displayln (format "Total entries: ~a" (hash-ref stats 'total_entries)))
(displayln (format "Average importance: ~a" (hash-ref stats 'avg_importance)))
(displayln (format "Entries with embeddings: ~a" (hash-ref stats 'entries_with_embeddings)))
(displayln (format "Unique tags: ~a" (hash-ref stats 'unique_tags)))
```

#### Archival Export/Import

```scheme
;; Export to JSON (without embeddings for smaller size)
(def json-export (archival-export archival-mgr
                                 format: 'json
                                 include-embeddings?: #f))

;; Export to text
(def text-export (archival-export archival-mgr format: 'text))

;; Import from exported data
(def export-data (string->json-object json-export))
(archival-import! archival-mgr export-data)
```

#### Embedding Generation

```scheme
;; Generate single embedding
(def embedding (generate-embedding archival-mgr "Text to embed"))
(displayln (format "Embedding dimensions: ~a" (length embedding)))

;; Generate batch embeddings
(def embeddings (generate-embeddings-batch archival-mgr
                                          '("Text 1" "Text 2" "Text 3")))
```

### Searching Memory Blocks

```scheme
;; Search by content
(def results (block-search manager "helpful"))
(displayln (format "Found ~a blocks" (length results)))

;; Search by label pattern
(def label-results (block-search-by-label manager "pref"))
```

### Memory Statistics

```scheme
;; Count blocks
(def total (block-count manager))
(def standard-count (block-count-standard manager))
(def custom-count (block-count-custom manager))

;; Calculate total size
(def size (block-total-size manager))

;; Get comprehensive statistics
(def stats (block-get-stats manager))
(displayln (format "Total blocks: ~a" (hash-ref stats 'total_blocks)))
(displayln (format "Standard blocks: ~a" (hash-ref stats 'standard_blocks)))
(displayln (format "Custom blocks: ~a" (hash-ref stats 'custom_blocks)))
(displayln (format "Total size: ~a bytes" (hash-ref stats 'total_size)))
```

### Export and Import

```scheme
;; Export to JSON
(def json-export (block-export manager format: 'json))
(displayln json-export)

;; Export to text
(def text-export (block-export manager format: 'text))
(displayln text-export)

;; Import from exported data
(def blocks-data (hash 'agent_id agent-id
                       'blocks (list ...)))
(block-import! manager blocks-data)
```

### Block Validation

```scheme
;; Validate single block
(def result (block-validate manager "persona"))
(if (car result)
    (displayln "Block is valid")
    (displayln (format "Validation errors: ~a" (cdr result))))

;; Validate all blocks
(def all-result (block-validate-all manager))
(if (car all-result)
    (displayln "All blocks are valid")
    (displayln (format "Validation errors: ~a" (cdr all-result))))

;; Validate block parameters
(def params (hash 'label "test" 'value "Test value"))
(def param-result (validate-block-params params))
```

### Block Caching

```scheme
;; Blocks are automatically cached when retrieved

;; Get from cache (fast)
(def cached (block-cache-get manager "persona"))

;; Invalidate cache entry
(block-cache-invalidate! manager "persona")

;; Clear entire cache
(block-cache-clear! manager)
```

### Block Utilities

```scheme
;; List all block labels
(def labels (block-list-labels manager))
(displayln (format "Labels: ~a" labels))

;; Get blocks by labels
(def blocks (block-get-by-labels manager '("persona" "human")))

;; Copy block
(block-copy! manager "persona" "persona_backup")
```

## API Reference

### Core Memory Manager

#### Constructor

- `(make-core-memory-manager agent-id #!key ...)` - Create core memory manager
  - `cache-enabled` - Enable block caching (default: #t)
  - `max-history` - Maximum history size (default: 100)
  - `constraints` - Memory constraints (default: default-constraints)

#### Core Memory Operations

- `(core-memory-append! manager block-label text #!key ...)` - Append text to block
  - `separator` - Separator between old and new text (default: "\n")
  - `validate?` - Validate constraints (default: #t)

- `(core-memory-replace! manager block-label old-text new-text #!key ...)` - Replace text
  - `validate?` - Validate constraints (default: #t)
  - `case-sensitive?` - Case-sensitive search (default: #t)

- `(memory-apply-patch! manager block-label patch #!key ...)` - Apply JSON patch
  - `validate?` - Validate constraints (default: #t)
  - Patch operations: append, prepend, replace, set, clear

#### Memory Rollback

- `(memory-rollback! manager block-label #!key (steps 1))` - Rollback N changes
- `(memory-rollback-to-timestamp! manager block-label timestamp)` - Rollback to timestamp

#### Memory Validation

- `(validate-core-memory manager)` - Validate entire core memory
- `(validate-constraints manager block-label new-value)` - Validate constraints

#### Memory History

- `(history-get-changes history #!key (limit 10))` - Get recent changes
- `(history-get-changes-for-block history block-label)` - Get changes for block
- `(history-add-change! history change)` - Add change to history

#### Memory Statistics

- `(get-memory-stats manager)` - Get memory statistics with history

#### Convenience Functions

- `(core-memory-set-block! manager block-label value #!key ...)` - Set block value
- `(core-memory-clear-block! manager block-label)` - Clear block
- `(core-memory-prepend! manager block-label text #!key ...)` - Prepend text

### Archival Memory Manager

#### Constructor

- `(make-archival-manager agent-id #!key ...)` - Create archival manager
  - `llm-provider` - LLM provider for embeddings (default: :openai)
  - `llm-model` - Embedding model (default: "text-embedding-3-small")
  - `cache-enabled` - Enable caching (default: #t)

#### Entry Creation

- `(archival-insert! manager content #!key ...)` - Insert archival entry
  - `importance` - Importance score 0.0-1.0 (default: 0.5)
  - `tags` - List of tags (default: empty)
  - `generate-embedding?` - Generate embedding (default: #t)

- `(archival-insert-batch! manager entries)` - Insert multiple entries

#### Entry Retrieval

- `(archival-get manager entry-id)` - Get entry by ID
- `(archival-get-all manager #!key (limit 100) (offset 0))` - Get all entries
- `(archival-get-recent manager n)` - Get N recent entries

#### Search Operations

- `(archival-search manager query #!key (limit 10))` - Text-based search
- `(archival-search-by-tags manager tags #!key (limit 10))` - Search by tags
- `(archival-search-by-importance manager min-importance #!key (limit 10))` - Search by importance

#### Pagination

- `(archival-get-page manager page-number page-size)` - Get paginated entries
- `(archival-get-next-page page-result)` - Get next page number
- `(archival-get-prev-page page-result)` - Get previous page number

#### Entry Update

- `(archival-update! manager entry-id updates)` - Update entry
- `(archival-update-importance! manager entry-id importance)` - Update importance
- `(archival-add-tags! manager entry-id new-tags)` - Add tags

#### Entry Deletion

- `(archival-delete! manager entry-id)` - Delete entry
- `(archival-delete-batch! manager entry-ids)` - Delete multiple entries
- `(archival-clear! manager)` - Clear all entries

#### Statistics

- `(archival-count manager)` - Count entries
- `(archival-total-size manager)` - Calculate total size
- `(archival-get-stats manager)` - Get comprehensive statistics

#### Caching

- `(archival-cache-get manager entry-id)` - Get from cache
- `(archival-cache-put! manager entry-id entry)` - Put in cache
- `(archival-cache-invalidate! manager entry-id)` - Invalidate cache
- `(archival-cache-clear! manager)` - Clear cache

#### Export/Import

- `(archival-export manager #!key ...)` - Export archival memory
  - `format` - Export format (json or text)
  - `include-embeddings?` - Include embeddings (default: #f)

- `(archival-import! manager data)` - Import archival memory

#### Embedding Generation

- `(generate-embedding manager content)` - Generate single embedding
- `(generate-embeddings-batch manager contents)` - Generate batch embeddings

### Block Manager

#### Constructor

- `(make-block-manager agent-id #!key (cache-enabled #t))` - Create block manager

#### Block Creation

- `(block-create! manager label value #!key ...)` - Create memory block
  - `is-template` - Is this a template block?
  - `is-readonly` - Is this block read-only?
  - `validate?` - Validate parameters?

- `(block-create-from-template! manager label)` - Create from template
- `(block-ensure-exists! manager label)` - Ensure block exists

#### Block Retrieval

- `(block-get manager label)` - Get block by label
- `(block-get-all manager)` - Get all blocks
- `(block-get-value manager label)` - Get block value
- `(block-get-standard manager)` - Get standard blocks
- `(block-get-custom manager)` - Get custom blocks

#### Block Update

- `(block-update! manager label value #!key (validate? #t))` - Update block
- `(block-append! manager label text #!key (separator "\n"))` - Append to block
- `(block-replace! manager label old-text new-text)` - Replace text
- `(block-set-readonly! manager label readonly?)` - Set read-only status

#### Block Deletion

- `(block-delete! manager label)` - Delete block (custom blocks only)

#### Block Validation

- `(block-exists? manager label)` - Check if block exists
- `(block-validate manager label)` - Validate block
- `(block-validate-all manager)` - Validate all blocks

#### Block Caching

- `(block-cache-get manager label)` - Get from cache
- `(block-cache-put! manager label block)` - Put in cache
- `(block-cache-invalidate! manager label)` - Invalidate cache entry
- `(block-cache-clear! manager)` - Clear cache

#### Core Memory

- `(core-memory-get manager)` - Get core memory structure
- `(core-memory-set! manager memory)` - Set core memory structure
- `(core-memory-initialize! manager #!key ...)` - Initialize core memory

#### Block Search

- `(block-search manager query)` - Search by content
- `(block-search-by-label manager pattern)` - Search by label

#### Block Statistics

- `(block-count manager)` - Count total blocks
- `(block-count-standard manager)` - Count standard blocks
- `(block-count-custom manager)` - Count custom blocks
- `(block-total-size manager)` - Calculate total size
- `(block-get-stats manager)` - Get comprehensive statistics

#### Export/Import

- `(block-export manager #!key (format 'json))` - Export blocks
- `(block-import! manager blocks-data)` - Import blocks

#### Block Utilities

- `(block-list-labels manager)` - List all labels
- `(block-get-by-labels manager labels)` - Get blocks by labels
- `(block-copy! manager from-label to-label)` - Copy block

### Memory Types

#### Structures

```scheme
(defstruct memory-block
  (id agent-id label value is-template is-readonly created-at updated-at)
  transparent: #t)

(defstruct core-memory
  (persona human custom)
  transparent: #t)

(defstruct core-memory-manager
  (block-manager history constraints)
  transparent: #t)

(defstruct memory-change
  (timestamp operation block-label old-value new-value metadata)
  transparent: #t)

(defstruct memory-history
  (agent-id changes max-history)
  transparent: #t)

(defstruct memory-constraints
  (max-block-size min-block-size readonly-blocks required-blocks custom-validators)
  transparent: #t)

(defstruct archival-entry
  (id agent-id content embedding importance tags created-at)
  transparent: #t)

(defstruct recall-entry
  (id agent-id content context created-at)
  transparent: #t)

(defstruct memory-stats
  (core-blocks-count archival-entries-count recall-entries-count
   total-memory-size last-updated)
  transparent: #t)
```

#### Validation

- `(valid-block-label? label)` - Check if label is valid
- `(standard-block? label)` - Check if block is standard
- `(validate-block-params params)` - Validate block parameters
- `(validate-archival-params params)` - Validate archival parameters

#### Templates

- `(get-block-template label)` - Get default template
- `(is-template-block? block)` - Check if block is template
- `(is-readonly-block? block)` - Check if block is read-only

#### Core Memory

- `(make-default-core-memory)` - Create default core memory
- `(core-memory-get-block memory label)` - Get block from core memory
- `(core-memory-set-block! memory label value)` - Set block in core memory
- `(core-memory-has-block? memory label)` - Check if block exists
- `(core-memory-list-blocks memory)` - List all block labels

#### Conversion

- `(hash->memory-block h)` - Convert hash to memory block
- `(memory-block->hash block)` - Convert memory block to hash
- `(hash->archival-entry h)` - Convert hash to archival entry
- `(archival-entry->hash entry)` - Convert archival entry to hash

#### Utilities

- `(memory-block-size block)` - Calculate block size
- `(archival-entry-size entry)` - Calculate entry size
- `(calculate-total-memory-size blocks entries)` - Calculate total size

## Data Structures

### Memory Block

```scheme
(hash
 'id "uuid"
 'agent_id "uuid"
 'label "persona"
 'value "You are a helpful AI assistant."
 'is_template #t
 'is_readonly #f
 'created_at 1705401600
 'updated_at 1705401600)
```

### Core Memory

```scheme
(make-core-memory
 persona: "You are a helpful AI assistant."
 human: "User prefers concise responses."
 custom: (hash "preferences" "User likes technical details."))
```

### Archival Entry

```scheme
(hash
 'id "uuid"
 'agent_id "uuid"
 'content "Important information"
 'embedding [0.1, 0.2, ...]  ; 1536 dimensions
 'importance 0.8
 'tags '("important" "user_preference")
 'created_at 1705401600)
```

### Memory Statistics

```scheme
(hash
 'total_blocks 5
 'standard_blocks 2
 'custom_blocks 3
 'total_size 1024)
```

## Features

### Memory Blocks

Structured memory blocks for agent identity:

- **Standard Blocks** - Persona and human blocks
- **Custom Blocks** - User-defined blocks
- **Templates** - Default templates for standard blocks
- **Read-Only** - Protect blocks from modification
- **Validation** - Ensure block consistency

### Core Memory

Stable identity with editable blocks:

- **Persona Block** - Agent's personality and behavior
- **Human Block** - Information about the user
- **Custom Blocks** - Additional memory blocks
- **Atomic Updates** - Update entire core memory at once
- **Block Access** - Get/set individual blocks

### Memory Caching

In-memory cache for performance:

- **Automatic Caching** - Blocks cached on retrieval
- **Cache Invalidation** - Invalidate on update
- **Manual Control** - Clear cache manually
- **Configurable** - Enable/disable caching

### Memory Validation

Comprehensive validation:

- **Parameter Validation** - Validate block parameters
- **Block Validation** - Validate individual blocks
- **Bulk Validation** - Validate all blocks
- **Error Reporting** - Detailed error messages

### Export/Import

Multiple export formats:

- **JSON** - Machine-readable format
- **Text** - Human-readable format
- **Import** - Restore from exported data

## Performance

### Caching Strategy

- **Default**: Caching enabled
- **Cache invalidation**: On update
- **Manual control**: Clear cache anytime

### Database Queries

- **Efficient retrieval**: Single query for all blocks
- **Indexed queries**: Fast label lookup
- **Batch operations**: Update multiple blocks

### Optimization Tips

1. **Enable Caching**: Use cache for frequent access
2. **Batch Updates**: Update multiple blocks at once
3. **Validate Once**: Disable validation for trusted data
4. **Use Templates**: Create from templates when possible
5. **Monitor Size**: Track total memory size

## Integration

### With Database Client

```scheme
(import :gerbil/database/client
        :gerbil/memory/blocks)

;; Connect to database
(db-connect!)

;; Create manager
(def manager (make-block-manager agent-id))

;; Manager uses database client internally
```

### With Agent Core

```scheme
(import :gerbil/agent/core
        :gerbil/memory/blocks)

;; Create agent with memory
(def agent (make-agent ...))
(def manager (make-block-manager (agent-id agent)))

;; Initialize core memory
(core-memory-initialize! manager
                        persona: (agent-persona agent)
                        human: (agent-human-info agent))
```

### With Message Manager

```scheme
(import :gerbil/message/manager
        :gerbil/memory/blocks)

;; Use memory in message context
(def memory (core-memory-get manager))
(def persona (core-memory-persona memory))

;; Include persona in system prompt
(def system-prompt (format "~a\n\nUser info: ~a"
                          persona
                          (core-memory-human memory)))
```

## Error Handling

All operations may throw errors. Use `try/catch` for error handling:

```scheme
(try
 (block-update! manager "readonly-block" "New value")
 (catch (e)
   (displayln (format "Error: ~a" (error-message e)))))
```

## Testing

```scheme
;; Test block creation
(def manager (make-block-manager agent-id))
(def block (block-create! manager "test" "Test value"))
(assert (block-exists? manager "test"))

;; Test block update
(block-update! manager "test" "Updated value")
(def value (block-get-value manager "test"))
(assert (equal? value "Updated value"))

;; Test core memory
(core-memory-initialize! manager)
(def memory (core-memory-get manager))
(assert (core-memory-has-block? memory "persona"))
```

## Future Enhancements

- [ ] Block versioning
- [ ] Block history tracking
- [ ] Block permissions
- [ ] Block encryption
- [ ] Block compression
- [ ] Block templates library
- [ ] Block diff/merge
- [ ] Block search with regex

## License

Part of Project O - Self-Evolving AI Agent Platform
