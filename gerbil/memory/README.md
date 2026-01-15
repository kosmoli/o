# Gerbil Memory System

Advanced memory management system for Project O, implementing memos-compatible memory blocks with core memory, archival memory, and recall memory.

## Overview

The memory system provides:
- **Memory Blocks** - Structured memory blocks (persona, human, custom)
- **Core Memory** - Stable identity with editable blocks
- **Archival Memory** - Long-term storage with semantic search
- **Recall Memory** - Short-term context management
- **Memory Validation** - Validate memory parameters and consistency
- **Memory Statistics** - Track memory usage and counts
- **Export/Import** - Export memory to JSON/text formats

## Architecture

```
Application
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
