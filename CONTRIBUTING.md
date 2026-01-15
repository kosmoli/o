# Contributing to Project O

Thank you for your interest in contributing to Project O! This document provides guidelines and instructions for contributing.

---

## Table of Contents

1. [Code of Conduct](#code-of-conduct)
2. [Getting Started](#getting-started)
3. [Development Workflow](#development-workflow)
4. [Coding Standards](#coding-standards)
5. [Testing Guidelines](#testing-guidelines)
6. [Documentation](#documentation)
7. [Pull Request Process](#pull-request-process)
8. [Architecture Decisions](#architecture-decisions)

---

## Code of Conduct

### Our Pledge

We are committed to providing a welcoming and inclusive environment for all contributors.

### Our Standards

**Positive behaviors:**
- Using welcoming and inclusive language
- Being respectful of differing viewpoints
- Gracefully accepting constructive criticism
- Focusing on what is best for the project
- Showing empathy towards other contributors

**Unacceptable behaviors:**
- Harassment, trolling, or insulting comments
- Personal or political attacks
- Publishing others' private information
- Other conduct which could reasonably be considered inappropriate

---

## Getting Started

### Prerequisites

1. Read the [GETTING_STARTED.md](GETTING_STARTED.md) guide
2. Set up your development environment
3. Familiarize yourself with the architecture:
   - [ARCHITECTURE_V2.md](docs/ARCHITECTURE_V2.md)
   - [ELIXIR_INTEGRATION.md](docs/ELIXIR_INTEGRATION.md)

### Finding Issues to Work On

- Check the [Issues](https://github.com/your-repo/o/issues) page
- Look for issues labeled `good first issue` or `help wanted`
- Comment on the issue to let others know you're working on it

### Setting Up Your Fork

```bash
# Fork the repository on GitHub, then:
git clone https://github.com/YOUR_USERNAME/o.git
cd o

# Add upstream remote
git remote add upstream https://github.com/original-repo/o.git

# Create a feature branch
git checkout -b feature/your-feature-name
```

---

## Development Workflow

### 1. Create a Feature Branch

```bash
git checkout -b feature/amazing-feature
```

Branch naming conventions:
- `feature/` - New features
- `fix/` - Bug fixes
- `docs/` - Documentation updates
- `refactor/` - Code refactoring
- `test/` - Test additions or fixes
- `chore/` - Maintenance tasks

### 2. Make Your Changes

- Write clean, readable code
- Follow the coding standards (see below)
- Add tests for new functionality
- Update documentation as needed

### 3. Test Your Changes

```bash
# Run all tests
cd o_supervisor
mix test

# Run specific tests
mix test test/your_test.exs

# Check code formatting
mix format --check-formatted

# Run linter
mix credo --strict
```

### 4. Commit Your Changes

```bash
git add .
git commit -m "feat: add amazing feature"
```

**Commit message format:**
```
<type>(<scope>): <subject>

<body>

<footer>
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Test additions or changes
- `chore`: Maintenance tasks

**Examples:**
```
feat(gerbil): add memory search optimization

Implement SIMD-optimized vector search for memory blocks.
This improves search performance by 50%.

Closes #123
```

```
fix(elixir): prevent checkpoint corruption on crash

Add checksum verification before saving checkpoints.

Fixes #456
```

### 5. Push to Your Fork

```bash
git push origin feature/amazing-feature
```

### 6. Create a Pull Request

- Go to GitHub and create a Pull Request
- Fill out the PR template
- Link related issues
- Request review from maintainers

---

## Coding Standards

### Elixir

Follow the [Elixir Style Guide](https://github.com/christopheradams/elixir_style_guide):

```elixir
# Good
defmodule OSupervisor.MyModule do
  @moduledoc """
  Brief description of the module.
  """
  
  use GenServer
  require Logger
  
  # Public API
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  # Callbacks
  
  @impl true
  def init(state) do
    {:ok, state}
  end
  
  # Private functions
  
  defp helper_function(arg) do
    # Implementation
  end
end
```

**Key points:**
- Use 2 spaces for indentation
- Maximum line length: 98 characters
- Add `@moduledoc` and `@doc` for all public modules/functions
- Use `@impl true` for callback implementations
- Group functions: public API, callbacks, private functions
- Use `snake_case` for atoms, functions, and variables
- Use `PascalCase` for module names

### Gerbil Scheme

Follow Scheme conventions:

```scheme
;;; module-name.ss - Brief description
;;;
;;; Detailed description of the module.

(export #t
  public-function-1
  public-function-2)

(import :std/format
        :std/sugar)

;;; Public API

(def (public-function-1 arg1 arg2)
  "Documentation string for the function."
  (let ((result (helper-function arg1)))
    (process-result result arg2)))

;;; Private helpers

(def (helper-function arg)
  ;; Implementation
  (+ arg 1))
```

**Key points:**
- Use 2 spaces for indentation
- Use `kebab-case` for function names
- Add documentation strings for public functions
- Group exports at the top
- Separate public and private functions

### Zig

Follow the [Zig Style Guide](https://ziglang.org/documentation/master/#Style-Guide):

```zig
const std = @import("std");

pub fn myFunction(allocator: std.mem.Allocator, param: []const u8) !void {
    // Implementation
    const result = try allocator.alloc(u8, param.len);
    defer allocator.free(result);
    
    std.mem.copy(u8, result, param);
}
```

**Key points:**
- Use 4 spaces for indentation
- Use `camelCase` for functions and variables
- Use `PascalCase` for types
- Always handle errors explicitly

### Rust

Use `rustfmt` and `clippy`:

```rust
pub fn my_function(param: &str) -> Result<String, Error> {
    // Implementation
    let result = process_param(param)?;
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_my_function() {
        assert_eq!(my_function("test").unwrap(), "expected");
    }
}
```

**Key points:**
- Use 4 spaces for indentation
- Use `snake_case` for functions and variables
- Use `PascalCase` for types
- Always include tests

---

## Testing Guidelines

### Writing Tests

**Elixir:**

```elixir
defmodule OSupervisor.MyModuleTest do
  use ExUnit.Case
  
  setup do
    # Setup code
    {:ok, pid} = OSupervisor.MyModule.start_link([])
    
    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
    end)
    
    {:ok, module: pid}
  end
  
  test "does something correctly", %{module: pid} do
    result = OSupervisor.MyModule.do_something(pid, "input")
    assert result == :expected
  end
  
  test "handles errors gracefully", %{module: pid} do
    assert {:error, :invalid} = 
      OSupervisor.MyModule.do_something(pid, "bad_input")
  end
end
```

**Test coverage requirements:**
- All public functions must have tests
- Aim for >80% code coverage
- Test both success and error cases
- Test edge cases and boundary conditions

### Running Tests

```bash
# All tests
mix test

# Specific file
mix test test/my_module_test.exs

# Specific test
mix test test/my_module_test.exs:42

# With coverage
mix test --cover

# Watch mode
mix test.watch
```

---

## Documentation

### Code Documentation

**Elixir:**

```elixir
@doc """
Brief one-line description.

Longer description with more details about what the function does,
its parameters, and return values.

## Examples

    iex> MyModule.my_function("input")
    {:ok, "output"}

## Parameters

- `param1` - Description of param1
- `param2` - Description of param2

## Returns

- `{:ok, result}` - Success case
- `{:error, reason}` - Error case
"""
def my_function(param1, param2) do
  # Implementation
end
```

**Gerbil:**

```scheme
(def (my-function param1 param2)
  "Brief one-line description.
  
  Longer description with more details.
  
  Parameters:
    param1 - Description
    param2 - Description
  
  Returns:
    Result description
  
  Example:
    (my-function \"input\" 42)
    => \"output\""
  ;; Implementation
  )
```

### Architecture Documentation

When making significant architectural changes:

1. Create an ADR (Architecture Decision Record)
2. Place it in `docs/adr/`
3. Follow the template:

```markdown
# ADR-XXX: Title

## Status
Proposed | Accepted | Deprecated | Superseded

## Context
What is the issue we're facing?

## Decision
What decision did we make?

## Consequences
What are the positive and negative consequences?

## Alternatives Considered
What other options did we consider?
```

---

## Pull Request Process

### Before Submitting

- [ ] Code follows style guidelines
- [ ] Tests pass locally
- [ ] New tests added for new functionality
- [ ] Documentation updated
- [ ] Commit messages follow conventions
- [ ] No merge conflicts with main branch

### PR Template

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## Related Issues
Closes #123

## Testing
Describe how you tested your changes

## Checklist
- [ ] Tests pass
- [ ] Code formatted
- [ ] Documentation updated
- [ ] ADR created (if architectural change)

## Screenshots (if applicable)
```

### Review Process

1. **Automated Checks**: CI/CD runs tests and linters
2. **Code Review**: At least one maintainer reviews
3. **Feedback**: Address review comments
4. **Approval**: Maintainer approves PR
5. **Merge**: Maintainer merges to main

### After Merge

- Delete your feature branch
- Update your local main branch:

```bash
git checkout main
git pull upstream main
git push origin main
```

---

## Architecture Decisions

### When to Create an ADR

Create an ADR when:
- Making significant architectural changes
- Choosing between multiple approaches
- Introducing new technologies or patterns
- Deprecating existing functionality

### ADR Process

1. **Propose**: Create ADR with status "Proposed"
2. **Discuss**: Open issue or PR for discussion
3. **Decide**: Team reviews and decides
4. **Accept**: Update status to "Accepted"
5. **Implement**: Implement the decision
6. **Document**: Update related documentation

---

## Communication

### Channels

- **GitHub Issues**: Bug reports, feature requests
- **GitHub Discussions**: General discussions, questions
- **Pull Requests**: Code reviews, implementation discussions

### Response Times

- **Issues**: We aim to respond within 48 hours
- **PRs**: We aim to review within 1 week
- **Security issues**: Report privately, response within 24 hours

---

## Recognition

Contributors will be recognized in:
- CHANGELOG.md
- README.md (Contributors section)
- Release notes

---

## Questions?

If you have questions:
1. Check existing documentation
2. Search closed issues
3. Ask in GitHub Discussions
4. Open a new issue

---

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

---

**Thank you for contributing to Project O! 🚀**
