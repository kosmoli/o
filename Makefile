# Makefile for Project O

.PHONY: help setup install compile test format lint clean docker-build docker-up docker-down docs

# Default target
.DEFAULT_GOAL := help

# Colors for output
BLUE := \033[0;34m
GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
NC := \033[0m # No Color

## help: Show this help message
help:
	@echo "$(BLUE)Project O - Makefile Commands$(NC)"
	@echo ""
	@echo "$(GREEN)Setup & Installation:$(NC)"
	@echo "  make setup          - Initial project setup"
	@echo "  make install        - Install all dependencies"
	@echo ""
	@echo "$(GREEN)Development:$(NC)"
	@echo "  make compile        - Compile Elixir code"
	@echo "  make test           - Run all tests"
	@echo "  make test-watch     - Run tests in watch mode"
	@echo "  make format         - Format code"
	@echo "  make lint           - Run linters"
	@echo "  make clean          - Clean build artifacts"
	@echo ""
	@echo "$(GREEN)Running:$(NC)"
	@echo "  make dev            - Start development server"
	@echo "  make iex            - Start interactive shell"
	@echo "  make release        - Build production release"
	@echo ""
	@echo "$(GREEN)Docker:$(NC)"
	@echo "  make docker-build   - Build Docker images"
	@echo "  make docker-up      - Start Docker services"
	@echo "  make docker-down    - Stop Docker services"
	@echo "  make docker-logs    - View Docker logs"
	@echo ""
	@echo "$(GREEN)Documentation:$(NC)"
	@echo "  make docs           - Generate documentation"
	@echo "  make docs-serve     - Serve documentation locally"
	@echo ""
	@echo "$(GREEN)Utilities:$(NC)"
	@echo "  make check          - Run all checks (format, lint, test)"
	@echo "  make ci             - Run CI pipeline locally"

## setup: Initial project setup
setup:
	@echo "$(BLUE)Setting up Project O...$(NC)"
	@mkdir -p data/checkpoints data/wal data/logs
	@echo "$(GREEN)✓ Created data directories$(NC)"
	@cd o_supervisor && mix local.hex --force && mix local.rebar --force
	@echo "$(GREEN)✓ Installed Hex and Rebar$(NC)"
	@echo "$(GREEN)✓ Setup complete!$(NC)"

## install: Install all dependencies
install:
	@echo "$(BLUE)Installing dependencies...$(NC)"
	@cd o_supervisor && mix deps.get
	@echo "$(GREEN)✓ Elixir dependencies installed$(NC)"

## compile: Compile Elixir code
compile:
	@echo "$(BLUE)Compiling Elixir code...$(NC)"
	@cd o_supervisor && mix compile
	@echo "$(GREEN)✓ Compilation complete$(NC)"

## test: Run all tests
test:
	@echo "$(BLUE)Running tests...$(NC)"
	@cd o_supervisor && mix test
	@echo "$(GREEN)✓ Tests complete$(NC)"

## test-watch: Run tests in watch mode
test-watch:
	@echo "$(BLUE)Starting test watcher...$(NC)"
	@cd o_supervisor && mix test.watch

## test-coverage: Run tests with coverage
test-coverage:
	@echo "$(BLUE)Running tests with coverage...$(NC)"
	@cd o_supervisor && mix test --cover
	@echo "$(GREEN)✓ Coverage report generated$(NC)"

## format: Format code
format:
	@echo "$(BLUE)Formatting code...$(NC)"
	@cd o_supervisor && mix format
	@echo "$(GREEN)✓ Code formatted$(NC)"

## format-check: Check code formatting
format-check:
	@echo "$(BLUE)Checking code formatting...$(NC)"
	@cd o_supervisor && mix format --check-formatted

## lint: Run linters
lint:
	@echo "$(BLUE)Running linters...$(NC)"
	@cd o_supervisor && mix credo --strict
	@echo "$(GREEN)✓ Linting complete$(NC)"

## clean: Clean build artifacts
clean:
	@echo "$(BLUE)Cleaning build artifacts...$(NC)"
	@cd o_supervisor && mix clean
	@rm -rf o_supervisor/_build
	@rm -rf o_supervisor/deps
	@rm -rf data/checkpoints/*.ckpt
	@rm -rf data/wal/*.log
	@echo "$(GREEN)✓ Clean complete$(NC)"

## dev: Start development server
dev:
	@echo "$(BLUE)Starting development server...$(NC)"
	@cd o_supervisor && mix run --no-halt

## iex: Start interactive shell
iex:
	@echo "$(BLUE)Starting interactive shell...$(NC)"
	@cd o_supervisor && iex -S mix

## release: Build production release
release:
	@echo "$(BLUE)Building production release...$(NC)"
	@cd o_supervisor && MIX_ENV=prod mix release
	@echo "$(GREEN)✓ Release built successfully$(NC)"
	@echo "$(YELLOW)Run: _build/prod/rel/o_supervisor/bin/o_supervisor start$(NC)"

## docker-build: Build Docker images
docker-build:
	@echo "$(BLUE)Building Docker images...$(NC)"
	@docker-compose build
	@echo "$(GREEN)✓ Docker images built$(NC)"

## docker-up: Start Docker services
docker-up:
	@echo "$(BLUE)Starting Docker services...$(NC)"
	@docker-compose up -d
	@echo "$(GREEN)✓ Services started$(NC)"
	@echo "$(YELLOW)View logs: make docker-logs$(NC)"

## docker-down: Stop Docker services
docker-down:
	@echo "$(BLUE)Stopping Docker services...$(NC)"
	@docker-compose down
	@echo "$(GREEN)✓ Services stopped$(NC)"

## docker-logs: View Docker logs
docker-logs:
	@docker-compose logs -f

## docker-ps: List Docker services
docker-ps:
	@docker-compose ps

## docker-restart: Restart Docker services
docker-restart:
	@echo "$(BLUE)Restarting Docker services...$(NC)"
	@docker-compose restart
	@echo "$(GREEN)✓ Services restarted$(NC)"

## docs: Generate documentation
docs:
	@echo "$(BLUE)Generating documentation...$(NC)"
	@cd o_supervisor && mix docs
	@echo "$(GREEN)✓ Documentation generated$(NC)"
	@echo "$(YELLOW)Open: o_supervisor/doc/index.html$(NC)"

## docs-serve: Serve documentation locally
docs-serve: docs
	@echo "$(BLUE)Serving documentation at http://localhost:8000$(NC)"
	@cd o_supervisor/doc && python3 -m http.server 8000

## check: Run all checks
check: format-check lint test
	@echo "$(GREEN)✓ All checks passed!$(NC)"

## ci: Run CI pipeline locally
ci:
	@echo "$(BLUE)Running CI pipeline...$(NC)"
	@make clean
	@make install
	@make compile
	@make format-check
	@make lint
	@make test-coverage
	@echo "$(GREEN)✓ CI pipeline complete!$(NC)"

## benchmark: Run performance benchmarks
benchmark:
	@echo "$(BLUE)Running benchmarks...$(NC)"
	@cd o_supervisor && mix run benchmarks/run.exs
	@echo "$(GREEN)✓ Benchmarks complete$(NC)"

## dialyzer: Run Dialyzer type checker
dialyzer:
	@echo "$(BLUE)Running Dialyzer...$(NC)"
	@cd o_supervisor && mix dialyzer
	@echo "$(GREEN)✓ Dialyzer complete$(NC)"

## security: Run security checks
security:
	@echo "$(BLUE)Running security checks...$(NC)"
	@cd o_supervisor && mix deps.audit
	@echo "$(GREEN)✓ Security checks complete$(NC)"

## version: Show version information
version:
	@echo "$(BLUE)Project O Version Information$(NC)"
	@echo "Elixir: $$(elixir --version | head -1)"
	@echo "Erlang: $$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)"
	@echo "Gerbil: $$(gerbil version 2>/dev/null || echo 'Not installed')"
	@echo "Zig: $$(zig version 2>/dev/null || echo 'Not installed')"
	@echo "Rust: $$(rustc --version 2>/dev/null || echo 'Not installed')"

## status: Show project status
status:
	@echo "$(BLUE)Project O Status$(NC)"
	@echo ""
	@echo "$(GREEN)Data Directories:$(NC)"
	@ls -lh data/ 2>/dev/null || echo "  No data directory"
	@echo ""
	@echo "$(GREEN)Docker Services:$(NC)"
	@docker-compose ps 2>/dev/null || echo "  Docker not running"
	@echo ""
	@echo "$(GREEN)Recent Checkpoints:$(NC)"
	@ls -lt data/checkpoints/*.ckpt 2>/dev/null | head -5 || echo "  No checkpoints"

## init: Initialize new development environment
init: setup install compile
	@echo "$(GREEN)✓ Development environment initialized!$(NC)"
	@echo "$(YELLOW)Next steps:$(NC)"
	@echo "  1. Review docs/GETTING_STARTED.md"
	@echo "  2. Run: make iex"
	@echo "  3. Start coding!"

.PHONY: all
all: check
