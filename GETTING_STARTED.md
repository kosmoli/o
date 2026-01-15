# Getting Started with O V2

This guide will help you get O V2 up and running on your local machine.

---

## Prerequisites

### Required Software

1. **Elixir 1.14+** and **Erlang/OTP 25+**
   ```bash
   # macOS
   brew install elixir
   
   # Ubuntu/Debian
   wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
   sudo dpkg -i erlang-solutions_2.0_all.deb
   sudo apt-get update
   sudo apt-get install esl-erlang elixir
   
   # Verify installation
   elixir --version
   erl -version
   ```

2. **Gerbil Scheme**
   ```bash
   # Install Gambit Scheme first
   # macOS
   brew install gambit-scheme
   
   # Ubuntu/Debian
   sudo apt-get install gambit-c
   
   # Install Gerbil
   git clone https://github.com/vyzo/gerbil.git
   cd gerbil/src
   ./configure --prefix=/usr/local
   make
   sudo make install
   
   # Set environment variables
   export GERBIL_HOME=/usr/local/gerbil
   export PATH=$GERBIL_HOME/bin:$PATH
   
   # Verify installation
   gerbil version
   ```

3. **Zig 0.13+** (Optional, for infrastructure layer)
   ```bash
   # macOS
   brew install zig
   
   # Ubuntu/Debian
   snap install zig --classic --beta
   
   # Verify
   zig version
   ```

4. **Rust 1.70+** (Optional, for compute layer)
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   source $HOME/.cargo/env
   rustc --version
   ```

5. **Docker & Docker Compose** (Optional, for containerized deployment)
   ```bash
   # macOS
   brew install --cask docker
   
   # Ubuntu/Debian
   sudo apt-get install docker.io docker-compose
   
   # Verify
   docker --version
   docker-compose --version
   ```

---

## Installation

### Step 1: Clone Repository

```bash
cd ~/work
git clone <repository-url> o
cd o
```

### Step 2: Install Elixir Dependencies

```bash
cd o_supervisor
mix deps.get
mix compile
```

### Step 3: Create Data Directories

```bash
mkdir -p data/{checkpoints,wal,logs}
```

### Step 4: Configure Environment

```bash
# Copy example config
cp config/dev.exs.example config/dev.exs

# Edit configuration
vim config/dev.exs
```

**Key configuration options:**

```elixir
config :o_supervisor,
  gerbil_executable: "/usr/local/bin/gerbil",
  gerbil_main_script: "../gerbil/main.ss",
  checkpoint_dir: "data/checkpoints",
  wal_dir: "data/wal",
  heartbeat_interval: 1000,
  heartbeat_timeout: 5000,
  max_concurrent_shadows: 5
```

---

## Running the System

### Development Mode

#### Option 1: Interactive Shell (Recommended for Development)

```bash
cd o_supervisor
iex -S mix
```

This starts an interactive Elixir shell with the application loaded. You can:

```elixir
# Check status
OSupervisor.status()

# List checkpoints
OSupervisor.list_checkpoints()

# List active shadows
OSupervisor.list_shadows()

# Get version
OSupervisor.version()
```

#### Option 2: Background Process

```bash
cd o_supervisor
mix run --no-halt
```

### Production Mode

#### Build Release

```bash
cd o_supervisor
MIX_ENV=prod mix release
```

#### Run Release

```bash
# Start
_build/prod/rel/o_supervisor/bin/o_supervisor start

# Or as daemon
_build/prod/rel/o_supervisor/bin/o_supervisor daemon

# Check status
_build/prod/rel/o_supervisor/bin/o_supervisor pid

# Stop
_build/prod/rel/o_supervisor/bin/o_supervisor stop
```

### Docker Mode

#### Build and Start

```bash
# Build images
docker-compose build

# Start all services
docker-compose up -d

# View logs
docker-compose logs -f o_supervisor

# Check status
docker-compose ps
```

#### Stop Services

```bash
docker-compose down

# Stop and remove volumes
docker-compose down -v
```

---

## Running Tests

### All Tests

```bash
cd o_supervisor
mix test
```

### Specific Test File

```bash
mix test test/memory_vault_test.exs
```

### With Coverage

```bash
mix test --cover
```

### Watch Mode (Auto-run on file changes)

```bash
mix test.watch
```

### Integration Tests Only

```bash
mix test --only integration
```

### Stress Tests Only

```bash
mix test --only stress
```

---

## Verifying Installation

### 1. Check Elixir Application

```bash
cd o_supervisor
iex -S mix
```

You should see:
```
Erlang/OTP 26 [erts-14.1] [source] [64-bit] [smp:8:8] [ds:8:8:10]

Interactive Elixir (1.15.7) - press Ctrl+C to exit
Starting O Supervisor Application v0.1.0...
Ensured directory exists: data/checkpoints
Ensured directory exists: data/wal
MemoryVault initialized with DETS: data/checkpoints/checkpoints.dets
WALManager initialized, segment: 1
HealthMonitor initialized
EvolutionArbiter initialized
iex(1)>
```

### 2. Check Gerbil Installation

```bash
gerbil version
```

Expected output:
```
Gerbil v0.18.1 on Gambit v4.9.5
```

### 3. Test Gerbil-Elixir Bridge

```bash
cd gerbil
gerbil eval '(import :agent/elixir-bridge) (displayln "Bridge loaded successfully")'
```

### 4. Run Health Check

```bash
# If running in Docker
curl http://localhost:4000/health

# Expected response
{"status":"ok","version":"0.1.0"}
```

---

## Common Issues and Solutions

### Issue 1: Elixir Dependencies Won't Install

**Symptoms:**
```
** (Mix) Could not compile dependency :msgpax
```

**Solution:**
```bash
# Clean and retry
mix deps.clean --all
mix deps.get
mix compile
```

### Issue 2: Gerbil Not Found

**Symptoms:**
```
** (ErlangError) Erlang error: :enoent
```

**Solution:**
```bash
# Verify Gerbil is in PATH
which gerbil

# If not found, add to PATH
export GERBIL_HOME=/usr/local/gerbil
export PATH=$GERBIL_HOME/bin:$PATH

# Or update config
vim o_supervisor/config/dev.exs
# Set: gerbil_executable: "/full/path/to/gerbil"
```

### Issue 3: Port Already in Use

**Symptoms:**
```
** (RuntimeError) could not start application: address already in use
```

**Solution:**
```bash
# Find process using port 4000
lsof -i :4000

# Kill the process
kill -9 <PID>

# Or change port in config
vim config/dev.exs
# Add: port: 4001
```

### Issue 4: DETS File Corruption

**Symptoms:**
```
** (ArgumentError) errors were found at the given arguments: file: :dets.open_file failed
```

**Solution:**
```bash
# Remove corrupted DETS file
rm data/checkpoints/checkpoints.dets

# Restart application
iex -S mix
```

### Issue 5: Docker Build Fails

**Symptoms:**
```
ERROR: failed to solve: process "/bin/sh -c mix deps.get" did not complete successfully
```

**Solution:**
```bash
# Clean Docker cache
docker-compose down
docker system prune -a

# Rebuild
docker-compose build --no-cache
docker-compose up -d
```

---

## Development Workflow

### 1. Make Changes

Edit files in `o_supervisor/lib/` or `gerbil/agent/`

### 2. Reload in IEx

```elixir
# In running IEx session
recompile()

# Or restart specific module
:code.purge(OSupervisor.GerbilManager)
:code.load_file(OSupervisor.GerbilManager)
```

### 3. Run Tests

```bash
# In another terminal
cd o_supervisor
mix test
```

### 4. Format Code

```bash
# Elixir
mix format

# Check formatting
mix format --check-formatted
```

### 5. Run Linter

```bash
# Install Credo (if not already)
mix deps.get

# Run linter
mix credo

# Run strict mode
mix credo --strict
```

---

## Monitoring and Debugging

### View Logs

```bash
# Development
tail -f data/logs/o_supervisor.log

# Docker
docker-compose logs -f o_supervisor

# Specific service
docker-compose logs -f postgres
```

### Access Prometheus

```bash
# Open in browser
open http://localhost:9090

# Query examples:
# - vm_memory_total
# - o_supervisor_health_metrics
# - o_supervisor_checkpoint_created
```

### Access Grafana

```bash
# Open in browser
open http://localhost:3000

# Default credentials:
# Username: admin
# Password: admin
```

### IEx Debugging

```elixir
# Get process info
Process.list() |> length()

# Get supervisor children
Supervisor.which_children(OSupervisor.Supervisor)

# Get GenServer state
:sys.get_state(OSupervisor.MemoryVault)

# Trace messages
:sys.trace(OSupervisor.GerbilManager, true)

# Stop tracing
:sys.trace(OSupervisor.GerbilManager, false)
```

### Observer (GUI)

```elixir
# In IEx
:observer.start()
```

This opens a GUI showing:
- Process tree
- Memory usage
- CPU usage
- ETS tables
- Application tree

---

## Next Steps

1. **Read Architecture Documentation**
   - [ARCHITECTURE_V2.md](docs/ARCHITECTURE_V2.md)
   - [ELIXIR_INTEGRATION.md](docs/ELIXIR_INTEGRATION.md)

2. **Follow Implementation Checklist**
   - [IMPLEMENTATION_CHECKLIST.md](docs/IMPLEMENTATION_CHECKLIST.md)

3. **Review ADRs**
   - [docs/adr/001-elixir-supervision-layer.md](docs/adr/001-elixir-supervision-layer.md)
   - [docs/adr/002-communication-protocol.md](docs/adr/002-communication-protocol.md)
   - [docs/adr/003-checkpoint-strategy.md](docs/adr/003-checkpoint-strategy.md)

4. **Start Implementing Phase 1**
   - Gerbil agent core
   - DSL implementation
   - Memory system
   - Tool framework

---

## Getting Help

- **Documentation**: Check `docs/` directory
- **Issues**: Open an issue on GitHub
- **Discussions**: Use GitHub Discussions
- **Code Examples**: See `examples/` directory (coming soon)

---

## Quick Reference

### Useful Commands

```bash
# Elixir
mix deps.get          # Install dependencies
mix compile           # Compile project
mix test              # Run tests
mix format            # Format code
mix credo             # Run linter
iex -S mix            # Start interactive shell
mix release           # Build release

# Docker
docker-compose up -d          # Start services
docker-compose down           # Stop services
docker-compose logs -f        # View logs
docker-compose ps             # List services
docker-compose restart        # Restart services

# Gerbil
gerbil version               # Check version
gerbil compile file.ss       # Compile file
gerbil run file.ss           # Run file
gerbil eval '(+ 1 2)'       # Evaluate expression
```

### Important Paths

```
o/
├── o_supervisor/           # Elixir application
├── gerbil/                 # Gerbil agent code
├── docs/                   # Documentation
├── data/                   # Runtime data
│   ├── checkpoints/        # State checkpoints
│   ├── wal/                # Write-Ahead Logs
│   └── logs/               # Application logs
└── docker-compose.yml      # Docker orchestration
```

---

**Happy Coding! 🚀**

For questions or issues, please refer to the documentation or open an issue on GitHub.
