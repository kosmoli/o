# Multi-stage build for Elixir application
FROM hexpm/elixir:1.15.7-erlang-26.1.2-alpine-3.18.4 AS builder

# Install build dependencies
RUN apk add --no-cache \
    build-base \
    git \
    curl

# Set working directory
WORKDIR /app

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Copy mix files
COPY mix.exs mix.lock ./

# Install dependencies
ENV MIX_ENV=prod
RUN mix deps.get --only prod && \
    mix deps.compile

# Copy application code
COPY lib ./lib
COPY config ./config

# Compile application
RUN mix compile

# Build release
RUN mix release

# Runtime stage
FROM alpine:3.18.4

# Install runtime dependencies
RUN apk add --no-cache \
    openssl \
    ncurses-libs \
    libstdc++ \
    curl

# Install Gerbil Scheme
RUN apk add --no-cache \
    gambit \
    git \
    make \
    gcc \
    musl-dev

# Install Gerbil from source
WORKDIR /tmp
RUN git clone https://github.com/vyzo/gerbil.git && \
    cd gerbil/src && \
    ./configure --prefix=/usr/local && \
    make && \
    make install && \
    cd / && rm -rf /tmp/gerbil

# Create app user
RUN addgroup -g 1000 o && \
    adduser -D -u 1000 -G o o

# Set working directory
WORKDIR /app

# Copy release from builder
COPY --from=builder --chown=o:o /app/_build/prod/rel/o_supervisor ./

# Create data directories
RUN mkdir -p data/checkpoints data/wal data/logs && \
    chown -R o:o data

# Switch to app user
USER o

# Expose ports
EXPOSE 4000 9568

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
    CMD curl -f http://localhost:4000/health || exit 1

# Start application
CMD ["bin/o_supervisor", "start"]
