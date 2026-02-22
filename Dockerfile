## ---- Stage 1: Build --------------------------------------------------------
FROM erlang:28-alpine AS builder

RUN apk add --no-cache wget git

# Download rebar3
RUN wget -q https://s3.amazonaws.com/rebar3/rebar3 -O /usr/local/bin/rebar3 \
 && chmod +x /usr/local/bin/rebar3

WORKDIR /build

# Fetch deps first so this layer is cached unless rebar.config / rebar.lock change.
COPY rebar.config rebar.lock ./
RUN rebar3 get-deps

# Build a self-contained OTP release using the docker profile (TUI disabled).
COPY apps/ apps/
COPY config/ config/
RUN rebar3 as docker release

## ---- Stage 2: Minimal runtime ----------------------------------------------
FROM alpine:3.23

# Erlang runtime C-library dependencies only — no Erlang package needed
# because the OTP release from stage 1 bundles its own ERTS.
RUN apk add --no-cache ncurses-libs openssl libstdc++ libgcc

# Non-root user for principle-of-least-privilege
RUN addgroup -S beamclaw && adduser -S beamclaw -G beamclaw

# Copy the self-contained OTP release (includes ERTS, no other Erlang needed)
COPY --from=builder --chown=beamclaw:beamclaw \
     /build/_build/docker/rel/beamclaw /opt/beamclaw

USER beamclaw
WORKDIR /opt/beamclaw

# HTTP gateway (configurable via sys.docker.config)
EXPOSE 18800

HEALTHCHECK --interval=30s --timeout=5s --start-period=15s --retries=3 \
    CMD wget -qO- http://localhost:18800/health || exit 1

# Run the node in foreground so Docker can capture stdout and manage lifecycle.
# Secrets are injected via -e flags at runtime — never baked into the image.
ENTRYPOINT ["/opt/beamclaw/bin/beamclaw", "foreground"]
