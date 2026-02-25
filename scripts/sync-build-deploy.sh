#!/usr/bin/env bash
set -euo pipefail

BEAMCLAW_DIR="$HOME/beamclaw"
LOG="/tmp/beamclaw_sync.log"
FORK_REMOTE="fork"
UPSTREAM_REMOTE="origin"

log() { echo "$(date -u +%Y-%m-%dT%H:%M:%SZ) $*" | tee -a "$LOG"; }

cd "$BEAMCLAW_DIR"

# 1. Fetch upstream
log "Fetching upstream ($UPSTREAM_REMOTE)..."
git fetch "$UPSTREAM_REMOTE" main 2>&1 | tee -a "$LOG"

# 2. Check if there are new upstream commits
LOCAL_HEAD=$(git rev-parse HEAD)
UPSTREAM_HEAD=$(git rev-parse "$UPSTREAM_REMOTE/main")

if [ "$LOCAL_HEAD" = "$UPSTREAM_HEAD" ]; then
    log "Already up to date with upstream ($LOCAL_HEAD). Nothing to do."
    exit 0
fi

log "New upstream commits: $LOCAL_HEAD -> $UPSTREAM_HEAD"

# 3. Rebase local changes on top of upstream
# Stash any uncommitted local changes before rebasing
STASHED=false
if ! git diff --quiet || ! git diff --cached --quiet; then
    log "Stashing uncommitted local changes..."
    git stash push -m "beamclaw-sync auto-stash" 2>&1 | tee -a "$LOG"
    STASHED=true
fi

log "Rebasing onto upstream..."
if ! git rebase "$UPSTREAM_REMOTE/main" 2>&1 | tee -a "$LOG"; then
    log "ERROR: Rebase failed. Aborting."
    git rebase --abort 2>/dev/null || true
    if [ "$STASHED" = true ]; then git stash pop 2>/dev/null || true; fi
    exit 1
fi

# Restore stashed changes
if [ "$STASHED" = true ]; then
    log "Restoring stashed changes..."
    if ! git stash pop 2>&1 | tee -a "$LOG"; then
        log "WARNING: Stash pop conflict. Changes left in stash."
    fi
fi

NEW_HEAD=$(git rev-parse HEAD)
log "Rebase complete: $NEW_HEAD"

# 4. Build
log "Building release..."
if ! rebar3 release 2>&1 | tee -a "$LOG"; then
    log "ERROR: Build failed."
    exit 1
fi
log "Build successful."

# 5. Run tests
log "Running tests..."
if ! rebar3 eunit 2>&1 | tee -a "$LOG"; then
    log "ERROR: Tests failed. NOT deploying."
    exit 1
fi
log "Tests passed."

# 6. Deploy (restart launchd service)
log "Deploying — stopping beamclaw..."
# Kill the BEAM process directly for clean shutdown
BEAM_PID=$(ps aux | grep "beamclaw.*-sname" | grep -v grep | awk '{print $2}' || true)
if [ -n "$BEAM_PID" ]; then
    kill "$BEAM_PID" 2>/dev/null || true
    sleep 3
    # Force kill if still alive
    kill -9 "$BEAM_PID" 2>/dev/null || true
    sleep 1
fi
# Kill any remaining daemon wrapper
ps aux | grep "beamclaw.*daemon" | grep -v grep | awk '{print $2}' | xargs kill -9 2>/dev/null || true
sleep 2

log "Starting beamclaw via launchd..."
launchctl kickstart "gui/$(id -u)/com.beamclaw" 2>&1 | tee -a "$LOG"
sleep 5

# 7. Health check
if curl -s -m 5 http://localhost:18800/health | grep -q '"ok"'; then
    log "Health check PASSED. Deployment complete."
else
    log "ERROR: Health check FAILED after deploy."
    exit 1
fi

# 8. Push to fork
log "Pushing to fork..."
git push "$FORK_REMOTE" main --force-with-lease 2>&1 | tee -a "$LOG"

log "Sync-build-deploy finished successfully. HEAD=$NEW_HEAD"
