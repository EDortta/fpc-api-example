#!/bin/bash
set -e

# Optional: define paths
PROJECT_DIR="/home/developer/Projects"
SCRIPTS_DIR="/home/developer/scripts"
WATCHDOG="$SCRIPTS_DIR/watchdog.sh"
VSCODE_CONFIG="/home/developer/.config"

# Launch watchdog in background
if [ -x "$WATCHDOG" ]; then
  echo "Starting watchdog..."
  "$WATCHDOG" &
else
  echo "Watchdog script not found or not executable: $WATCHDOG"
fi

sudo chown -R developer:developer /home/developer/.config

echo "Starting code-server..."
exec code-server --host 0.0.0.0 --port 8080 --auth none --user-data-dir "$VSCODE_CONFIG" "$PROJECT_DIR"
