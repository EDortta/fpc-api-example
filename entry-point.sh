#!/bin/bash
set -e

cd "$(dirname "$0")"

# Load .env file
if [ -f .env ]; then
  export $(grep -v '^#' .env | xargs)
fi

# Validate important paths
if [ ! -d "$PROJECT_DIR" ]; then
  echo "Error: PROJECT_DIR not found: $PROJECT_DIR"
  exit 1
fi

echo "Starting watchdog..."
/bin/bash "$WATCHDOG" &

sudo chown -R developer:developer /home/developer/.config

echo "Starting code-server..."
exec code-server --host 0.0.0.0 --port 8080 --auth none --user-data-dir "$VSCODE_CONFIG" "$PROJECT_DIR"
