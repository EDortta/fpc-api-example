#!/bin/bash
set -e

cd "$(dirname "$0")"

dos2unix -n /usr/local/bin/.env /tmp/.env

# Load .env file
if [ -f /tmp/.env ]; then
  export $(grep -v '^#' /tmp/.env | xargs)
fi

tries=5
until [ -d "$PROJECT_DIR" ] || [ $tries -eq 0 ]; do
  echo "Waiting for PROJECT_DIR ($PROJECT_DIR) to be available..."
  sleep 1
  tries=$((tries - 1))
done

if [ ! -d "$PROJECT_DIR" ]; then
  echo "Error: PROJECT_DIR not found: $PROJECT_DIR"
  ls -lahrt /home/developer/Projects
  exit 1
fi

echo "Starting watchdog..."
/bin/bash "$WATCHDOG" &

sudo chown -R developer:developer /home/developer/.config

echo "Starting code-server..."
exec code-server --host 0.0.0.0 --port 8080 --auth none --user-data-dir "$VSCODE_CONFIG" "$PROJECT_DIR"
