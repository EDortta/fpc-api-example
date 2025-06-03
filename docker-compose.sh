#!/bin/bash
ARCH=$(uname -m)

if [ "$ARCH" = "arm64" ]; then
  echo "Running on Apple Silicon (ARM64)"
  export DB_PLATFORM="linux/amd64"
  export TARGETARCH="arm64"
  export OS_PLATFORM="linux/arm64"
else
  echo "Running on Intel/AMD"
  export DB_PLATFORM="linux/amd64"
  export TARGETARCH="amd64"
  export OS_PLATFORM="linux/amd64"
fi

docker compose -f docker-compose.yml $@
