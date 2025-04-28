#!/bin/bash

cd ~/Projects/deployer || exit 1

rsync -rva ~/Projects/empresas/.deployer-db/* ~/Projects/deployer/dump/

if [ ! -d "venv" ]; then
  echo "Creating virtual environment..."
  python3 -m venv venv || exit 2
fi

source venv/bin/activate
pip install -r requirements.txt

python generate-schema-updates.py local-db:empresas_db remote-db:empresas_db --default-connection-file ../connection.json
deactivate

