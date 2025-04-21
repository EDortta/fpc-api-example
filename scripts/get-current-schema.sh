#!/bin/bash

cd ~/Projects/comparer || exit 1

if [ ! -d "venv" ]; then
  echo "Creating virtual environment..."
  python3 -m venv venv || exit 2
fi

source venv/bin/activate
pip install -r requirements.txt

python get-current-schema.py --host local-db --database empresas_db --default-connection-file ../connection.json
deactivate