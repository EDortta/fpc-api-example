#!/bin/bash

# Define the repository URLs and target directories
declare -A REPOS
REPOS["brookframework"]="https://github.com/risoflora/brookframework.git"
REPOS["DCPcrypt"]="https://github.com/SnakeDoctor/DCPcrypt.git"
REPOS["zeoslib"]="https://github.com/frones/ZeosLib.git"
REPOS["comparer"]="https://github.com/EDortta/database-structure-comparer.git"

# Define required -Fu and -Fi paths
FU_PATHS=(
  "/home/developer/Projects/zeoslib/src"
  "/home/developer/Projects/zeoslib/src/component"
  "/home/developer/Projects/zeoslib/src/dbc"
  "/home/developer/Projects/zeoslib/src/plain"
  "/home/developer/Projects/zeoslib/src/zcompat"
  "/home/developer/Projects/zeoslib/src/zcore"
  "/home/developer/Projects/zeoslib/src/core"
  "/home/developer/Projects/zeoslib/src/parsesql"
)

FI_PATHS=(
  "/home/developer/Projects/zeoslib/src"
  "/home/developer/Projects/zeoslib/src/component"
)

FCL_DB_PATH=$(find /usr/lib -type d -name fcl-db 2>/dev/null | grep "units" | head -n 1)
FU_PATHS+=( "$FCL_DB_PATH" )

FPC_CFG="/home/developer/.fpc.cfg"
TEMP_DIR=$(mktemp -d)
echo "Temporary directory created at: $TEMP_DIR"

sudo chmod 0777 /home/developer/Projects
sudo chown -fR developer:developer /home/developer/Projects

# Clone and unzip libraries
for REPO_NAME in "${!REPOS[@]}"; do
  echo "Cloning $REPO_NAME..."
  git clone --depth 1 "${REPOS[$REPO_NAME]}" "$TEMP_DIR/$REPO_NAME"
  cd "$TEMP_DIR/$REPO_NAME" || exit

  echo "Creating $REPO_NAME.zip"
  git archive --format=zip -o "$REPO_NAME.zip" HEAD

  echo "Installing to /home/developer/Projects/$REPO_NAME"
  cd /home/developer/Projects/ || exit
  mkdir -p "$REPO_NAME"
  unzip -o "$TEMP_DIR/$REPO_NAME/$REPO_NAME.zip" -d "$REPO_NAME"
  rm -rf "$TEMP_DIR/$REPO_NAME"
done

echo ""
echo "Ensuring -Fu and -Fi paths exist in: $FPC_CFG"
touch "$FPC_CFG"

# Ensure -Fu entries
for P in "${FU_PATHS[@]}"; do
  LINE="-Fu$P"
  if grep -qE "^$LINE$" "$FPC_CFG"; then
    echo "Path already in .fpc.cfg: $LINE"
  else
    echo "Adding $LINE to .fpc.cfg"
    echo "$LINE" >> "$FPC_CFG"
  fi
done

# Ensure -Fi entries
for P in "${FI_PATHS[@]}"; do
  LINE="-Fi$P"
  if grep -qE "^$LINE$" "$FPC_CFG"; then
    echo "Path already in .fpc.cfg: $LINE"
  else
    echo "Adding $LINE to .fpc.cfg"
    echo "$LINE" >> "$FPC_CFG"
  fi
done


echo ""
echo "All Pascal libraries updated and .fpc.cfg configured."


echo ""
echo "Checking if 'empresas_db' exists inside MySQL..."

mysql -u dev-empresas -pdevpass123 -h db -e "CREATE DATABASE IF NOT EXISTS empresas_db CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"

if [ $? -eq 0 ]; then
    echo "Database 'empresas_db' ensured."

    echo "Checking if table 'empresas' exists in 'empresas_db'..."

    mysql -u dev-empresas -pdevpass123 -h db empresas_db -e "
    CREATE TABLE IF NOT EXISTS \`empresas\` (
    \`empresa_id\` char(48) COLLATE utf8mb4_unicode_ci NOT NULL,
    \`cnpj\` varchar(18) COLLATE utf8mb4_unicode_ci NOT NULL,
    \`nome_fantasia\` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
    \`razao_social\` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
    PRIMARY KEY (\`empresa_id\`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;"


else
  echo "Failed to create or connect to MySQL inside container 'db'."
fi


