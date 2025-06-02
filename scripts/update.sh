#!/bin/bash

# Define the repository URLs and target directories
declare -A REPOS
REPOS["brookframework"]="https://github.com/risoflora/brookframework.git"
REPOS["DCPcrypt"]="https://github.com/SnakeDoctor/DCPcrypt.git"
REPOS["zeoslib"]="https://github.com/frones/ZeosLib.git"
REPOS["deployer"]="https://github.com/EDortta/database-structure-comparer.git"
REPOS["indy"]="https://github.com/IndySockets/Indy.git"
REPOS["libpascurl"]="https://github.com/isemenkov/libpascurl.git"
REPOS["libpascurl"]="https://github.com/iLya2IK/libpascurl.git"
REPOS["pascalutils"]="https://github.com/isemenkov/pascalutils"
#REPOS["lNet"]="https://github.com/almindor/lnet.git"

# Define required -Fu and -Fi paths
FU_PATHS=(
  "/home/developer/Projects/indy/Lib/Core/"
  "/home/developer/Projects/indy/Lib/Protocols/"
  "/home/developer/Projects/indy/Lib/System/"
  "/home/developer/Projects/libpascurl/source"
  "/home/developer/Projects/libpascurl/source/curl"
  "/home/developer/Projects/libpascurl/source/curl/response"
  "/home/developer/Projects/libpascurl/source/curl/session"
  "/home/developer/Projects/libpascurl/source/curl/utils/"
  "/home/developer/Projects/libpascurl/source/http/session/"
  "/home/developer/Projects/zeoslib/src"
  "/home/developer/Projects/zeoslib/src/component"
  "/home/developer/Projects/zeoslib/src/core"
  "/home/developer/Projects/zeoslib/src/dbc"
  "/home/developer/Projects/zeoslib/src/parsesql"
  "/home/developer/Projects/zeoslib/src/plain"
  "/home/developer/Projects/zeoslib/src/zcompat"
  "/home/developer/Projects/zeoslib/src/zcore"
  "/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/fcl-db"
  "/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/fcl-net/"
  "/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/iconvenc/"  
  "/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/libcurl"
  "/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/openssl/"
  "/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/rtl-extra/"
  "/usr/share/fpcsrc/3.2.2/packages/fcl-process/src/"
  "/usr/share/fpcsrc/3.2.2/packages/univint/src/"
)

FI_PATHS=(
  "/home/developer/Projects/zeoslib/src"
  "/home/developer/Projects/zeoslib/src/component"
  "/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/"
  "/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/fcl-web/"
  "/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/hash/"
  "/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/rtl/"
  "/usr/share/fpcsrc/3.2.2/packages/"
  "/usr/share/fpcsrc/3.2.2/packages/fcl-base/src/"
  "/usr/share/fpcsrc/3.2.2/packages/fcl-process/src/"
  "/usr/share/fpcsrc/3.2.2/packages/fcl-process/src/unix/"
  "/usr/share/fpcsrc/3.2.2/packages/rtl-generics/src/"
  "/usr/share/fpcsrc/3.2.2/packages/rtl-objpas/src/inc/"
)

FCL_DB_PATH=$(find /usr/lib -type d -name fcl-db 2>/dev/null | grep "units" | head -n 1)
FU_PATHS+=( "$FCL_DB_PATH" )

FPC_CFG="/home/developer/.fpc.cfg"
TEMP_DIR=$(mktemp -d)
echo "Temporary directory created at: $TEMP_DIR"

cd $TEMP_DIR
wget https://security.ubuntu.com/ubuntu/pool/main/o/openssl/libssl1.1_1.1.1f-1ubuntu2.24_amd64.deb
sudo dpkg -i libssl1.1_1.1.1f-1ubuntu2.24_amd64.deb

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

cd ~/Projects/libpascurl
git submodule update --init --recursive


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

sudo chmod 0777 /usr/share/fpcsrc/3.2.2/packages/fcl-process/src

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


