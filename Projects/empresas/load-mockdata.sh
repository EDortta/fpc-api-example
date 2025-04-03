#!/bin/bash

MYSQL_USER="root"
MYSQL_PASS="czyhnp"
MYSQL_HOST="db"

MOCKDATA_DIR="./mockdata"

echo "Checking and loading mock data into MySQL..."

for file in "$MOCKDATA_DIR"/*.json; do
  [ -e "$file" ] || continue  # skip if no .json files
  table=$(basename "$file" .json)

  echo "Table: $table"

  count=$(mysql --defaults-extra-file=./.my.cnf -N -s -e "SELECT COUNT(*) FROM $table;" empresas_db 2>/dev/null)

  if [[ "$count" =~ ^[0-9]+$ && "$count" -gt 0 ]]; then
    echo "Table '$table' already has $count records. Skipping."
    continue
  fi

  echo "Table '$table' is empty. Inserting mock data..."

  # Prepare insert commands via jq
  jq -c ".[]" "$file" | while read -r row; do
    empresa_id=$(echo "$row" | jq -r '.empresa_id')
    cnpj=$(echo "$row" | jq -r '.cnpj')
    nome_fantasia=$(echo "$row" | jq -r '.nome_fantasia // empty')
    razao_social=$(echo "$row" | jq -r '.razao_social // empty')

    echo "Empresa ID: $empresa_id"

    # Escape values for SQL
    nome_fantasia=$(printf '%s\n' "$nome_fantasia" | sed "s/'/\\\'/g")
    razao_social=$(printf '%s\n' "$razao_social" | sed "s/'/\\\'/g")

    # Build SQL insert
    sql="INSERT INTO $table (empresa_id, cnpj, nome_fantasia, razao_social) VALUES (
      '$empresa_id', '$cnpj', '$nome_fantasia', '$razao_social'
    );"

    # Execute SQL
    mysql --defaults-extra-file=./.my.cnf empresas_db -e "$sql" > /dev/null
  done

  echo "Mock data inserted into '$table'."
done

echo "Mock data loading complete."

