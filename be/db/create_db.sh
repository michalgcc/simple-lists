#!/usr/bin/env bash
set -e

# Set the database file name
DB_FILE="simple-lists.db"

rm -f "$DB_FILE"

# Create the database file
sqlite3 "$DB_FILE" "VACUUM;"

# Apply the schema
shmig -m ./migrations -d "$DB_FILE" -t sqlite3 up

echo "Database creation and schema application successful!"
