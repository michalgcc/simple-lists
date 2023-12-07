#!/bin/sh
mkdir -p /tmp

if [ -n "$DB_CONN_STRING" ] && [ ! -f "$DB_CONN_STRING" ]; then
    echo "Database not found. Creating new database."
    sqlite3 "$DB_CONN_STRING" "VACUUM;"
fi

echo "Applying migrations"
shmig -m ./migrations -d "$DB_CONN_STRING" -t sqlite3 up

./simple-lists-exe

