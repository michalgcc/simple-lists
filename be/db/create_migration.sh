#!/bin/sh

migration_name=$1

if [[ -z "$migration_name" ]]; then
    echo "Error: Migration name is required"
    exit 1
fi

shmig -t sqlite3 -d test.db create $migration_name
