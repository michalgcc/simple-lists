-- Migration: add_lists
-- Created at: 2023-12-08 14:52:42
-- ====  UP  ====

BEGIN;

	PRAGMA foreign_keys = OFF;

	CREATE TABLE lists (
		id INTEGER PRIMARY KEY,
		name TEXT NOT NULL,
		updated_at TIMESTAMP WITH TIMEZONE NOT NULL
	);

	CREATE INDEX ix_lists_updated_at ON lists (updated_at);

	INSERT INTO lists (id, name, updated_at) VALUES (5059536251242152449, 'Default list', '2023-12-08T14:55:20.044287013Z');

	ALTER TABLE entries ADD COLUMN list_id INTEGER NOT NULL DEFAULT 5059536251242152449 REFERENCES lists(id) ON DELETE CASCADE;

	CREATE INDEX ix_entries_list_id ON entries (list_id);

	PRAGMA foreign_keys = ON;

COMMIT;

-- ==== DOWN ====

BEGIN;


COMMIT;
