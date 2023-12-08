-- Migration: initial
-- Created at: 2023-11-28 10:55:23
-- ====  UP  ====

BEGIN;

	PRAGMA foreign_keys = ON;

	CREATE TABLE entries (
		id INTEGER PRIMARY KEY,
		text TEXT NOT NULL,
		is_done BOOLEAN NOT NULL,
		updated_at TIMESTAMP WITH TIMEZONE NOT NULL
	);

	CREATE INDEX ix_entries_updated_at ON entries (updated_at);

	INSERT INTO Entries (id, text, is_done, updated_at) VALUES (5059536251242152065, 'Example 1', 0, '2023-12-08T14:55:20.044287013Z');
	INSERT INTO Entries (id, text, is_done, updated_at) VALUES (5059536251242152193, 'Example 2', 1, '2023-12-09T14:55:20.044287013Z');
	INSERT INTO Entries (id, text, is_done, updated_at) VALUES (5059536251242152321, 'Example 3', 0, '2023-12-10T14:55:20.044287013Z');

COMMIT;

-- ==== DOWN ====

BEGIN;


COMMIT;