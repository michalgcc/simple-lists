-- Migration: initial
-- Created at: 2023-11-28 10:55:23
-- ====  UP  ====

BEGIN;

	PRAGMA foreign_keys = ON;

	CREATE TABLE entries (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		text TEXT,
		is_done BOOLEAN
	);

	INSERT INTO Entries (text, is_done) VALUES ('Example 1', 0);
	INSERT INTO Entries (text, is_done) VALUES ('Example 2', 1);
	INSERT INTO Entries (text, is_done) VALUES ('Example 3', 0);

COMMIT;

-- ==== DOWN ====

BEGIN;

	DROP TABLE entries;

COMMIT;
