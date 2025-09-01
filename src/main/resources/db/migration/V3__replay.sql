ALTER TABLE workflow_runs ADD COLUMN last_successful_index INTEGER DEFAULT -1;
ALTER TABLE workflow_runs ADD COLUMN script TEXT;
UPDATE settings SET value = '1.2' WHERE "key" = 'version';