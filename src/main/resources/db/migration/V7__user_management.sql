ALTER TABLE users ADD COLUMN password_change_required INTEGER(1) NOT NULL DEFAULT 0 CHECK (password_change_required IN (0, 1));

--- The two accounts created by the first migration have the same password on every installation. An account which
--- still has it is asked for a new one at its next login.
UPDATE users
SET password_change_required = 1
WHERE password IN ('$2a$10$LEPDrW3rJp98sJ96Rb2KgOaGwDJcMdGz0BPcDTRBgM9PhY2g2KmA2',
                   '$2a$10$omNibHqZ1p6kx4/bLMNWJ.82c30oAdg0asgGWr9jB9o2zwhim3G7O');

INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.users', 'users', 'class://org.myworkflows.view.UserView', 'ROLE_ADMIN', 9);

UPDATE menu_items SET position = 10 WHERE label = 'menu.main.doc-pages';
UPDATE menu_items SET position = 11 WHERE label = 'menu.main.statistics';
