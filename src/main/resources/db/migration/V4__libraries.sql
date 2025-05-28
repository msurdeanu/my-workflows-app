UPDATE menu_items SET position = 8 WHERE label = 'menu.main.doc-pages';
UPDATE menu_items SET position = 9 WHERE label = 'menu.main.statistics';
UPDATE menu_items SET role = 'ROLE_LOGGED' WHERE label in ('menu.main.workflow-params', 'menu.main.workflow-placeholders', 'menu.main.workflow-development');
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.libs', 'package', 'class://org.myworkflows.view.LibraryView', 'ROLE_LOGGED', '7');
