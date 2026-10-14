--- The page which exports and imports templates, definitions, parameters and placeholders comes right after the
--- libraries, so everything which followed them moves down by one.
UPDATE menu_items SET position = position + 1 WHERE position >= 8;

INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.transfer', 'exchange', 'class://org.myworkflows.view.WorkflowTransferView', 'ROLE_ADMIN', 8);
