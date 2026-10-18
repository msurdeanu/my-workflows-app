--- A report is a file which a command of a run writes into the reports directory, under a directory named after its
--- run. Nothing about it is kept here: like the libraries, the reports are read from their directory. Only the largest
--- report a run may write is a setting, in MB.
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('reportMaxSize', 'settings.report.max-size.label', 'settings.report.max-size.helper', 'int', '5', 1, 11);

--- The page of the reports comes right after the one of the definitions, so everything which followed it moves down by
--- one.
UPDATE menu_items SET position = position + 1 WHERE position >= 4;

INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.workflow-reports', 'newspaper', 'class://org.myworkflows.view.WorkflowReportView', 'ROLE_GUEST', 4);
