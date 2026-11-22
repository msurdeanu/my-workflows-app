--- A file which a run offers to download is copied by its command into the downloads directory, under a directory named
--- after its run. Nothing about it is kept here: like the reports, the files are read from their directory. Only the
--- largest file a run may offer is a setting, in MB.
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('downloadMaxSize', 'settings.download.max-size.label', 'settings.download.max-size.helper', 'int', '500', 1, 16);
