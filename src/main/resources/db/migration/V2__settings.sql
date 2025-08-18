CREATE TABLE settings
(
    "key"       TEXT PRIMARY KEY NOT NULL,
    title       TEXT             NOT NULL,
    description TEXT             NOT NULL,
    type        TEXT             NOT NULL,
    value       TEXT             NOT NULL,
    editable    INTEGER          NOT NULL DEFAULT 1,
    position    INTEGER          NOT NULL DEFAULT 0
) WITHOUT ROWID;

INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('version', 'settings.version.label', 'settings.version.helper', 'str', '1.1', 0, 0);
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('name', 'settings.name.label', 'settings.name.helper', 'str', 'My Workflows', 1, 1);
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('url', 'settings.url.label', 'settings.url.helper', 'str', 'https://myworkflows.org', 1, 2);
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('logo', 'settings.logo.label', 'settings.logo.helper', 'str', '/logo.png', 1, 3);
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('encryptionAlgo', 'settings.encryption.algo.label', 'settings.encryption.algo.helper', 'str', 'AES', 1, 4);
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('encryptionSecretKey', 'settings.encryption.secret-key.label', 'settings.encryption.secret-key.helper', 'str', lower(hex(randomblob(32))), 1, 5);
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('workflowRunMaxSize', 'settings.workflow-run.max-size.label', 'settings.workflow-run.max-size.helper', 'int', '250', 1, 6);
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('rememberMeCookieName', 'settings.remember-me.cookie-name.label',
        'settings.remember-me.cookie-name.helper', 'str', 'mw-rm', 1, 7);
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('rememberMeCookieDays', 'settings.remember-me.cookie-days.label',
        'settings.remember-me.cookie-days.helper', 'int', '30', 1, 8);
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('tipsFrequency', 'settings.tips.frequency.label', 'settings.tips.frequency.helper', 'int', '15', 1, 9);
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('tipsCount', 'settings.tips.count.label', 'settings.tips.count.helper', 'int', '23', 0, 10);

CREATE UNIQUE INDEX settings_key_index ON settings ("key");

INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.settings', 'cog', 'class://org.myworkflows.view.SettingView', 'ROLE_ADMIN', 8);

UPDATE menu_items SET position = 9 WHERE label = 'menu.main.doc-pages';
UPDATE menu_items SET position = 10 WHERE label = 'menu.main.statistics';
