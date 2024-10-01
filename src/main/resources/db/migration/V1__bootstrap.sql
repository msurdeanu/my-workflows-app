CREATE TABLE menu_items
(
    label    TEXT PRIMARY KEY NOT NULL,
    icon     TEXT             NOT NULL,
    path     TEXT             NOT NULL,
    role     TEXT                      DEFAULT 'ROLE_GUEST',
    position INTEGER          NOT NULL DEFAULT (0)
);

INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.workflow-runs', 'lines', 'class://org.myworkflows.view.WorkflowRunView', 'ROLE_GUEST',
        '1');
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.workflow-templates', 'cubes', 'class://org.myworkflows.view.WorkflowTemplateView', 'ROLE_GUEST',
        '2');
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.workflow-definitions', 'cube', 'class://org.myworkflows.view.WorkflowDefinitionView', 'ROLE_LOGGED',
        '3');
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.workflow-params', 'options', 'class://org.myworkflows.view.WorkflowParameterView', 'ROLE_ADMIN',
        '4');
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.workflow-development', 'code', 'class://org.myworkflows.view.WorkflowDevelopmentView', 'ROLE_ADMIN',
        '5');
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.statistics', 'chart', 'class://org.myworkflows.view.StatisticView', 'ROLE_LOGGED', '6');
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.github-myworkflows', 'qrcode', 'https://github.com/msurdeanu/my-workflows-app', 'ROLE_GUEST', '7');

CREATE TABLE workflow_definitions
(
    id     INTEGER PRIMARY KEY AUTOINCREMENT,
    name   TEXT NOT NULL UNIQUE,
    script TEXT NOT NULL DEFAULT ''
);

INSERT INTO workflow_definitions ("name", "script")
VALUES ('Simple script 1',
        '{"name":"Test 1","commands":[{"name":"Just a sleep","@type":"sleep","inputs":[{"name":"sleepTime","value":2000}],"outputs":[{"name":"test","value":2000}]},{"name":"Just a print","@type":"print","inputs":[{"name":"keys","value":["test"]}]}]}');
INSERT INTO workflow_definitions ("name", "script")
VALUES ('Simple script 2',
        '{"name":"Test 2","commands":[{"name":"Just a sleep","@type":"sleep","inputs":[{"name":"sleepTime","value":2000}],"outputs":[{"name":"$$(TEST)","value":2000}]},{"name":"Just a print","@type":"print","inputs":[{"name":"keys","value":["$$(TEST)"]}]}]}');

CREATE TABLE workflow_templates
(
    id      INTEGER PRIMARY KEY AUTOINCREMENT,
    enabled BOOLEAN NOT NULL DEFAULT (1),
    name    TEXT    NOT NULL UNIQUE,
    cron    TEXT    NOT NULL
);

INSERT INTO workflow_templates ("enabled", "name", "cron")
VALUES ('1', 'Simple template', '0 * * * * MON-FRI');

CREATE TABLE workflow_templates_workflow_definitions
(
    id                     INTEGER PRIMARY KEY AUTOINCREMENT,
    workflow_template_id   INTEGER NOT NULL,
    workflow_definition_id INTEGER NOT NULL
);

INSERT INTO workflow_templates_workflow_definitions ("workflow_template_id", "workflow_definition_id")
VALUES (1, 1);

CREATE TABLE workflow_parameters
(
    name  TEXT PRIMARY KEY NOT NULL,
    type  TEXT             NOT NULL,
    value TEXT             NOT NULL
);

INSERT INTO workflow_parameters ("name", "type", "value")
VALUES ('sleepTime', 'int', '5000');

CREATE TABLE workflow_definitions_workflow_parameters
(
    id                      INTEGER PRIMARY KEY AUTOINCREMENT,
    workflow_definition_id  INTEGER NOT NULL,
    workflow_parameter_name TEXT    NOT NULL
);

INSERT INTO workflow_definitions_workflow_parameters ("workflow_definition_id", "workflow_parameter_name")
VALUES (1, 'sleepTime');

CREATE TABLE workflow_runs
(
    id                   BLOB PRIMARY KEY NOT NULL,
    workflow_template_id INTEGER,
    cache                BLOB,
    printed_keys         TEXT,
    failure_message      TEXT,
    duration             INTEGER          NOT NULL DEFAULT (0),
    created              DATETIME         NOT NULL
);

CREATE TABLE users
(
    id       INTEGER PRIMARY KEY AUTOINCREMENT,
    enabled  INTEGER(1) NOT NULL DEFAULT (1),
    username TEXT NOT NULL,
    password TEXT NOT NULL,
    role     TEXT DEFAULT 'ROLE_USER'
);

INSERT INTO users ("username", "password", "role")
VALUES ('user', '$2a$10$LEPDrW3rJp98sJ96Rb2KgOaGwDJcMdGz0BPcDTRBgM9PhY2g2KmA2', 'ROLE_USER');
INSERT INTO users ("username", "password", "role")
VALUES ('admin', '$2a$10$omNibHqZ1p6kx4/bLMNWJ.82c30oAdg0asgGWr9jB9o2zwhim3G7O', 'ROLE_ADMIN');

CREATE UNIQUE INDEX users_username_index ON users (username);

CREATE TABLE placeholders
(
    name  TEXT PRIMARY KEY NOT NULL,
    value TEXT             NOT NULL
);

INSERT INTO placeholders ("name", "value")
VALUES ('TEST', 'test');
