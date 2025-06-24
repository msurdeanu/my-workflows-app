--- Workflow Runs
CREATE TABLE workflow_runs
(
    id                   BLOB PRIMARY KEY NOT NULL,
    workflow_template_id INTEGER,
    cache                BLOB,
    printed_keys         TEXT,
    failure_message      TEXT,
    duration             INTEGER          NOT NULL DEFAULT (0),
    created              INTEGER          NOT NULL,
    FOREIGN KEY (workflow_template_id) REFERENCES workflow_templates (id) ON DELETE CASCADE
);
CREATE INDEX workflow_runs_created_index ON workflow_runs (created DESC);

--- Workflow Definitions
CREATE TABLE workflow_definitions
(
    id     INTEGER PRIMARY KEY AUTOINCREMENT,
    name   TEXT NOT NULL,
    script TEXT NOT NULL DEFAULT ''
);

--- Workflow Templates
CREATE TABLE workflow_templates
(
    id      INTEGER PRIMARY KEY AUTOINCREMENT,
    enabled BOOLEAN NOT NULL DEFAULT (1),
    name    TEXT    NOT NULL,
    cron    TEXT
);

CREATE TABLE workflow_templates_workflow_definitions
(
    workflow_template_id   INTEGER NOT NULL,
    workflow_definition_id INTEGER NOT NULL,
    PRIMARY KEY (workflow_template_id, workflow_definition_id),
    FOREIGN KEY (workflow_template_id) REFERENCES workflow_templates (id) ON DELETE CASCADE,
    FOREIGN KEY (workflow_definition_id) REFERENCES workflow_definitions (id) ON DELETE CASCADE
) WITHOUT ROWID;
CREATE INDEX workflow_templates_workflow_definitions_workflow_template_id_index
    ON workflow_templates_workflow_definitions (workflow_template_id);
CREATE INDEX workflow_templates_workflow_definitions_workflow_definition_id_index
    ON workflow_templates_workflow_definitions (workflow_definition_id);

--- Workflow Parameters
CREATE TABLE workflow_parameters
(
    name  TEXT PRIMARY KEY NOT NULL,
    type  TEXT             NOT NULL,
    value TEXT             NOT NULL
) WITHOUT ROWID;

CREATE TABLE workflow_templates_workflow_parameters
(
    workflow_template_id    INTEGER NOT NULL,
    workflow_parameter_name TEXT    NOT NULL,
    PRIMARY KEY (workflow_template_id, workflow_parameter_name),
    FOREIGN KEY (workflow_template_id) REFERENCES workflow_templates (id) ON DELETE CASCADE,
    FOREIGN KEY (workflow_parameter_name) REFERENCES workflow_parameters (name) ON DELETE CASCADE
) WITHOUT ROWID;
CREATE INDEX workflow_templates_workflow_parameters_workflow_template_id_index
    ON workflow_templates_workflow_parameters (workflow_template_id);
CREATE INDEX workflow_templates_workflow_parameters_workflow_parameter_name_index
    ON workflow_templates_workflow_parameters (workflow_parameter_name);

--- Workflow Placeholders
CREATE TABLE workflow_placeholders
(
    name  TEXT PRIMARY KEY NOT NULL,
    value TEXT             NOT NULL
) WITHOUT ROWID;

--- Users
CREATE TABLE users
(
    id       INTEGER PRIMARY KEY AUTOINCREMENT,
    enabled  INTEGER(1) NOT NULL DEFAULT (1) CHECK (enabled IN (0, 1)),
    username TEXT NOT NULL,
    password TEXT NOT NULL,
    token    TEXT NOT NULL,
    admin    INTEGER(1) NOT NULL DEFAULT (0) CHECK (admin IN (0, 1))
);
CREATE UNIQUE INDEX users_username_index ON users (username);
CREATE UNIQUE INDEX users_token_index ON users (token);
INSERT INTO users ("username", "password", "token")
VALUES ('user', '$2a$10$LEPDrW3rJp98sJ96Rb2KgOaGwDJcMdGz0BPcDTRBgM9PhY2g2KmA2', hex(randomblob(32)));
INSERT INTO users ("username", "password", "token", "admin")
VALUES ('admin', '$2a$10$omNibHqZ1p6kx4/bLMNWJ.82c30oAdg0asgGWr9jB9o2zwhim3G7O', hex(randomblob(32)), 1);

--- Menu Items
CREATE TABLE menu_items
(
    label    TEXT PRIMARY KEY NOT NULL,
    icon     TEXT             NOT NULL,
    path     TEXT             NOT NULL,
    role     TEXT                      DEFAULT 'ROLE_GUEST',
    position INTEGER          NOT NULL DEFAULT (0)
) WITHOUT ROWID;
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.workflow-runs', 'lines', 'class://org.myworkflows.view.WorkflowRunView', 'ROLE_GUEST',
        1);
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.workflow-templates', 'cubes', 'class://org.myworkflows.view.WorkflowTemplateView', 'ROLE_GUEST',
        2);
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.workflow-definitions', 'cube', 'class://org.myworkflows.view.WorkflowDefinitionView', 'ROLE_LOGGED',
        3);
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.workflow-params', 'options', 'class://org.myworkflows.view.WorkflowParameterView', 'ROLE_LOGGED',
        4);
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.workflow-placeholders', 'archives', 'class://org.myworkflows.view.WorkflowPlaceholderView',
        'ROLE_LOGGED',
        5);
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.workflow-development', 'code', 'class://org.myworkflows.view.WorkflowDevelopmentView', 'ROLE_LOGGED',
        6);
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.libs', 'package', 'class://org.myworkflows.view.LibraryView', 'ROLE_LOGGED', 7);
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.doc-pages', 'book', 'class://org.myworkflows.view.DocPageView', 'ROLE_GUEST', 8);
INSERT INTO menu_items ("label", "icon", "path", "role", "position")
VALUES ('menu.main.statistics', 'chart', 'class://org.myworkflows.view.StatisticView', 'ROLE_LOGGED', 9);

--- Doc Pages
CREATE TABLE doc_pages
(
    name  TEXT PRIMARY KEY NOT NULL,
    value TEXT             NOT NULL
) WITHOUT ROWID;

INSERT INTO doc_pages ("name", "value")
VALUES ('Markdown Syntax', '# h1 Heading
## h2 Heading
### h3 Heading
#### h4 Heading
##### h5 Heading
###### h6 Heading

## Horizontal Rules

---

## Emphasis

**This is bold text**

*This is italic text*

~~Strikethrough~~

## Blockquotes

> Blockquotes can also be nested...
>> ...by using additional greater-than signs right next to each other...
> > > ...or with spaces between arrows.

## Lists

Unordered

+ Create a list by starting a line with `+`, `-`, or `*`
+ Sub-lists are made by indenting 2 spaces:
  - Marker character change forces new list start:
    * Ac tristique libero volutpat at
    + Facilisis in pretium nisl aliquet
    - Nulla volutpat aliquam velit
+ Very easy!

Ordered

1. Lorem ipsum dolor sit amet
2. Consectetur adipiscing elit
3. Integer molestie lorem at massa

## Code

Inline `code`

Block code "fences"

```
Sample text here...
```

Syntax highlighting

``` js
var foo = function (bar) {
  return bar++;
};

console.log(foo(5));
```

## Tables

| Option | Description |
| ------ | ----------- |
| data   | path to data files to supply the data that will be passed into templates. |
| engine | engine to be used for processing templates. Handlebars is the default. |
| ext    | extension to be used for dest files. |

Right aligned columns

| Option | Description |
| ------:| -----------:|
| data   | path to data files to supply the data that will be passed into templates. |
| engine | engine to be used for processing templates. Handlebars is the default. |
| ext    | extension to be used for dest files. |

## Links

[link text](http://dev.nodeca.com)

[link with title](http://nodeca.github.io/pica/demo/ "title text!")

Autoconverted link https://github.com/nodeca/pica (enable linkify to see)

## Images

![Minion](https://octodex.github.com/images/minion.png)
![Stormtroopocat](https://octodex.github.com/images/stormtroopocat.jpg "The Stormtroopocat")

## Diagrams with Mermaid

```mermaid
sequenceDiagram
Alice->>John: Hello John, how are you?
loop Healthcheck
    John->>John: Fight against hypochondria
end
Note right of John: Rational thoughts!
John-->>Alice: Great!
John->>Bob: How about you?
Bob-->>John: Jolly good!
```

### [Footnotes](https://github.com/markdown-it/markdown-it-footnote)

Footnote 1 link[^first].

Footnote 2 link[^second].

Duplicated footnote reference[^second].

[^first]: Footnote **can have markup**

    and multiple paragraphs.

[^second]: Footnote text.');
