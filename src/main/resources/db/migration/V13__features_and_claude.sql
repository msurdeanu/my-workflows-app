--- The model Claude is called with unless a command names its own. The key never comes from here: it is read from the
--- ANTHROPIC_API_KEY environment variable of the server, as the settings are plain text in the database.
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('aiModel', 'settings.ai.model.label', 'settings.ai.model.helper', 'str', 'claude-haiku-5-5', 1, 12);

--- What an admin turns on or off on the features page, while the application runs. They are hidden settings, which
--- the settings page does not show. The REST API and the editor tips were properties of the application until now,
--- read once at startup: an installation which set one keeps its value (see spring.flyway.placeholders), and from
--- then on the page decides.
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('featureRestApi', 'features.rest-api.name', 'features.rest-api.description', 'bool_h',
        CASE WHEN lower(trim('${restApiEnabled}')) = 'true' THEN 'true' ELSE 'false' END, 1, 13);
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('featureClaude', 'features.claude.name', 'features.claude.description', 'bool_h', 'false', 1, 14);
INSERT INTO settings ("key", "title", "description", "type", "value", "editable", "position")
VALUES ('featureEditorTips', 'features.editor-tips.name', 'features.editor-tips.description', 'bool_h',
        CASE WHEN lower(trim('${editorTipsEnabled}')) = 'false' THEN 'false' ELSE 'true' END, 1, 15);

--- The page of the features comes right after the one of the settings, so everything which followed it moves down by
--- one.
UPDATE menu_items SET position = position + 1
WHERE position > (SELECT position FROM menu_items WHERE label = 'menu.main.settings');

INSERT INTO menu_items ("label", "icon", "path", "role", "position")
SELECT 'menu.main.features', 'flag', 'class://org.myworkflows.view.FeatureView', 'ROLE_ADMIN', position + 1
FROM menu_items
WHERE label = 'menu.main.settings';
