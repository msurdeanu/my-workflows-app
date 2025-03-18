ALTER TABLE users ADD token TEXT;
UPDATE users SET token = hex(randomblob(32));
CREATE UNIQUE INDEX users_token_index ON users (token);
UPDATE menu_items SET role = 'ROLE_LOGGED' WHERE label = 'menu.main.workflow-development';
