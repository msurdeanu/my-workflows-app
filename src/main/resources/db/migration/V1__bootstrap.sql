CREATE TABLE users
(
    id       INTEGER PRIMARY KEY AUTOINCREMENT,
    enabled  INTEGER (1) NOT NULL DEFAULT (1),
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
