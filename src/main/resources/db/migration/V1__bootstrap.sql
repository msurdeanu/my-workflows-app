CREATE TABLE placeholders (
    name TEXT PRIMARY KEY NOT NULL,
    value TEXT NOT NULL
);

INSERT INTO placeholders ("name", "value")
VALUES ('TEST', 'test');
