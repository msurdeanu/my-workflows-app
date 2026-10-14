--- A user has as many API tokens as they ask for, each with a name and, if they want one, an expiry. What is kept of
--- a token is its SHA-256 digest: the token itself is shown once, to its user, when it is created.
CREATE TABLE user_tokens
(
    id         INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id    INTEGER NOT NULL,
    name       TEXT    NOT NULL,
    token_hash TEXT    NOT NULL,
    created    INTEGER NOT NULL,
    expires    INTEGER,
    FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE
);
CREATE UNIQUE INDEX user_tokens_token_hash_index ON user_tokens (token_hash);
CREATE UNIQUE INDEX user_tokens_user_id_name_index ON user_tokens (user_id, name);

--- The token every user had so far moves to the new table with the next migration, which is written in Java: SQLite
--- has no function to compute the digest.
