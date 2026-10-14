package org.myworkflows.migration;

import org.flywaydb.core.api.MigrationVersion;
import org.flywaydb.core.api.migration.Context;
import org.flywaydb.core.api.migration.JavaMigration;
import org.springframework.stereotype.Component;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The ninth migration, the one which is not a script of {@code db/migration}: it moves the token every user had so
 * far from {@code users.token}, where it was readable, to {@code user_tokens}, which keeps its SHA-256 digest - and
 * SQLite cannot compute one. Such a token keeps working, under the name {@value #TOKEN_NAME} and without an expiry,
 * but it cannot be shown anymore.
 *
 * <p>Flyway gets this migration from Spring, as a bean. It uses no class of the application on purpose: it has to do
 * tomorrow what it does today, whatever happens to them.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Component
public final class UserTokenHashMigration implements JavaMigration {

    public static final String TOKEN_NAME = "default";

    @Override
    public MigrationVersion getVersion() {
        return MigrationVersion.fromVersion("9");
    }

    @Override
    public String getDescription() {
        return "user token hash";
    }

    @Override
    public Integer getChecksum() {
        return null;
    }

    @Override
    public boolean canExecuteInTransaction() {
        return true;
    }

    @Override
    public void migrate(Context context) throws Exception {
        final var connection = context.getConnection();
        final var tokens = findTokens(connection);
        final var digest = MessageDigest.getInstance("SHA-256");
        final var created = System.currentTimeMillis();
        try (var insert = connection.prepareStatement("INSERT INTO user_tokens (user_id, name, token_hash, created) VALUES (?, ?, ?, ?)")) {
            for (var token : tokens.entrySet()) {
                insert.setInt(1, token.getKey());
                insert.setString(2, TOKEN_NAME);
                insert.setString(3, HexFormat.of().formatHex(digest.digest(token.getValue().getBytes(StandardCharsets.UTF_8))));
                insert.setLong(4, created);
                insert.executeUpdate();
            }
        }
        try (var statement = connection.createStatement()) {
            // a column cannot be dropped while an index is built on it
            statement.execute("DROP INDEX users_token_index");
            statement.execute("ALTER TABLE users DROP COLUMN token");
        }
    }

    private Map<Integer, String> findTokens(Connection connection) throws SQLException {
        final var tokens = new LinkedHashMap<Integer, String>();
        try (var statement = connection.createStatement(); var rows = statement.executeQuery("SELECT id, token FROM users ORDER BY id")) {
            while (rows.next()) {
                tokens.put(rows.getInt(1), rows.getString(2));
            }
        }
        return tokens;
    }

}
