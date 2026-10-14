package org.myworkflows.migration;

import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.myworkflows.domain.UserToken;

import java.nio.file.Path;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Runs the real migrations against a database of its own, the way the application does at startup.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class UserTokenHashMigrationTest {

    @TempDir
    private Path tempDir;

    @Test
    public void whenMigrationRunsThenEveryUserKeepsTheirTokenAsADigest() throws SQLException {
        // given a database as the previous version left it, with a token readable on every user
        final var url = "jdbc:sqlite:" + tempDir.resolve("test.db").toAbsolutePath();
        migrate(url, "8");
        final var tokens = findTokens(url);
        assertEquals(2, tokens.size());

        // when
        migrate(url, "9");

        // then
        final var rows = query(url, "SELECT user_id || '|' || name || '|' || token_hash || '|' || typeof(created) || '|' || ifnull(expires, 'never') "
            + "FROM user_tokens ORDER BY user_id");
        final var expectedRows = new ArrayList<String>();
        tokens.forEach((userId, token) -> expectedRows.add(userId + "|default|" + UserToken.hashOf(token) + "|integer|never"));
        // a call which carries the old token is matched by the digest the application computes for it
        assertEquals(expectedRows, rows);
        // and the token itself is not kept anywhere
        assertFalse(query(url, "SELECT name FROM pragma_table_info('users')").contains("token"));
        assertTrue(query(url, "SELECT name FROM pragma_index_list('users')").stream().noneMatch(name -> name.contains("token")));
        assertEquals(List.of("2"), query(url, "SELECT count(*) FROM users"));
    }

    private void migrate(String url, String target) {
        Flyway.configure()
            .dataSource(url, null, null)
            .locations("classpath:db/migration")
            .javaMigrations(new UserTokenHashMigration())
            .target(target)
            .load()
            .migrate();
    }

    private Map<Integer, String> findTokens(String url) throws SQLException {
        final var tokens = new LinkedHashMap<Integer, String>();
        try (var connection = DriverManager.getConnection(url);
             var statement = connection.createStatement();
             var rows = statement.executeQuery("SELECT id, token FROM users ORDER BY id")) {
            while (rows.next()) {
                tokens.put(rows.getInt(1), rows.getString(2));
            }
        }
        return tokens;
    }

    private List<String> query(String url, String sql) throws SQLException {
        final var values = new ArrayList<String>();
        try (var connection = DriverManager.getConnection(url);
             var statement = connection.createStatement();
             var rows = statement.executeQuery(sql)) {
            while (rows.next()) {
                values.add(rows.getString(1));
            }
        }
        return values;
    }

}
