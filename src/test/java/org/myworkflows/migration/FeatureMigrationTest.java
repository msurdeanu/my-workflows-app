package org.myworkflows.migration;

import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Runs the real migrations against a database of its own, the way the application does at startup, with the
 * placeholders {@code application.yml} hands over to Flyway.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class FeatureMigrationTest {

    @TempDir
    private Path tempDir;

    @Test
    public void whenPropertiesTurnedTheRestApiOnAndTheTipsOffThenTheSwitchesKeepIt() throws SQLException {
        // when
        final var url = migrate("restApi", Map.of("restApiEnabled", " TRUE ", "editorTipsEnabled", "false"));

        // then
        assertEquals(List.of("aiModel|str|claude-haiku-5-5", "featureRestApi|bool_h|true", "featureClaude|bool_h|false",
            "featureEditorTips|bool_h|false"), findNewSettings(url));
    }

    @Test
    public void whenPropertiesWereNotSetThenTheSwitchesTakeTheirDefaults() throws SQLException {
        // when the placeholders get the defaults of application.yml
        final var url = migrate("defaults", Map.of("restApiEnabled", "false", "editorTipsEnabled", "true"));

        // then
        assertEquals(List.of("aiModel|str|claude-haiku-5-5", "featureRestApi|bool_h|false", "featureClaude|bool_h|false",
            "featureEditorTips|bool_h|true"), findNewSettings(url));
    }

    @Test
    public void whenMigrationRunsThenTheFeaturesComeRightAfterTheSettingsInTheMenu() throws SQLException {
        // when
        final var url = migrate("menu", Map.of("restApiEnabled", "false", "editorTipsEnabled", "true"));

        // then
        final var labels = query(url, "SELECT label FROM menu_items ORDER BY position");
        assertEquals(labels.indexOf("menu.main.settings") + 1, labels.indexOf("menu.main.features"));
        assertEquals(List.of("ROLE_ADMIN|class://org.myworkflows.view.FeatureView|flag"),
            query(url, "SELECT role || '|' || path || '|' || icon FROM menu_items WHERE label = 'menu.main.features'"));
        // and no two entries share a position
        assertEquals(query(url, "SELECT count(*) FROM menu_items"), query(url, "SELECT count(DISTINCT position) FROM menu_items"));
    }

    private String migrate(String name, Map<String, String> placeholders) {
        final var url = "jdbc:sqlite:" + tempDir.resolve(name + ".db").toAbsolutePath();
        Flyway.configure()
            .dataSource(url, null, null)
            .locations("classpath:db/migration")
            .javaMigrations(new UserTokenHashMigration())
            .placeholders(placeholders)
            .target("13")
            .load()
            .migrate();
        return url;
    }

    private List<String> findNewSettings(String url) throws SQLException {
        return query(url, "SELECT \"key\" || '|' || type || '|' || value FROM settings WHERE position >= 12 ORDER BY position");
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
