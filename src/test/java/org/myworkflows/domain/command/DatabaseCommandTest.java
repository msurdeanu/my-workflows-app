package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.RuntimeEvaluator;
import org.myworkflows.domain.WorkflowRun;

import java.nio.file.Path;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class DatabaseCommandTest {

    @TempDir
    private Path tempDir;

    @Test
    public void whenQueryReturnsRowsThenTheResultSetIsStillReadableAfterTheCommandReturns() throws Exception {
        // given
        final var url = createDatabase("INSERT INTO test (name) VALUES ('a'), ('b')");
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("database.url", url);
        workflowRun.getCache().put("database.query", "SELECT name FROM test ORDER BY name");

        // when
        final var output = runAndCaptureOutput(workflowRun);

        // then the connection is already closed, yet the rows must remain accessible
        assertTrue(output instanceof Optional<?>);
        final var resultSet = (ResultSet) ((Optional<?>) output).orElseThrow();
        assertTrue(resultSet.next());
        assertEquals("a", resultSet.getString("name"));
        assertTrue(resultSet.next());
        assertEquals("b", resultSet.getString("name"));
        assertFalse(resultSet.next());
    }

    @Test
    public void whenQueryContainsStringLiteralsThenItIsNotCorrupted() throws Exception {
        // given
        final var url = createDatabase("INSERT INTO test (name) VALUES ('quoted')");
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("database.url", url);
        workflowRun.getCache().put("database.query", "SELECT name FROM test WHERE name = 'quoted'");

        // when
        final var output = runAndCaptureOutput(workflowRun);

        // then
        final var resultSet = (ResultSet) ((Optional<?>) output).orElseThrow();
        assertTrue(resultSet.next());
        assertEquals("quoted", resultSet.getString("name"));
    }

    @Test
    public void whenQueryReturnsNoRowThenTheOutputIsEmpty() throws Exception {
        // given
        final var url = createDatabase("INSERT INTO test (name) VALUES ('a')");
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("database.url", url);
        workflowRun.getCache().put("database.query", "SELECT name FROM test WHERE name = 'missing'");

        // when
        new DatabaseCommand("db", Set.of(), Set.of(), Set.of(), Set.of()).run(workflowRun);

        // then no output was produced, so no assert or output expression ran
        assertTrue(workflowRun.getCache().find("db.output").isEmpty());
    }

    private Object runAndCaptureOutput(WorkflowRun workflowRun) {
        final var outputs = new LinkedHashSet<ExpressionNameValue>();
        outputs.add(new ExpressionNameValue("db.output", "#output", RuntimeEvaluator.SPEL));
        new DatabaseCommand("db", Set.of(), Set.of(), Set.of(), outputs).run(workflowRun);
        return workflowRun.getCache().get("db.output");
    }

    private String createDatabase(String insertStatement) throws Exception {
        final var url = "jdbc:sqlite:" + tempDir.resolve("test.db").toAbsolutePath();
        try (var connection = DriverManager.getConnection(url);
             var statement = connection.createStatement()) {
            statement.executeUpdate("CREATE TABLE IF NOT EXISTS test (name TEXT)");
            statement.executeUpdate(insertStatement);
        }
        return url;
    }

}
