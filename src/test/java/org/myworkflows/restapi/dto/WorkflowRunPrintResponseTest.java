package org.myworkflows.restapi.dto;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRunPrint;

import java.math.BigDecimal;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.sql.rowset.RowSetProvider;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Mihai Surdeanu
 * @since 2.2
 */
public final class WorkflowRunPrintResponseTest {

    @Test
    public void whenValueIsAMapThenItIsWrittenAsJsonWithItsSecretsMasked() {
        // given
        final var value = new LinkedHashMap<String, Object>();
        value.put("total", 42);
        value.put("ratio", 0.5);
        value.put("ok", true);
        value.put("missing", null);
        value.put("tags", List.of("a", "b"));
        value.put("db", Map.of("password", "s3cret"));

        // when
        final var response = WorkflowRunPrintResponse.of(new WorkflowRunPrint("summary", value));

        // then
        final var expected = new LinkedHashMap<String, Object>();
        expected.put("total", new BigDecimal("42"));
        expected.put("ratio", new BigDecimal("0.5"));
        expected.put("ok", true);
        expected.put("missing", null);
        expected.put("tags", List.of("a", "b"));
        expected.put("db", Map.of("password", "******"));
        assertEquals("summary", response.name());
        assertEquals("linkedhashmap", response.type());
        assertEquals(expected, response.value());
    }

    @Test
    public void whenValueIsATextThenItStaysATextEvenWhenItHoldsJson() {
        // when and then
        assertEquals("Hello, Ana!", WorkflowRunPrintResponse.of(new WorkflowRunPrint("greeting", "Hello, Ana!")).value());
        assertEquals("[1, 2]", WorkflowRunPrintResponse.of(new WorkflowRunPrint("body", "[1, 2]")).value());
        // the text is written again once a secret in it is masked
        assertEquals("{\"user\":\"ana\",\"password\":\"*\"}",
            WorkflowRunPrintResponse.of(new WorkflowRunPrint("body", "{\"user\": \"ana\", \"password\": \"x\"}")).value());
        // and a name which gives a secret away hides the whole value
        assertEquals("*****", WorkflowRunPrintResponse.of(new WorkflowRunPrint("dbPassword", Map.of("a", 1))).value());
        assertEquals("null", WorkflowRunPrintResponse.of(new WorkflowRunPrint("nothing", null)).value());
    }

    @Test
    public void whenValueIsAResultSetThenItIsAListOfRows() throws SQLException {
        // given
        try (var connection = DriverManager.getConnection("jdbc:sqlite::memory:");
             var statement = connection.createStatement();
             var resultSet = statement.executeQuery("SELECT 1 AS id, 'ana' AS name, 'x' AS password UNION ALL SELECT 2, NULL, 'y'")) {
            final var rowSet = RowSetProvider.newFactory().createCachedRowSet();
            rowSet.populate(resultSet);

            // when the result set comes wrapped, as the output of a query does
            final var response = WorkflowRunPrintResponse.of(new WorkflowRunPrint("users", Optional.of(rowSet)));

            // then
            final var secondRow = new LinkedHashMap<String, Object>();
            secondRow.put("id", new BigDecimal("2"));
            secondRow.put("name", null);
            secondRow.put("password", "*");
            assertEquals(List.of(Map.of("id", new BigDecimal("1"), "name", "ana", "password", "*"), secondRow), response.value());
        }
    }

    @Test
    public void whenNumberDoesNotWriteItselfAsOneThenItIsItsText() {
        // given a number of somebody else
        final var number = new Number() {
            @Override
            public int intValue() {
                return 0;
            }

            @Override
            public long longValue() {
                return 0;
            }

            @Override
            public float floatValue() {
                return 0;
            }

            @Override
            public double doubleValue() {
                return 0;
            }

            @Override
            public String toString() {
                return "n/a";
            }
        };

        // when and then
        assertEquals(List.of("n/a"), WorkflowRunPrintResponse.of(new WorkflowRunPrint("values", List.of(number))).value());
    }

}
