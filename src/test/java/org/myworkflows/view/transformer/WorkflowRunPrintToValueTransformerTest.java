package org.myworkflows.view.transformer;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRunPrint;
import org.myworkflows.view.transformer.WorkflowPrintValue.Node;
import org.myworkflows.view.transformer.WorkflowPrintValue.Source;
import org.myworkflows.view.transformer.WorkflowPrintValue.Type;
import org.myworkflows.view.transformer.WorkflowPrintValue.View;

import java.sql.DriverManager;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.IntStream;

import javax.sql.rowset.CachedRowSet;
import javax.sql.rowset.RowSetProvider;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowRunPrintToValueTransformerTest {

    private final WorkflowRunPrintToValueTransformer transformer = new WorkflowRunPrintToValueTransformer();

    @Test
    public void whenValueIsNeitherACollectionNorJsonThenItStaysAText() {
        // given what is not read as anything else: the last ones only look like JSON
        final var values = new ArrayList<>(List.of("Hello, Ana!", 42, "{total=42, failed=3}", "[INFO] started", "{\"open\": ", "{} {}", new byte[] {1, 2}));
        values.add(null);

        values.forEach(value -> {
            // when
            final var print = new WorkflowRunPrint("greeting", value);
            final var printValue = transformer.transform(print);

            // then
            assertEquals(Source.TEXT, printValue.source());
            assertEquals(print.fullValue(), printValue.text());
            assertNull(printValue.root());
            assertEquals(List.of(View.TEXT), printValue.views());
            assertEquals(View.TEXT, printValue.preferredView());
            assertEquals(List.of(), printValue.jsonLines());
        });
    }

    @Test
    public void whenValueIsAMapThenItIsReadAsATreeWhichOpensAsOne() {
        // given
        final var order = new LinkedHashMap<String, Object>();
        order.put("id", 10482);
        order.put("customer", Map.of("vip", true));
        order.put("items", List.of("KB-204", 29.5));
        order.put("coupon", null);

        // when
        final var printValue = transformer.transform(new WorkflowRunPrint("order", order));

        // then
        assertEquals(Source.STRUCTURE, printValue.source());
        assertEquals(List.of(View.TREE, View.JSON, View.TEXT), printValue.views());
        assertEquals(View.TREE, printValue.preferredView());
        assertEquals(Node.branch(null, Type.MAP, List.of(
            Node.leaf("id", Type.NUMBER, "10482"),
            Node.branch("customer", Type.MAP, List.of(Node.leaf("vip", Type.LITERAL, "true"))),
            Node.branch("items", Type.LIST, List.of(Node.leaf("0", Type.STRING, "KB-204"), Node.leaf("1", Type.NUMBER, "29.5"))),
            Node.leaf("coupon", Type.LITERAL, "null"))), printValue.root());
        // nothing is masked, so the text is the one the map always had
        assertEquals(order.toString(), printValue.text());
        assertEquals("""
            {
              "id": 10482,
              "customer": {
                "vip": true
              },
              "items": [
                "KB-204",
                29.5
              ],
              "coupon": null
            }""", printValue.json());
    }

    @Test
    public void whenValueIsAListOrAnArrayThenItIsReadAsAList() {
        // given
        final var values = List.of(List.of(1, 2), new int[] {1, 2}, new Integer[] {1, 2});

        values.forEach(value -> {
            // when
            final var printValue = transformer.transform(new WorkflowRunPrint("ids", value));

            // then
            assertEquals(Source.STRUCTURE, printValue.source());
            assertEquals(Node.branch(null, Type.LIST, List.of(Node.leaf("0", Type.NUMBER, "1"), Node.leaf("1", Type.NUMBER, "2"))), printValue.root());
        });
    }

    @Test
    public void whenAKeyIsNamedAfterAPasswordThenWhatItHoldsIsMaskedInEveryView() {
        // given a secret two levels down, and another one which is a map itself
        final var login = new LinkedHashMap<String, Object>();
        login.put("user", "ana");
        login.put("apiPassword", "s3cr3t key");
        final var request = new LinkedHashMap<String, Object>();
        request.put("logins", List.of(login));
        request.put("passwords", Map.of("old", "abc"));

        // when
        final var printValue = transformer.transform(new WorkflowRunPrint("request", request));

        // then
        final var maskedLogin = printValue.root().children().getFirst().children().getFirst();
        assertEquals(Node.leaf("apiPassword", Type.MASKED, "****** ***"), maskedLogin.children().getLast());
        assertEquals(Node.leaf("passwords", Type.MASKED, "*********"), printValue.root().children().getLast());
        assertEquals("{logins=[{user=ana, apiPassword=****** ***}], passwords=*********}", printValue.text());
        assertFalse(printValue.json().contains("s3cr3t"));
        assertTrue(printValue.json().contains("\"apiPassword\": \"****** ***\""));
    }

    @Test
    public void whenThePrintItselfIsNamedAfterAPasswordThenItIsNotReadAtAll() {
        // when
        final var printValue = transformer.transform(new WorkflowRunPrint("dbPasswords", Map.of("main", "s3cr3t")));

        // then
        assertEquals(Source.TEXT, printValue.source());
        assertEquals("*************", printValue.text());
    }

    @Test
    public void whenValueIsATextWhichHoldsJsonThenItIsReadAsATreeWhichOpensAsJson() {
        // given
        final var json = " {\"status\":\"ok\",\"took\":42,\"hits\":[{\"score\":0.93}],\"next\":null,\"big\":12345678901234567890} ";

        // when
        final var printValue = transformer.transform(new WorkflowRunPrint("response", json));

        // then
        assertEquals(Source.JSON, printValue.source());
        assertEquals(List.of(View.TREE, View.JSON, View.TEXT), printValue.views());
        assertEquals(View.JSON, printValue.preferredView());
        assertEquals(Node.branch(null, Type.MAP, List.of(
            Node.leaf("status", Type.STRING, "ok"),
            Node.leaf("took", Type.NUMBER, "42"),
            Node.branch("hits", Type.LIST, List.of(Node.branch("0", Type.MAP, List.of(Node.leaf("score", Type.NUMBER, "0.93"))))),
            Node.leaf("next", Type.LITERAL, "null"),
            Node.leaf("big", Type.NUMBER, "12345678901234567890"))), printValue.root());
        // the text is the one which was printed, to the last space
        assertEquals(json, printValue.text());
    }

    @Test
    public void whenATextWhichHoldsJsonHasASecretThenItsTextIsWrittenAgainWithoutIt() {
        // when
        final var printValue = transformer.transform(new WorkflowRunPrint("response", "[{\"user\": \"ana\", \"password\": \"abc\"}, 7]"));

        // then
        assertEquals(Source.JSON, printValue.source());
        assertEquals("[{\"user\":\"ana\",\"password\":\"***\"},7]", printValue.text());
    }

    @Test
    public void whenValueHoldsWhatJsonDoesNotKnowThenItIsWrittenAsAText() {
        // given
        final var values = new LinkedHashMap<String, Object>();
        values.put("say \"hi\"", "a\tb\\c");
        values.put("ratio", Double.NaN);
        values.put("speed", Float.POSITIVE_INFINITY);
        values.put("letter", 'x');
        values.put("builder", new StringBuilder("built"));
        values.put("optional", Optional.of(1));
        values.put("bytes", new byte[] {10, (byte) 0xff});
        values.put("blob", new byte[65]);
        values.put("none", Map.of());
        values.put("nothing", List.of());

        // when
        final var printValue = transformer.transform(new WorkflowRunPrint("values", values));

        // then
        assertEquals(List.of(Type.STRING, Type.OTHER, Type.OTHER, Type.STRING, Type.STRING, Type.OTHER, Type.OTHER, Type.OTHER, Type.MAP, Type.LIST),
            printValue.root().children().stream().map(Node::type).toList());
        assertEquals("""
            {
              "say \\"hi\\"": "a\\tb\\\\c",
              "ratio": "NaN",
              "speed": "Infinity",
              "letter": "x",
              "builder": "built",
              "optional": "Optional[1]",
              "bytes": "0x0aff",
              "blob": "byte[65]",
              "none": {},
              "nothing": []
            }""", printValue.json());
    }

    @Test
    public void whenValueIsTooLargeOrHoldsItselfThenItStaysAText() {
        // given
        final var tooLong = IntStream.range(0, WorkflowRunPrintToValueTransformer.MAX_NODES).boxed().toList();
        final var holdsItself = new HashMap<String, Object>();
        holdsItself.put("self", holdsItself);
        final var tooLongJson = IntStream.range(0, WorkflowRunPrintToValueTransformer.MAX_NODES).boxed().toList().toString();

        // when and then
        assertEquals(Source.TEXT, transformer.transform(new WorkflowRunPrint("numbers", tooLong)).source());
        assertEquals(Source.TEXT, transformer.transform(new WorkflowRunPrint("numbers", tooLongJson)).source());
        final var printValue = transformer.transform(new WorkflowRunPrint("map", holdsItself));
        assertEquals(Source.TEXT, printValue.source());
        assertEquals("{self=(this Map)}", printValue.text());

        // and a value which just fits is still read
        assertEquals(Source.STRUCTURE, transformer.transform(new WorkflowRunPrint("numbers", tooLong.subList(1, tooLong.size()))).source());
    }

    @Test
    public void whenValueChangesWhileItIsReadThenItStaysAText() {
        // given a list which a run adds to while somebody looks at it
        final var orders = new ArrayList<Object>(List.of(1041)) {
            @Override
            public Object[] toArray() {
                throw new java.util.ConcurrentModificationException();
            }
        };

        // when
        final var printValue = transformer.transform(new WorkflowRunPrint("orders", orders));

        // then
        assertEquals(Source.TEXT, printValue.source());
        assertEquals("[1041]", printValue.text());
    }

    @Test
    public void whenValueIsAResultSetThenItIsReadAsATableAndItsCursorIsLeftAlone() throws Exception {
        // given the output of a database command, as it is saved by an output
        final var rowSet = query("""
            SELECT 'Nightly, "full" backup' AS template, 30 AS runs, 0.5 AS ratio, NULL AS last_failure, 'abc' AS vault_password, x'0aff' AS id
            UNION ALL SELECT 'Sync customers', 120, 1.0, 'Connection timed out', 'def', x'0b00'""");
        rowSet.next();

        // when
        final var printValue = transformer.transform(new WorkflowRunPrint("failedRuns", Optional.of(rowSet)));

        // then
        assertEquals(Source.TABLE, printValue.source());
        assertEquals(List.of(View.TABLE, View.JSON), printValue.views());
        assertEquals(View.TABLE, printValue.preferredView());
        assertEquals(List.of("template", "runs", "ratio", "last_failure", "vault_password", "id"), printValue.columns());
        assertEquals(2, printValue.totalRows());
        assertEquals(List.of(
            Node.branch("0", Type.MAP, List.of(
                Node.leaf("template", Type.STRING, "Nightly, \"full\" backup"),
                Node.leaf("runs", Type.NUMBER, "30"),
                Node.leaf("ratio", Type.NUMBER, "0.5"),
                Node.leaf("last_failure", Type.LITERAL, "null"),
                Node.leaf("vault_password", Type.MASKED, "***"),
                Node.leaf("id", Type.OTHER, "0x0aff"))),
            Node.branch("1", Type.MAP, List.of(
                Node.leaf("template", Type.STRING, "Sync customers"),
                Node.leaf("runs", Type.NUMBER, "120"),
                Node.leaf("ratio", Type.NUMBER, "1.0"),
                Node.leaf("last_failure", Type.STRING, "Connection timed out"),
                Node.leaf("vault_password", Type.MASKED, "***"),
                Node.leaf("id", Type.OTHER, "0x0b00")))), printValue.root().children());
        assertEquals("""
            template,runs,ratio,last_failure,vault_password,id
            "Nightly, ""full"" backup",30,0.5,,***,0x0aff
            Sync customers,120,1.0,Connection timed out,***,0x0b00""", printValue.text());
        assertTrue(printValue.json().startsWith("""
            [
              {
                "template": "Nightly, \\"full\\" backup",
                "runs": 30,"""));

        // and the run finds its result set on the row it left it
        assertEquals(1, rowSet.getRow());
        assertEquals("Nightly, \"full\" backup", rowSet.getString("template"));
    }

    @Test
    public void whenAResultSetHasMoreRowsThanAreReadThenItSaysHowManyItHas() throws Exception {
        // given
        final var rows = WorkflowRunPrintToValueTransformer.MAX_ROWS + 5;
        final var rowSet = query("WITH RECURSIVE numbers(n) AS (SELECT 1 UNION ALL SELECT n + 1 FROM numbers WHERE n < " + rows + ") SELECT n FROM numbers");

        // when
        final var printValue = transformer.transform(new WorkflowRunPrint("numbers", rowSet));

        // then
        assertEquals(rows, printValue.totalRows());
        assertEquals(WorkflowRunPrintToValueTransformer.MAX_ROWS, printValue.root().children().size());
        assertEquals("1000", printValue.root().children().getLast().children().getFirst().text());
    }

    @Test
    public void whenAResultSetHasManyColumnsThenFewerOfItsRowsAreRead() throws Exception {
        // given a row of 50 columns, 500 times
        final var columns = IntStream.range(0, 50).mapToObj(index -> "n AS c" + index).reduce((first, second) -> first + ", " + second).orElseThrow();
        final var rowSet = query("WITH RECURSIVE numbers(n) AS (SELECT 1 UNION ALL SELECT n + 1 FROM numbers WHERE n < 500) SELECT " + columns + " FROM numbers");

        // when
        final var printValue = transformer.transform(new WorkflowRunPrint("numbers", rowSet));

        // then
        assertEquals(500, printValue.totalRows());
        assertEquals(WorkflowRunPrintToValueTransformer.MAX_CELLS / 50, printValue.root().children().size());
    }

    private CachedRowSet query(String sql) throws Exception {
        try (final var connection = DriverManager.getConnection("jdbc:sqlite::memory:");
             final var statement = connection.createStatement();
             final var resultSet = statement.executeQuery(sql)) {
            final var rowSet = RowSetProvider.newFactory().createCachedRowSet();
            rowSet.populate(resultSet);
            rowSet.beforeFirst();
            return rowSet;
        }
    }

}
