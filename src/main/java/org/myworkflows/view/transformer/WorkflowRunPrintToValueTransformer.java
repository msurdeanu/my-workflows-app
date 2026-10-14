package org.myworkflows.view.transformer;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowRunPrint;
import org.myworkflows.view.transformer.WorkflowPrintValue.JsonLine;
import org.myworkflows.view.transformer.WorkflowPrintValue.Node;
import org.myworkflows.view.transformer.WorkflowPrintValue.Source;
import org.myworkflows.view.transformer.WorkflowPrintValue.Type;

import java.lang.reflect.Array;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.IntStream;

import javax.sql.rowset.CachedRowSet;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static java.util.stream.Collectors.joining;
import static org.myworkflows.domain.WorkflowRunPrint.NULL_AS_STR;
import static org.myworkflows.domain.WorkflowRunPrint.PASSWORD;

/**
 * Reads a printed value for whoever looks at it. Whatever is found under a key whose name contains {@code password}
 * is masked, however deep it sits, like a variable of that name is.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowRunPrintToValueTransformer implements Transformer<WorkflowRunPrint, WorkflowPrintValue> {

    /**
     * A value with more entries than this, all its levels counted, is no longer read as a tree.
     */
    static final int MAX_NODES = 10_000;

    static final int MAX_DEPTH = 32;

    /**
     * The rows of a result set which are read: the first ones, and fewer of them when the columns are many.
     */
    static final int MAX_ROWS = 1_000;

    static final int MAX_CELLS = 20_000;

    private static final int MAX_JSON_LENGTH = 1_048_576;

    private static final int MAX_BYTES_AS_HEX = 64;

    private static final ObjectMapper JSON_MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    @Override
    public WorkflowPrintValue transform(WorkflowRunPrint workflowRunPrint) {
        final var text = workflowRunPrint.fullValue();
        // a name which gives a secret away hides the whole value, as it always did
        if (isSecret(workflowRunPrint.name())) {
            return WorkflowPrintValue.ofText(text);
        }
        try {
            return read(unwrap(workflowRunPrint.value()), text);
        } catch (RuntimeException | SQLException notReadable) {
            // too large, changed by its run in the meantime or a collection of somebody else: it stays the text it was
            return WorkflowPrintValue.ofText(text);
        }
    }

    private WorkflowPrintValue read(Object value, String text) throws SQLException {
        if (value instanceof CachedRowSet rowSet) {
            return readTable(rowSet);
        }
        final var reader = new NodeReader();
        if (isStructure(value)) {
            final var root = reader.read(null, value, 0);
            return new WorkflowPrintValue(Source.STRUCTURE, reader.masked ? describe(root) : text, root, List.of(), 0);
        }
        if (value instanceof CharSequence) {
            return parseJson(text)
                .map(json -> reader.read(null, json, 0))
                .map(root -> new WorkflowPrintValue(Source.JSON, reader.masked ? compact(root) : text, root, List.of(), 0))
                .orElseGet(() -> WorkflowPrintValue.ofText(text));
        }
        return WorkflowPrintValue.ofText(text);
    }

    /**
     * The output of a command comes wrapped when the command may return nothing, like a query without rows.
     */
    private Object unwrap(Object value) {
        return value instanceof Optional<?> optional && optional.isPresent() ? optional.get() : value;
    }

    private boolean isStructure(Object value) {
        return value instanceof Map<?, ?> || value instanceof Collection<?> || isArray(value) && !(value instanceof byte[]);
    }

    private Optional<Object> parseJson(String text) {
        final var json = text.strip();
        if (json.length() > MAX_JSON_LENGTH || !isWrapped(json, '{', '}') && !isWrapped(json, '[', ']')) {
            return empty();
        }
        try {
            return of(JSON_MAPPER.readValue(json, Object.class));
        } catch (JsonProcessingException notJson) {
            return empty();
        }
    }

    private boolean isWrapped(String text, char first, char last) {
        return text.length() > 1 && text.charAt(0) == first && text.charAt(text.length() - 1) == last;
    }

    private WorkflowPrintValue readTable(CachedRowSet rowSet) throws SQLException {
        // a cursor of its own over the same rows: the one of the run stays where its commands left it
        final var rows = rowSet.createShared();
        final var metaData = rows.getMetaData();
        final var columns = new ArrayList<String>();
        for (int column = 1; column <= metaData.getColumnCount(); column++) {
            columns.add(metaData.getColumnLabel(column));
        }
        final var maxRows = Math.max(1, Math.min(MAX_ROWS, MAX_CELLS / Math.max(1, columns.size())));
        final var readRows = new ArrayList<Node>();
        rows.beforeFirst();
        while (readRows.size() < maxRows && rows.next()) {
            readRows.add(readRow(rows, columns, readRows.size()));
        }
        final var root = Node.branch(null, Type.LIST, List.copyOf(readRows));
        return new WorkflowPrintValue(Source.TABLE, toCsv(columns, root), root, List.copyOf(columns), Math.max(rowSet.size(), readRows.size()));
    }

    private Node readRow(ResultSet rows, List<String> columns, int index) throws SQLException {
        final var cells = new ArrayList<Node>(columns.size());
        for (int column = 0; column < columns.size(); column++) {
            cells.add(readLeaf(columns.get(column), rows.getObject(column + 1)));
        }
        return Node.branch(String.valueOf(index), Type.MAP, List.copyOf(cells));
    }

    private String toCsv(List<String> columns, Node root) {
        final var lines = new ArrayList<String>();
        lines.add(columns.stream().map(this::toCsvCell).collect(joining(",")));
        root.children().forEach(row -> lines.add(row.children().stream()
            .map(cell -> isNull(cell) ? StringUtils.EMPTY : toCsvCell(cell.text()))
            .collect(joining(","))));
        return String.join("\n", lines);
    }

    private String toCsvCell(String text) {
        return StringUtils.containsAny(text, '"', ',', '\n', '\r') ? '"' + text.replace("\"", "\"\"") + '"' : text;
    }

    private static boolean isNull(Node node) {
        return node.type() == Type.LITERAL && NULL_AS_STR.equals(node.text());
    }

    /**
     * @return the given tree as Java prints a map and a list, which is what its text was before a secret was masked.
     */
    private static String describe(Node node) {
        return switch (node.type()) {
            case MAP -> node.children().stream().map(child -> child.key() + "=" + describe(child)).collect(joining(", ", "{", "}"));
            case LIST -> node.children().stream().map(WorkflowRunPrintToValueTransformer::describe).collect(joining(", ", "[", "]"));
            default -> node.text();
        };
    }

    private static String compact(Node root) {
        return root.jsonLines().stream().map(WorkflowRunPrintToValueTransformer::compact).collect(joining());
    }

    private static String compact(JsonLine line) {
        return (line.key() == null ? StringUtils.EMPTY : line.key() + ":") + line.value() + (line.comma() ? "," : StringUtils.EMPTY);
    }

    private static boolean isSecret(String key) {
        return StringUtils.containsIgnoreCase(key, PASSWORD);
    }

    private static boolean isArray(Object value) {
        return value != null && value.getClass().isArray();
    }

    private static Node readLeaf(String key, Object value) {
        if (isSecret(key)) {
            return Node.leaf(key, Type.MASKED, String.valueOf(value).replaceAll("\\S", "*"));
        }
        return switch (value) {
            case null -> Node.leaf(key, Type.LITERAL, NULL_AS_STR);
            case Boolean flag -> Node.leaf(key, Type.LITERAL, flag.toString());
            // JSON has no way to write a number which is not finite
            case Number number -> Node.leaf(key, isFinite(number) ? Type.NUMBER : Type.OTHER, number.toString());
            case CharSequence chars -> Node.leaf(key, Type.STRING, chars.toString());
            case Character character -> Node.leaf(key, Type.STRING, character.toString());
            case byte[] bytes -> Node.leaf(key, Type.OTHER, toHex(bytes));
            default -> Node.leaf(key, Type.OTHER, String.valueOf(value));
        };
    }

    private static boolean isFinite(Number number) {
        return !(number instanceof Double || number instanceof Float) || Double.isFinite(number.doubleValue());
    }

    private static String toHex(byte[] bytes) {
        return bytes.length > MAX_BYTES_AS_HEX ? "byte[" + bytes.length + "]" : "0x" + HexFormat.of().formatHex(bytes);
    }

    /**
     * Reads one value into a tree and remembers what it came across on its way: how much it read, as a value can be
     * too large or hold itself, and whether it masked anything.
     *
     * @author Mihai Surdeanu
     * @since 2.1
     */
    private static final class NodeReader {

        private int nodes;
        private boolean masked;

        private Node read(String key, Object value, int depth) {
            nodes++;
            if (nodes > MAX_NODES || depth > MAX_DEPTH) {
                throw new IllegalStateException("The value is too large to be read as a tree");
            }
            if (isSecret(key)) {
                masked = true;
                return readLeaf(key, value);
            }
            return switch (value) {
                case Map<?, ?> map -> Node.branch(key, Type.MAP, map.entrySet().stream()
                    .map(entry -> read(String.valueOf(entry.getKey()), entry.getValue(), depth + 1))
                    .toList());
                case Collection<?> items -> readItems(key, new ArrayList<>(items), depth);
                case byte[] bytes -> readLeaf(key, bytes);
                case null, default -> isArray(value)
                    ? readItems(key, IntStream.range(0, Array.getLength(value)).mapToObj(index -> Array.get(value, index)).toList(), depth)
                    : readLeaf(key, value);
            };
        }

        private Node readItems(String key, List<?> items, int depth) {
            return Node.branch(key, Type.LIST, IntStream.range(0, items.size())
                .mapToObj(index -> read(String.valueOf(index), items.get(index), depth + 1))
                .toList());
        }

    }

}
