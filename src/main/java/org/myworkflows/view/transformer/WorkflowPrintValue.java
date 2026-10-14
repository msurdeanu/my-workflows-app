package org.myworkflows.view.transformer;

import com.fasterxml.jackson.core.io.JsonStringEncoder;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

import static java.util.stream.Collectors.joining;

/**
 * A printed value, read for somebody who looks at it: a map, a list and a text which holds JSON are a tree, a result
 * set is a table, and everything else is the text it always was. The text is kept in every case, as it is what is
 * copied and what is shown when nothing else is asked for.
 *
 * @param text what the value reads like as a text. For a table, its rows as CSV.
 * @param root the tree of the value, missing for a plain text. The rows of a table are the children of its root, each
 *     with one child per column.
 * @param columns the columns of a table, empty for anything else.
 * @param totalRows how many rows the result set has, which can be more than the ones which were read.
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record WorkflowPrintValue(Source source, String text, Node root, List<String> columns, int totalRows) {

    private static final String INDENT = "  ";

    public static WorkflowPrintValue ofText(String text) {
        return new WorkflowPrintValue(Source.TEXT, text, null, List.of(), 0);
    }

    /**
     * @return the ways this value can be shown, in the order they are offered.
     */
    public List<View> views() {
        return switch (source) {
            case STRUCTURE, JSON -> List.of(View.TREE, View.JSON, View.TEXT);
            case TABLE -> List.of(View.TABLE, View.JSON);
            case TEXT -> List.of(View.TEXT);
        };
    }

    /**
     * @return the view this value opens with: a text which was written as JSON is read as JSON first.
     */
    public View preferredView() {
        return source == Source.JSON ? View.JSON : views().getFirst();
    }

    /**
     * @return the value as JSON, one line per entry. A plain text has none.
     */
    public List<JsonLine> jsonLines() {
        return root == null ? List.of() : root.jsonLines();
    }

    public String json() {
        return jsonLines().stream().map(JsonLine::text).collect(joining("\n"));
    }

    private static String quote(String text) {
        return '"' + new String(JsonStringEncoder.getInstance().quoteAsString(text)) + '"';
    }

    /**
     * Where a printed value comes from, which decides how it can be shown.
     *
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public enum Source {
        TEXT, STRUCTURE, JSON, TABLE
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public enum View {
        TREE, JSON, TABLE, TEXT
    }

    /**
     * What a node of the tree holds. A literal is {@code true}, {@code false} or {@code null}, a masked node holds a
     * secret and anything which is not known to JSON is {@code OTHER}: it is shown as the text it reads like.
     *
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public enum Type {
        MAP, LIST, STRING, NUMBER, LITERAL, MASKED, OTHER
    }

    /**
     * A value inside a printed value, under the key it is found by: the key of a map, the position in a list or the
     * column of a row. The root has no key.
     *
     * @param text what the node reads like, without quotes. Empty for a map and for a list.
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public record Node(String key, Type type, String text, List<Node> children) {

        public static Node leaf(String key, Type type, String text) {
            return new Node(key, type, text, List.of());
        }

        public static Node branch(String key, Type type, List<Node> children) {
            return new Node(key, type, StringUtils.EMPTY, children);
        }

        public boolean isBranch() {
            return type == Type.MAP || type == Type.LIST;
        }

        /**
         * @return this node as it is written in JSON, which knows numbers, literals and texts only.
         */
        public String jsonText() {
            return type == Type.NUMBER || type == Type.LITERAL ? text : quote(text);
        }

        /**
         * @return this node and everything under it as JSON, one line per entry.
         */
        public List<JsonLine> jsonLines() {
            final var lines = new ArrayList<JsonLine>();
            addJsonLines(lines, 0, false, false);
            return lines;
        }

        private void addJsonLines(List<JsonLine> lines, int depth, boolean keyed, boolean comma) {
            final var quotedKey = keyed ? quote(key) : null;
            if (!isBranch()) {
                lines.add(new JsonLine(depth, quotedKey, jsonText(), type, comma));
                return;
            }
            final var list = type == Type.LIST;
            if (children.isEmpty()) {
                lines.add(new JsonLine(depth, quotedKey, list ? "[]" : "{}", type, comma));
                return;
            }
            lines.add(new JsonLine(depth, quotedKey, list ? "[" : "{", type, false));
            for (int index = 0; index < children.size(); index++) {
                children.get(index).addJsonLines(lines, depth + 1, !list, index < children.size() - 1);
            }
            lines.add(new JsonLine(depth, null, list ? "]" : "}", type, comma));
        }

    }

    /**
     * A line of a value written as JSON, in the parts which are told apart by their colour.
     *
     * @param key the key of the entry, in quotes, or nothing for an item of a list and for a closing bracket.
     * @param type what the value is: a bracket has the type of what it opens or closes.
     * @param comma whether another entry follows.
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public record JsonLine(int depth, String key, String value, Type type, boolean comma) {

        public String indent() {
            return INDENT.repeat(depth);
        }

        public String text() {
            return indent() + (key == null ? StringUtils.EMPTY : key + ": ") + value + (comma ? "," : StringUtils.EMPTY);
        }

    }

}
