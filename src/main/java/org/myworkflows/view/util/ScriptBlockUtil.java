package org.myworkflows.view.util;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.SafeConstructor;
import org.yaml.snakeyaml.error.YAMLException;
import org.yaml.snakeyaml.nodes.MappingNode;
import org.yaml.snakeyaml.nodes.Node;
import org.yaml.snakeyaml.nodes.NodeTuple;
import org.yaml.snakeyaml.nodes.ScalarNode;
import org.yaml.snakeyaml.nodes.SequenceNode;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Locates the java.script and groovy.script values of a workflow definition and rewrites them as literal block scalars.
 * Only the text of the value is replaced, so the comments, anchors and formatting of the definition are preserved.
 *
 * @author Mihai Surdeanu
 * @since 1.3
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ScriptBlockUtil {

    private static final Pattern LINE_BREAK = Pattern.compile("\r\n|\r|\n");
    // YAML also breaks lines on these characters, but the editor does not, so the row numbers would no longer match.
    private static final Pattern YAML_ONLY_LINE_BREAK = Pattern.compile("[\\x{85}\\x{2028}\\x{2029}]");
    private static final Pattern BLOCK_HEADER = Pattern.compile("((?:[&!]\\S*\\s+)*)[|>]([1-9]?)([-+]?)([1-9]?)(\\s*(?:#.*)?)");
    private static final Pattern PROPERTIES = Pattern.compile("(?:[&!]\\S*(?:\\s+|$))*");
    private static final Set<String> COMMAND_LIST_KEYS = Set.of("commands", "finallyCommands", "subcommands");
    private static final String NAME_KEY = "name";
    private static final String VALUE_KEY = "value";
    private static final int DEFAULT_INDENTATION = 2;
    private static final int MAX_INDENTATION_INDICATOR = 9;

    public static Optional<ScriptBlock> locate(String definition, int row) {
        return find(definition, row).map(location -> location.toScriptBlock(row));
    }

    public static Optional<Replacement> replace(String definition, int row, String script) {
        return find(definition, row).flatMap(location -> {
            final var replacement = location.replace(script);
            // parse the result once again to make sure that the new YAML holds exactly the new script
            return find(replacement.applyTo(definition), replacement.startRow())
                .filter(newLocation -> newLocation.script().equals(normalize(script)))
                .map(_ -> replacement);
        });
    }

    private static Optional<Location> find(String definition, int row) {
        if (definition == null || YAML_ONLY_LINE_BREAK.matcher(definition).find()) {
            return Optional.empty();
        }

        final Node root;
        try {
            root = new Yaml(new SafeConstructor(new LoaderOptions())).compose(new StringReader(definition));
        } catch (YAMLException exception) {
            log.debug("The workflow definition is not a valid YAML document.", exception);
            return Optional.empty();
        }

        final var scan = new Scan(LINE_BREAK.split(definition, -1), new ArrayList<>(), Collections.newSetFromMap(new IdentityHashMap<>()));
        collect(root, null, scan);
        return scan.locations().stream()
            .filter(location -> location.firstRow() <= row && row <= location.lastRow())
            .max(Comparator.comparingInt(Location::firstRow));
    }

    private static void collect(Node node, String commandName, Scan scan) {
        if (node == null || !scan.visited().add(node)) {
            return;
        }

        if (node instanceof SequenceNode sequenceNode) {
            sequenceNode.getValue().forEach(item -> collect(item, commandName, scan));
        } else if (node instanceof MappingNode mappingNode) {
            scalarValue(mappingNode, NAME_KEY)
                .flatMap(ScriptBlock.Language::of)
                .flatMap(language -> toLocation(mappingNode, language, commandName, scan.lines()))
                .ifPresent(scan.locations()::add);
            mappingNode.getValue().forEach(tuple -> collect(tuple, commandName, scan));
        }
    }

    private static void collect(NodeTuple tuple, String commandName, Scan scan) {
        final var isCommandList = scalarValue(tuple.getKeyNode()).filter(COMMAND_LIST_KEYS::contains).isPresent();
        if (isCommandList && tuple.getValueNode() instanceof SequenceNode commands) {
            commands.getValue().forEach(command -> collect(command, scalarValue(command, NAME_KEY).orElse(null), scan));
        } else {
            collect(tuple.getValueNode(), commandName, scan);
        }
    }

    private static Optional<Location> toLocation(MappingNode mappingNode, ScriptBlock.Language language, String commandName, String[] lines) {
        final var valueTuple = tuple(mappingNode, VALUE_KEY).orElse(null);
        if (valueTuple == null || mappingNode.getFlowStyle() == DumperOptions.FlowStyle.FLOW
            || !(valueTuple.getValueNode() instanceof ScalarNode valueNode) || isAlias(valueTuple)) {
            return Optional.empty();
        }

        final var keyColumn = valueTuple.getKeyNode().getStartMark().getColumn();
        final var startRow = valueNode.getStartMark().getLine();
        final var startColumn = toUtf16Column(lines[startRow], valueNode.getStartMark().getColumn());
        final var keyRows = mappingNode.getValue().stream()
            .mapToInt(tuple -> tuple.getKeyNode().getStartMark().getLine())
            .summaryStatistics();
        final var extent = isBlock(valueNode)
            ? blockExtent(lines, startRow, startColumn, keyColumn)
            : flowExtent(lines, valueNode, startRow, startColumn, keyColumn);
        return extent.map(it -> Location.builder()
            .language(language)
            .commandName(commandName)
            .script(normalize(valueNode.getValue()))
            .literal(valueNode.getScalarStyle() == DumperOptions.ScalarStyle.LITERAL)
            .firstRow(Math.min(keyRows.getMin(), startRow))
            .lastRow(Math.max(keyRows.getMax(), it.endRow()))
            .startRow(startRow)
            .startColumn(startColumn)
            .endRow(it.endRow())
            .endColumn(lines[it.endRow()].length())
            .source(String.join("\n", List.of(lines).subList(startRow, it.endRow() + 1)).substring(startColumn))
            .extent(it)
            .keyColumn(keyColumn)
            .build());
    }

    private static Optional<Extent> blockExtent(String[] lines, int startRow, int startColumn, int keyColumn) {
        final var header = BLOCK_HEADER.matcher(lines[startRow].substring(startColumn));
        if (!header.matches()) {
            return Optional.empty();
        }

        final var indentationIndicator = header.group(2) + header.group(4);
        final var indentation = indentationIndicator.isEmpty()
            ? detectIndentation(lines, startRow, keyColumn)
            : keyColumn + Integer.parseInt(indentationIndicator);
        return Optional.of(new Extent(lastContentRow(lines, startRow, indentation), header.group(1).strip(), header.group(3),
            header.group(5).stripTrailing(), indentation));
    }

    private static Optional<Extent> flowExtent(String[] lines, ScalarNode valueNode, int startRow, int startColumn, int keyColumn) {
        final var endRow = valueNode.getEndMark().getLine();
        final var comment = lines[endRow].substring(toUtf16Column(lines[endRow], valueNode.getEndMark().getColumn())).stripTrailing();
        if (!comment.isEmpty() && !comment.stripLeading().startsWith("#")) {
            return Optional.empty();
        }

        final var properties = PROPERTIES.matcher(lines[startRow].substring(startColumn));
        return Optional.of(new Extent(endRow, properties.lookingAt() ? properties.group().strip() : "", "", comment,
            keyColumn + DEFAULT_INDENTATION));
    }

    private static int detectIndentation(String[] lines, int headerRow, int keyColumn) {
        for (int row = headerRow + 1; row < lines.length; row++) {
            if (!isEmptyLine(lines[row])) {
                final var indentation = indentationOf(lines[row]);
                return indentation > keyColumn ? indentation : keyColumn + DEFAULT_INDENTATION;
            }
        }
        return keyColumn + DEFAULT_INDENTATION;
    }

    private static int lastContentRow(String[] lines, int headerRow, int indentation) {
        var lastRow = headerRow;
        for (int row = headerRow + 1; row < lines.length; row++) {
            if (indentationOf(lines[row]) >= indentation && lines[row].length() > indentation) {
                lastRow = row;
            } else if (!isEmptyLine(lines[row])) {
                break;
            }
        }
        return lastRow;
    }

    private static int indentationOf(String line) {
        var indentation = 0;
        while (indentation < line.length() && line.charAt(indentation) == ' ') {
            indentation++;
        }
        return indentation;
    }

    private static boolean isEmptyLine(String line) {
        return indentationOf(line) == line.length();
    }

    private static boolean isBlock(ScalarNode scalarNode) {
        return scalarNode.getScalarStyle() == DumperOptions.ScalarStyle.LITERAL || scalarNode.getScalarStyle() == DumperOptions.ScalarStyle.FOLDED;
    }

    private static boolean isAlias(NodeTuple tuple) {
        // an alias node keeps the position of its anchor, which is always defined earlier in the document
        return tuple.getValueNode().getStartMark().getIndex() < tuple.getKeyNode().getEndMark().getIndex();
    }

    private static int toUtf16Column(String line, int codePointColumn) {
        return line.offsetByCodePoints(0, Math.min(codePointColumn, line.codePointCount(0, line.length())));
    }

    private static Optional<NodeTuple> tuple(MappingNode mappingNode, String key) {
        return mappingNode.getValue().stream()
            .filter(tuple -> scalarValue(tuple.getKeyNode()).filter(key::equals).isPresent())
            .findFirst();
    }

    private static Optional<String> scalarValue(Node node) {
        return node instanceof ScalarNode scalarNode ? Optional.of(scalarNode.getValue()) : Optional.empty();
    }

    private static Optional<String> scalarValue(Node node, String key) {
        return node instanceof MappingNode mappingNode
            ? tuple(mappingNode, key).flatMap(tuple -> scalarValue(tuple.getValueNode()))
            : Optional.empty();
    }

    private static List<String> toLines(String script) {
        final var lines = new ArrayList<>(List.of(LINE_BREAK.split(script, -1)));
        lines.replaceAll(line -> line.isBlank() ? "" : line);
        while (!lines.isEmpty() && lines.getLast().isEmpty()) {
            lines.removeLast();
        }
        return lines;
    }

    private static String normalize(String script) {
        return String.join("\n", toLines(script));
    }

    /**
     * The text found between (startRow, startColumn) and (endRow, endColumn) is the source, which has to be replaced by
     * the text. Rows and columns are 0-based and the columns are counted in UTF-16 code units, exactly like in the editor.
     *
     * @author Mihai Surdeanu
     * @since 1.3
     */
    public record Replacement(int startRow, int startColumn, int endRow, int endColumn, String source, String text) {

        public String applyTo(String definition) {
            final var lines = List.of(LINE_BREAK.split(definition, -1));
            final var head = lines.subList(0, startRow).stream().map(line -> line + '\n').collect(Collectors.joining());
            final var tail = lines.subList(endRow + 1, lines.size()).stream().map(line -> '\n' + line).collect(Collectors.joining());
            return head + lines.get(startRow).substring(0, startColumn) + text + lines.get(endRow).substring(endColumn) + tail;
        }

    }

    private record Scan(String[] lines, List<Location> locations, Set<Node> visited) {
    }

    private record Extent(int endRow, String properties, String chomping, String comment, int indentation) {
    }

    @Builder
    private record Location(ScriptBlock.Language language, String commandName, String script, boolean literal,
                            int firstRow, int lastRow, int startRow, int startColumn, int endRow, int endColumn,
                            String source, Extent extent, int keyColumn) {

        ScriptBlock toScriptBlock(int row) {
            final var line = literal && row > startRow ? Math.min(row - startRow - 1, (int) script.lines().count() - 1) : 0;
            return new ScriptBlock(language, commandName, script, Math.max(0, line));
        }

        Replacement replace(String newScript) {
            final var lines = toLines(newScript);
            // the indentation has to be explicit when the first line of the script starts with spaces
            final var hasIndentedFirstLine = lines.stream().filter(line -> !line.isEmpty()).findFirst()
                .filter(line -> line.startsWith(" "))
                .isPresent();
            final var indentation = hasIndentedFirstLine && extent.indentation() - keyColumn > MAX_INDENTATION_INDICATOR
                ? keyColumn + DEFAULT_INDENTATION
                : extent.indentation();
            final var text = new StringBuilder(extent.properties().isEmpty() ? "" : extent.properties() + " ")
                .append('|')
                .append(hasIndentedFirstLine ? String.valueOf(indentation - keyColumn) : "")
                .append(extent.chomping())
                .append(extent.comment());
            lines.forEach(line -> text.append('\n').append(line.isEmpty() ? line : " ".repeat(indentation) + line));
            return new Replacement(startRow, startColumn, endRow, endColumn, source, text.toString());
        }

    }

}
