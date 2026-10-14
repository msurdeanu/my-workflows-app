package org.myworkflows.view.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.SafeConstructor;
import org.yaml.snakeyaml.error.YAMLException;
import org.yaml.snakeyaml.nodes.MappingNode;
import org.yaml.snakeyaml.nodes.Node;
import org.yaml.snakeyaml.nodes.ScalarNode;
import org.yaml.snakeyaml.nodes.SequenceNode;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Tells which lines of a workflow definition, as it is typed in the editor, make which of its commands. Only the commands
 * a run goes through one by one are told apart - the ones listed under {@code commands} and {@code finallyCommands}: a
 * command which is run by another command belongs to the lines of that command.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ScriptCommandUtil {

    // YAML also breaks lines on these characters, but the editor does not, so the row numbers would no longer match.
    private static final Pattern YAML_ONLY_LINE_BREAK = Pattern.compile("[\\x{85}\\x{2028}\\x{2029}]");
    private static final String COMMANDS_KEY = "commands";
    private static final String FINALLY_COMMANDS_KEY = "finallyCommands";
    private static final String NAME_KEY = "name";
    private static final int NO_ROW = -1;

    /**
     * @return the commands of the given definition, the final ones after the others. It is empty for a definition which
     *     is not a YAML document.
     */
    public static List<ScriptCommand> locate(String definition) {
        if (definition == null || YAML_ONLY_LINE_BREAK.matcher(definition).find()) {
            return List.of();
        }

        final Node root;
        try {
            root = new Yaml(new SafeConstructor(new LoaderOptions())).compose(new StringReader(definition));
        } catch (YAMLException exception) {
            log.debug("The workflow definition is not a valid YAML document.", exception);
            return List.of();
        }

        final var commands = new ArrayList<ScriptCommand>();
        collect(root, COMMANDS_KEY, false, commands);
        collect(root, FINALLY_COMMANDS_KEY, true, commands);
        return commands;
    }

    /**
     * @return the command the given row of the editor belongs to, whether it is a final command or not.
     */
    public static Optional<ScriptCommand> findByRow(Collection<ScriptCommand> commands, int row) {
        return commands.stream()
            .filter(command -> command.contains(row))
            .findFirst();
    }

    /**
     * @return the command at the given position among the ones a run can pause before, which leaves the final ones out.
     */
    public static Optional<ScriptCommand> findByIndex(Collection<ScriptCommand> commands, int index) {
        return commands.stream()
            .filter(command -> !command.finalCommand() && command.index() == index)
            .findFirst();
    }

    /**
     * @return the positions of the commands the given rows belong to. A row which belongs to no command, or to a final
     *     one, is left out.
     */
    public static Set<Integer> findIndexesByRows(Collection<ScriptCommand> commands, Collection<Integer> rows) {
        return rows.stream()
            .flatMap(row -> findByRow(commands, row).stream())
            .filter(command -> !command.finalCommand())
            .map(ScriptCommand::index)
            .collect(Collectors.toSet());
    }

    private static void collect(Node root, String key, boolean finalCommand, List<ScriptCommand> commands) {
        if (!(root instanceof MappingNode mappingNode)) {
            return;
        }
        mappingNode.getValue().stream()
            .filter(tuple -> tuple.getKeyNode() instanceof ScalarNode keyNode && key.equals(keyNode.getValue()))
            .map(tuple -> tuple.getValueNode())
            .filter(SequenceNode.class::isInstance)
            .map(SequenceNode.class::cast)
            .findFirst()
            .ifPresent(sequence -> collect(sequence, finalCommand, commands));
    }

    private static void collect(SequenceNode sequence, boolean finalCommand, List<ScriptCommand> commands) {
        final var items = sequence.getValue();
        final var firstRows = findFirstRows(sequence);
        // a block ends where whatever follows it starts, which is the start of a line unless the document ends there
        final var endMark = sequence.getEndMark();
        final var lastRowOfSequence = endMark.getColumn() == 0 ? endMark.getLine() - 1 : endMark.getLine();
        for (int index = 0; index < items.size(); index++) {
            final var firstRow = firstRows.get(index);
            final var lastRow = firstRow == NO_ROW ? NO_ROW : Math.max(firstRow, findRowBeforeNext(firstRows, index, lastRowOfSequence));
            commands.add(new ScriptCommand(finalCommand, index, findName(items.get(index)), firstRow, lastRow));
        }
    }

    /**
     * @return the row each item of the sequence starts on. An item which does not start after the one before it has no
     *     row of its own: it is an alias, written where its anchor is, or the sequence is written on a single line.
     */
    private static List<Integer> findFirstRows(SequenceNode sequence) {
        final var firstRows = new ArrayList<Integer>();
        var previousRow = sequence.getStartMark().getLine() - 1;
        for (Node item : sequence.getValue()) {
            final var row = item.getStartMark().getLine();
            if (row > previousRow && row <= sequence.getEndMark().getLine()) {
                firstRows.add(row);
                previousRow = row;
            } else {
                firstRows.add(NO_ROW);
            }
        }
        return firstRows;
    }

    private static int findRowBeforeNext(List<Integer> firstRows, int index, int lastRowOfSequence) {
        return firstRows.subList(index + 1, firstRows.size()).stream()
            .filter(row -> row != NO_ROW)
            .findFirst()
            .map(row -> row - 1)
            .orElse(lastRowOfSequence);
    }

    private static String findName(Node command) {
        if (!(command instanceof MappingNode mappingNode)) {
            return StringUtils.EMPTY;
        }
        return mappingNode.getValue().stream()
            .filter(tuple -> tuple.getKeyNode() instanceof ScalarNode keyNode && NAME_KEY.equals(keyNode.getValue()))
            .map(tuple -> tuple.getValueNode())
            .filter(ScalarNode.class::isInstance)
            .map(node -> ((ScalarNode) node).getValue())
            .findFirst()
            .orElse(StringUtils.EMPTY);
    }

}
