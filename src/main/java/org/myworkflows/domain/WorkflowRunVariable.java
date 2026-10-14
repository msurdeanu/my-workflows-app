package org.myworkflows.domain;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * A value a workflow run holds, as it was when somebody looked: its type and its value are written down, so that the
 * run can go on changing what it holds. The value of a variable whose name contains {@code password} is masked, as it
 * is when it is printed.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record WorkflowRunVariable(String name, String type, String value, Change change) {

    /**
     * @param previous what the run held the last time somebody looked, or nothing when nobody did: no variable is then
     *     marked as added or changed.
     * @return everything the given cache holds, sorted by name.
     */
    public static List<WorkflowRunVariable> allOf(WorkflowRunCache cache, List<WorkflowRunVariable> previous) {
        final var previousByName = previous == null ? null : previous.stream()
            .collect(Collectors.toMap(WorkflowRunVariable::name, Function.identity(), (first, second) -> second));
        return cache.getAllKeys().stream()
            .map(name -> of(new WorkflowRunPrint(name, cache.get(name)), previousByName))
            .sorted(Comparator.comparing(WorkflowRunVariable::name))
            .toList();
    }

    private static WorkflowRunVariable of(WorkflowRunPrint print, Map<String, WorkflowRunVariable> previousByName) {
        final var type = print.type();
        final var value = print.fullValue();
        if (previousByName == null) {
            return new WorkflowRunVariable(print.name(), type, value, Change.NONE);
        }
        final var before = previousByName.get(print.name());
        if (before == null) {
            return new WorkflowRunVariable(print.name(), type, value, Change.ADDED);
        }
        return new WorkflowRunVariable(print.name(), type, value,
            before.type().equals(type) && before.value().equals(value) ? Change.NONE : Change.CHANGED);
    }

    public boolean isMarked() {
        return change != Change.NONE;
    }

    /**
     * What happened to a variable since somebody looked the last time.
     *
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public enum Change {
        NONE, ADDED, CHANGED
    }

}
