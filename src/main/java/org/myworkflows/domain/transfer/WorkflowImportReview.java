package org.myworkflows.domain.transfer;

import org.myworkflows.domain.transfer.WorkflowImportItem.Action;
import org.myworkflows.domain.transfer.WorkflowImportItem.State;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * A file which is about to be imported, compared with what the instance held when the file was read.
 *
 * @param here what the bundle was compared with.
 * @param items the items of the bundle, each next to what is here under its name. Empty when there are problems.
 * @param problems why nothing of the bundle can be imported, whatever is chosen for its items.
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record WorkflowImportReview(WorkflowBundle bundle, WorkflowTransferContent here, List<WorkflowImportItem> items,
                                   List<WorkflowImportProblem> problems) {

    public Optional<WorkflowImportItem> find(WorkflowTransferKind kind, String name, String template) {
        final var key = WorkflowImportItem.keyOf(kind, name, template);
        return items.stream().filter(item -> item.key().equals(key)).findFirst();
    }

    public long count(State state) {
        return items.stream().filter(item -> item.state() == state).count();
    }

    public long countNeedingLook() {
        return items.stream().filter(WorkflowImportItem::needsLook).count();
    }

    /**
     * @return whether the other review found the same items in the same state, which it does unless the instance
     *     changed between the two.
     */
    public boolean hasStatesOf(WorkflowImportReview other) {
        return statesByKey().equals(other.statesByKey());
    }

    /**
     * @return what is done with the given item. An item which is the same here is left alone whatever was asked for,
     *     and a new parameter of a single template goes where its template goes: it has nowhere to be without it.
     */
    public Action actionOf(WorkflowImportItem item, WorkflowImportRequest request) {
        if (item.state() == State.SAME) {
            return Action.SKIP;
        }
        if (item.template() != null && item.state() == State.NEW && !isImported(WorkflowTransferKind.TEMPLATE, item.template(), request)) {
            return Action.SKIP;
        }
        return request.actions().getOrDefault(item.key(), Action.IMPORT);
    }

    public boolean isImported(WorkflowTransferKind kind, String name, WorkflowImportRequest request) {
        return find(kind, name, null).filter(item -> actionOf(item, request) == Action.IMPORT).isPresent();
    }

    /**
     * @return the parameter of the bundle the given item stands for, when it stands for a parameter.
     */
    public Optional<WorkflowBundle.Parameter> parameterOf(WorkflowImportItem item) {
        if (item.kind() != WorkflowTransferKind.PARAMETER) {
            return Optional.empty();
        }
        final var parameters = item.template() == null
            ? bundle.parameters().stream()
            : bundle.templates().stream().filter(template -> item.template().equals(template.name())).flatMap(template -> template.ownParameters().stream());
        return parameters.filter(parameter -> item.name().equals(parameter.name())).findFirst();
    }

    private Map<String, State> statesByKey() {
        return items.stream().collect(Collectors.toMap(WorkflowImportItem::key, WorkflowImportItem::state, (first, _) -> first));
    }

}
