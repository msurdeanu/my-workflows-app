package org.myworkflows.domain.transfer;

import java.util.List;

/**
 * What an import did, item by item, and what it leaves for somebody to do.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record WorkflowImportResult(List<Outcome> outcomes, List<Todo> todos) {

    public List<Outcome> findOutcomes(Change change) {
        return outcomes.stream().filter(outcome -> outcome.change() == change).toList();
    }

    /**
     * @return how many items were created or overwritten.
     */
    public int countImported() {
        return findOutcomes(Change.CREATED).size() + findOutcomes(Change.OVERWRITTEN).size();
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public enum Change {
        CREATED,
        OVERWRITTEN,
        // it was here and stays as it was: it is the same in the file, or it was chosen to keep it
        KEPT,
        // it is not here and was not imported
        SKIPPED
    }

    /**
     * @param template the name of the template a parameter is meant for only, missing for anything else.
     * @param target what the item is here by now: the id of a definition or of a template, the name a parameter or a
     *     placeholder is saved under. Missing for an item which was not imported and is not here.
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public record Outcome(WorkflowTransferKind kind, String name, String template, String target, Change change) {

    }

    /**
     * @param outcome the imported item something is left to do about.
     * @param detail what the task is about, where its type has something to add.
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public record Todo(Type type, Outcome outcome, String detail) {

        /**
         * @author Mihai Surdeanu
         * @since 2.1
         */
        public enum Type {
            // a template was created unticked although the file has it ticked
            TEMPLATE_PAUSED,
            // a parameter or a placeholder was created without a value
            VALUE_MISSING,
            // a definition uses commands no library of this instance brings: the detail names them
            UNKNOWN_COMMANDS
        }

    }

}
