package org.myworkflows.domain.transfer;

/**
 * Why a file cannot be imported, or cannot be imported with what was chosen for its items.
 *
 * @param name the name of the item the problem is about.
 * @param detail what the item lacks or why it is refused, where the type of the problem has something to add.
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record WorkflowImportProblem(Type type, WorkflowTransferKind kind, String name, String detail) {

    public static WorkflowImportProblem of(Type type, WorkflowTransferKind kind, String name) {
        return new WorkflowImportProblem(type, kind, name, null);
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public enum Type {
        // the item has no name, a name which cannot be used or - a parameter - no type, or it holds nothing
        ITEM_UNUSABLE,
        // two items of the file go by this name
        DUPLICATE_IN_FILE,
        // two items which are here go by this name, so the file cannot say which one it means
        AMBIGUOUS_HERE,
        // a note of the item says why it cannot be imported as it is
        ITEM_INVALID,
        // the value typed for an item does not go with its type: the detail says why
        VALUE_INVALID,
        // a template runs a definition, or hands over a parameter, which is neither imported nor here: the detail names it
        DEFINITION_MISSING,
        PARAMETER_MISSING
    }

}
