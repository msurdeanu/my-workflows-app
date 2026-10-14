package org.myworkflows.domain.transfer;

import org.myworkflows.domain.transfer.WorkflowImportNote.Severity;

import java.util.List;

/**
 * An item of a file which is about to be imported, next to what is already here under its name.
 *
 * @param name the name the item goes by in the file.
 * @param template the name of the template a parameter is meant for only, missing for anything else.
 * @param target what the item is here: the id of a definition or of a template, the name a parameter or a placeholder
 *     is saved under. Missing for an item which is not here yet.
 * @param secret whether the item holds a secret, whose value is shown nowhere.
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record WorkflowImportItem(WorkflowTransferKind kind, String name, String template, State state, String target, boolean secret,
                                 List<WorkflowImportNote> notes) {

    /**
     * @return what tells this item from any other of its file.
     */
    public String key() {
        return keyOf(kind, name, template);
    }

    public static String keyOf(WorkflowTransferKind kind, String name, String template) {
        return kind.name() + ':' + (template == null ? "" : template + '/') + name;
    }

    public boolean has(WorkflowImportNote.Type type) {
        return notes.stream().anyMatch(note -> note.type() == type);
    }

    /**
     * @return whether something about this item has to be read before it is imported.
     */
    public boolean needsLook() {
        return notes.stream().anyMatch(note -> note.type().getSeverity() != Severity.INFO);
    }

    /**
     * @return whether this item cannot be imported as the file has it.
     */
    public boolean isInvalid() {
        return notes.stream().anyMatch(note -> note.type().getSeverity() == Severity.ERROR);
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public enum State {
        NEW, DIFFERENT, SAME
    }

    /**
     * What to do with an item: either what is here becomes what the file has - it is created or overwritten - or
     * nothing changes.
     *
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public enum Action {
        IMPORT, SKIP
    }

}
