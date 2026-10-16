package org.myworkflows.exception;

import lombok.Getter;

/**
 * Raised when a definition or a template cannot be duplicated. The reason is meant for the user who asked: its
 * {@link Reason#getTranslationKey() translation} says what to do differently.
 *
 * @author Mihai Surdeanu
 * @since 2.2
 */
@Getter
public final class WorkflowDuplicateException extends RuntimeException {

    private final Reason reason;

    // what the translation of the reason names
    private final String name;

    public WorkflowDuplicateException(Reason reason, String name) {
        super(reason.name());
        this.reason = reason;
        this.name = name;
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.2
     */
    public enum Reason {
        NAME_MISSING,
        // another item of the same kind goes by the name: an import finds an item by its name, so it would find two
        NAME_TAKEN,
        // something the original is made of is not in the database anymore
        ORIGINAL_CHANGED;

        public String getTranslationKey() {
            return "duplicate.error." + name().toLowerCase().replace('_', '-');
        }
    }

}
