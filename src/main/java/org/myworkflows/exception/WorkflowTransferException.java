package org.myworkflows.exception;

import lombok.Getter;

/**
 * Raised when an export cannot be written or an import cannot be read or applied. The reason is meant for the user who
 * asked: its {@link Reason#getTranslationKey() translation} says what is wrong with the file or with the selection.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Getter
public final class WorkflowTransferException extends RuntimeException {

    private final Reason reason;

    // what the translation of the reason names, in the order it names it
    private final transient Object[] arguments;

    public WorkflowTransferException(Reason reason, Object... arguments) {
        super(reason.name());
        this.reason = reason;
        this.arguments = arguments;
    }

    public WorkflowTransferException(Reason reason, Throwable cause, Object... arguments) {
        super(reason.name(), cause);
        this.reason = reason;
        this.arguments = arguments;
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public enum Reason {
        // nothing is picked, so there is nothing to write
        NOTHING_TO_EXPORT,
        // two exported items go by the same name: the argument names them
        DUPLICATE_NAMES,
        EXPORT_FAILED,
        // the file is neither YAML nor an archive which can be read: the argument says why
        FILE_UNREADABLE,
        // the file can be read, but it is no export
        FILE_NOT_AN_EXPORT,
        // the file was written by a version which came later: the arguments are its format and the one known here
        FORMAT_NOT_SUPPORTED,
        FILE_TOO_LARGE,
        // the archive names a file it does not hold: the argument is that file
        FILE_INCOMPLETE,
        // what is here is no longer what the file was compared with
        INSTANCE_CHANGED,
        // something keeps the import from being applied as it was asked for
        NOT_IMPORTABLE;

        public String getTranslationKey() {
            return "transfer.error." + name().toLowerCase().replace('_', '-');
        }
    }

}
