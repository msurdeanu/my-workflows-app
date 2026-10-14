package org.myworkflows.domain.transfer;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.List;

/**
 * Something worth knowing about an item of a file which is about to be imported: how it differs from the one which is
 * already here, what it lacks, or why it cannot be imported as it is.
 *
 * @param arguments what the note is about, in the order its text names them.
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record WorkflowImportNote(Type type, List<String> arguments) {

    public static WorkflowImportNote of(Type type, String... arguments) {
        return new WorkflowImportNote(type, List.of(arguments));
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public enum Severity {
        INFO, WARNING, ERROR
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    @Getter
    @RequiredArgsConstructor
    public enum Type {
        // a new template: how many definitions and how many parameters it comes with
        CONTENT(Severity.INFO),
        // a template: its cron here and in the file
        CRON(Severity.INFO),
        CRON_INVALID(Severity.ERROR),
        // a template: the names of its definitions, then of its parameters, here and in the file
        DEFINITIONS(Severity.INFO),
        PARAMETERS(Severity.INFO),
        SCRIPT(Severity.INFO),
        // a definition: the classes of its commands which no library of this instance brings
        UNKNOWN_COMMANDS(Severity.WARNING),
        // a parameter: the code of its type, or of its type here and in the file
        TYPE(Severity.INFO),
        TYPE_CHANGE(Severity.INFO),
        // a parameter or a placeholder: its value here and in the file
        VALUE(Severity.INFO),
        // the same for a secret, whose values are not shown
        SECRET_VALUE(Severity.INFO),
        // the file left the value out: a new item needs one, an item which is here keeps its own
        VALUE_MISSING(Severity.WARNING),
        VALUE_KEPT(Severity.INFO),
        // a parameter: why its value does not go with its type
        VALUE_INVALID(Severity.ERROR);

        private final Severity severity;
    }

}
