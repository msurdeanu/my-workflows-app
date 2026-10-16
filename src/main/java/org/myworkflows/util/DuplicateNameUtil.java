package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.myworkflows.exception.WorkflowDuplicateException;
import org.myworkflows.exception.WorkflowDuplicateException.Reason;

import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * The name of a copy: an import finds a definition or a template by its name, so a copy never takes one which is
 * taken already.
 *
 * @author Mihai Surdeanu
 * @since 2.2
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class DuplicateNameUtil {

    // what a name suggested here ends with: a copy of a copy is numbered instead of getting a second one
    private static final Pattern COPY_SUFFIX = Pattern.compile(" \\(copy(?: \\d+)?\\)$");

    /**
     * @return the first of {@code <name> (copy)}, {@code <name> (copy 2)}, {@code <name> (copy 3)}... which is not taken.
     */
    public static String suggest(String name, Predicate<String> taken) {
        final var base = COPY_SUFFIX.matcher(name).replaceFirst("");
        var suggestion = base + " (copy)";
        for (var number = 2; taken.test(suggestion); number++) {
            suggestion = base + " (copy " + number + ")";
        }
        return suggestion;
    }

    /**
     * @return the given name without the blanks around it.
     * @throws WorkflowDuplicateException when there is no name, or when it is taken.
     */
    public static String requireFree(String name, Predicate<String> taken) {
        if (name == null || name.isBlank()) {
            throw new WorkflowDuplicateException(Reason.NAME_MISSING, name);
        }
        final var trimmedName = name.trim();
        if (taken.test(trimmedName)) {
            throw new WorkflowDuplicateException(Reason.NAME_TAKEN, trimmedName);
        }
        return trimmedName;
    }

}
