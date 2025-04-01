package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.springframework.core.NestedExceptionUtils;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ExceptionUtil {

    private static final String DOT = ".";

    public static String getMessageAndCause(Throwable throwable) {
        if (throwable == null) {
            return null;
        }

        final var builder = new StringBuilder();
        ofNullable(throwable.getMessage()).ifPresent(builder::append);
        ofNullable(NestedExceptionUtils.getRootCause(throwable)).ifPresent(cause -> builder.append(formatCause(cause)));
        return builder.toString();
    }

    private static String formatCause(Throwable cause) {
        final var builder = new StringBuilder(". Type: ");
        builder.append(cause.getClass().getSimpleName());
        builder.append(DOT);

        ofNullable(cause.getMessage()).ifPresent(causeMessage -> {
            builder.append(" Message: ");
            builder.append(causeMessage);
            builder.append(DOT);
        });

        final var stackTrace = cause.getStackTrace();
        if (stackTrace.length > 0) {
            builder.append(" Stacktrace: ");
            builder.append(stackTrace[0]);
            builder.append(DOT);
        }
        return builder.toString();
    }

}
