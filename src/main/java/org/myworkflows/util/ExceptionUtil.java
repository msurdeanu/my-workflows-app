package org.myworkflows.util;

import org.springframework.core.NestedExceptionUtils;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public class ExceptionUtil {

    private static final String DOT = ".";

    public static String getMessageAndCause(final Throwable throwable) {
        if (throwable == null) {
            return null;
        }

        final var builder = new StringBuilder();
        ofNullable(throwable.getMessage()).ifPresent(builder::append);
        ofNullable(NestedExceptionUtils.getRootCause(throwable)).ifPresent(cause -> builder.append(formatCause(cause)));
        return builder.toString();
    }

    private static String formatCause(final Throwable cause) {
        final var builder = new StringBuilder(" Cause type: ");
        builder.append(cause.getClass().getSimpleName());
        builder.append(DOT);

        ofNullable(cause.getMessage()).ifPresent(causeMessage -> {
            builder.append(" Cause message: ");
            builder.append(causeMessage);
            builder.append(DOT);
        });

        final var stackTrace = cause.getStackTrace();
        if (stackTrace.length > 0) {
            builder.append(" Cause stacktrace: ");
            builder.append(stackTrace[0]);
            builder.append(DOT);
        }
        return builder.toString();
    }

}
