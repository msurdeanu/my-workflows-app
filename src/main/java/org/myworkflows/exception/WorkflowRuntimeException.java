package org.myworkflows.exception;

import org.myworkflows.domain.CheckedSupplier;

import java.util.function.Consumer;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowRuntimeException extends RuntimeException {

    public WorkflowRuntimeException(String message) {
        super(message);
    }

    public WorkflowRuntimeException(Throwable throwable) {
        super(throwable);
    }

    public WorkflowRuntimeException(String message, Throwable throwable) {
        super(message, throwable);
    }

    public static <T> T wrap(CheckedSupplier<T> trySupplier) {
        return wrap(trySupplier, exception -> {
        });
    }

    public static <T> T wrap(CheckedSupplier<T> trySupplier, Consumer<Exception> catchExceptionConsumer) {
        try {
            return trySupplier.get();
        } catch (Exception exception) {
            catchExceptionConsumer.accept(exception);
            throw new WorkflowRuntimeException(exception);
        }
    }

}
