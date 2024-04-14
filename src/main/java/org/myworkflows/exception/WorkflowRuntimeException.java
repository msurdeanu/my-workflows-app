package org.myworkflows.exception;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public class WorkflowRuntimeException extends RuntimeException {

    public WorkflowRuntimeException(final String message) {
        super(message);
    }

    public WorkflowRuntimeException(final Throwable throwable) {
        super(throwable);
    }

}
