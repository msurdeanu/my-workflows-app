package org.myworkflows.exception;

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

}
