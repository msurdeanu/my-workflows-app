package org.myworkflows.domain.event;

import lombok.Builder;
import org.myworkflows.domain.WorkflowRun;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Builder
public record WorkflowDefinitionOnProgressEvent(WorkflowRun workflowRun, boolean persisted) implements Event {

    public static WorkflowDefinitionOnProgressEvent of(WorkflowRun workflowRun, boolean persisted) {
        final var onProgressEventBuilder = WorkflowDefinitionOnProgressEvent.builder();
        onProgressEventBuilder.workflowRun(workflowRun);
        onProgressEventBuilder.persisted(persisted);
        return onProgressEventBuilder.build();
    }

}
