package org.myworkflows.domain.event;

import lombok.Builder;
import org.myworkflows.domain.WorkflowRun;

import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Builder
public record WorkflowDefinitionOnProgressEvent(UUID token, WorkflowRun workflowRun, boolean persisted) implements Event {

    public static WorkflowDefinitionOnProgressEvent of(UUID token, WorkflowRun workflowRun, boolean persisted) {
        final var onProgressEventBuilder = WorkflowDefinitionOnProgressEvent.builder();
        onProgressEventBuilder.token(token);
        onProgressEventBuilder.workflowRun(workflowRun);
        onProgressEventBuilder.persisted(persisted);
        return onProgressEventBuilder.build();
    }

}
