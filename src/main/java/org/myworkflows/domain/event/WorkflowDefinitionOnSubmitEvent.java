package org.myworkflows.domain.event;

import lombok.Builder;
import org.myworkflows.domain.WorkflowRun;

import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Builder
public record WorkflowDefinitionOnSubmitEvent(UUID token, WorkflowRun workflowRun, Object workflowDefinitionScript) implements Event {

}
