package org.myworkflows.domain.event;

import lombok.Builder;
import org.myworkflows.domain.WorkflowRun;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Builder
public record WorkflowDefinitionOnSubmitEvent(WorkflowRun workflowRun, Object workflowDefinitionScript) implements Event {

}
