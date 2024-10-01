package org.myworkflows.domain.event;

import lombok.Builder;
import lombok.Getter;
import org.myworkflows.domain.WorkflowDefinition;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Getter
public class WorkflowDefinitionOnDeleteEvent implements Event {

    private final WorkflowDefinition workflowDefinition;

}
