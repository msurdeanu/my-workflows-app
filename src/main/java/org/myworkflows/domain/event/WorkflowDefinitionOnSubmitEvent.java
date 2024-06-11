package org.myworkflows.domain.event;

import lombok.Builder;
import lombok.Getter;
import org.myworkflows.domain.WorkflowRun;

import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Getter
public class WorkflowDefinitionOnSubmitEvent implements Event {

    private final boolean isManual;
    private final UUID token;
    private final Object workflowDefinitionScript; // allowed object types: WorkflowDefinitionScript or String
    private final WorkflowRun workflowRun;

}
