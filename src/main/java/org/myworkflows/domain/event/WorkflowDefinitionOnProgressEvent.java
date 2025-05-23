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
public class WorkflowDefinitionOnProgressEvent implements Event {

    private final UUID token;
    private final WorkflowRun workflowRun;
    private final boolean persisted;

}
