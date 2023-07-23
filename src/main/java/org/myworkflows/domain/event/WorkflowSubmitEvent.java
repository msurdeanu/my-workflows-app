package org.myworkflows.domain.event;

import lombok.Builder;
import lombok.Getter;
import org.myworkflows.domain.ExecutionContext;
import org.myworkflows.domain.Workflow;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Getter
public class WorkflowSubmitEvent implements Event {

    private final Workflow workflow;

    private final ExecutionContext executionContext;

}
