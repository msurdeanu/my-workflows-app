package org.myworkflows.domain.event;

import lombok.Builder;
import lombok.Getter;
import org.myworkflows.domain.ExecutionContext;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Getter
public class WorkflowResultEvent implements Event {

    private final ExecutionContext executionContext;

}
