package org.myworkflows.domain.event;

import lombok.Builder;
import lombok.Getter;
import org.myworkflows.domain.ExecutionContext;

import java.util.concurrent.Future;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Getter
public class WorkflowScheduleEvent implements Event {

    private final Future<ExecutionContext> executionContextFuture;

}
