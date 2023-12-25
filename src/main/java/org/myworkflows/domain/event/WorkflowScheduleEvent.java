package org.myworkflows.domain.event;

import com.networknt.schema.ValidationMessage;
import lombok.Builder;
import lombok.Getter;
import org.myworkflows.domain.ExecutionContext;

import java.util.Set;
import java.util.concurrent.Future;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Getter
public class WorkflowScheduleEvent implements Event {

    private final Set<ValidationMessage> validationMessages;
    private final Future<ExecutionContext> executionContextFuture;

}
