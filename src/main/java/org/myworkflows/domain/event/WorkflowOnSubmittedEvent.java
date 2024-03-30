package org.myworkflows.domain.event;

import com.networknt.schema.ValidationMessage;
import lombok.Builder;
import lombok.Getter;

import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Getter
public class WorkflowOnSubmittedEvent implements Event {

    private final UUID token;
    private final Set<ValidationMessage> validationMessages;
    private final Future<?> executionContextFuture;

}
