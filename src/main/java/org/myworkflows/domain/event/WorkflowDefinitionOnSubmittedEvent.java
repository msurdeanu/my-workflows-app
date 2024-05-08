package org.myworkflows.domain.event;

import com.networknt.schema.ValidationMessage;
import lombok.Builder;
import lombok.Getter;
import org.myworkflows.domain.ExecutionContext;

import java.util.Set;
import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Getter
public class WorkflowDefinitionOnSubmittedEvent implements Event {

    private final UUID token;
    private final Set<ValidationMessage> validationMessages;
    private final ExecutionContext executionContext;

}
