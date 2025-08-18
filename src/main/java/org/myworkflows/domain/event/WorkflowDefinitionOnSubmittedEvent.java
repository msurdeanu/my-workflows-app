package org.myworkflows.domain.event;

import com.networknt.schema.ValidationMessage;
import lombok.Builder;
import org.myworkflows.domain.WorkflowRun;

import java.util.Set;
import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Builder
public record WorkflowDefinitionOnSubmittedEvent(UUID token, Set<ValidationMessage> validationMessages, WorkflowRun workflowRun) implements Event {

}
