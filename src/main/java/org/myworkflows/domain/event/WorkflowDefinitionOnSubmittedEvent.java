package org.myworkflows.domain.event;

import com.networknt.schema.Error;
import lombok.Builder;
import org.myworkflows.domain.WorkflowRun;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Builder
public record WorkflowDefinitionOnSubmittedEvent(List<Error> validationMessages, WorkflowRun workflowRun) implements Event {

}
