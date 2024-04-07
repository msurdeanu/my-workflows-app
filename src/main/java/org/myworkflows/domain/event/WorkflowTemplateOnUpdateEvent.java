package org.myworkflows.domain.event;

import lombok.Builder;
import lombok.Getter;
import org.myworkflows.domain.WorkflowTemplate;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Getter
public class WorkflowTemplateOnUpdateEvent implements Event {

    private final WorkflowTemplate workflowTemplate;

}
