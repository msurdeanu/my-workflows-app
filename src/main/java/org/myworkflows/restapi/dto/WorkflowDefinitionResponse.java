package org.myworkflows.restapi.dto;

import lombok.Builder;
import org.myworkflows.domain.WorkflowDefinition;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
public class WorkflowDefinitionResponse {

    private final int id;
    private final String name;

    public static WorkflowDefinitionResponse of(WorkflowDefinition workflowDefinition) {
        return WorkflowDefinitionResponse.builder()
            .id(workflowDefinition.getId())
            .name(workflowDefinition.getName())
            .build();
    }

}
