package org.myworkflows.restapi.dto;

import lombok.Builder;
import org.myworkflows.domain.WorkflowTemplate;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Builder
public record WorkflowTemplateResponse(int id, String name, String cron) {

    public static WorkflowTemplateResponse of(WorkflowTemplate workflowTemplate) {
        return WorkflowTemplateResponse.builder()
            .id(workflowTemplate.getId())
            .name(workflowTemplate.getName())
            .cron(workflowTemplate.getCron())
            .build();
    }

}
