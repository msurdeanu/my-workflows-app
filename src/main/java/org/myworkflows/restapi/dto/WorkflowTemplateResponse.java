package org.myworkflows.restapi.dto;

import lombok.Builder;
import org.myworkflows.domain.WorkflowTemplate;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
public class WorkflowTemplateResponse {

    private final int id;
    private final String name;
    private final String cron;

    public static WorkflowTemplateResponse of(WorkflowTemplate workflowTemplate) {
        return WorkflowTemplateResponse.builder()
            .id(workflowTemplate.getId())
            .name(workflowTemplate.getName())
            .cron(workflowTemplate.getCron())
            .build();
    }

}
