package org.myworkflows.restapi.dto;

import lombok.Builder;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowTemplate;

import java.util.List;

import static java.util.Optional.ofNullable;

/**
 * @param definitionIds the definitions the template runs, in the order it runs them.
 * @param parameterNames the parameters the template hands over to its run.
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Builder
public record WorkflowTemplateResponse(int id, String name, String cron, boolean enabled, List<Integer> definitionIds,
                                       List<String> parameterNames) {

    public static WorkflowTemplateResponse of(WorkflowTemplate workflowTemplate) {
        return WorkflowTemplateResponse.builder()
            .id(workflowTemplate.getId())
            .name(workflowTemplate.getName())
            .cron(workflowTemplate.getCron())
            .enabled(workflowTemplate.isEnabled())
            .definitionIds(ofNullable(workflowTemplate.getWorkflowDefinitions()).orElse(List.of()).stream()
                .map(WorkflowDefinition::getId)
                .toList())
            .parameterNames(ofNullable(workflowTemplate.getWorkflowParameters()).orElse(List.of()).stream()
                .map(WorkflowParameter::getName)
                .toList())
            .build();
    }

}
