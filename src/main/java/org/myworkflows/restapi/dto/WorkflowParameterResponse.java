package org.myworkflows.restapi.dto;

import lombok.Builder;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.transfer.WorkflowBundle;

/**
 * @param value missing for a secret, see {@code secret}.
 * @param secret whether the parameter holds a secret - its type is {@code PASS}, or its name contains {@code password}
 *     - whose value is left out of every answer, like it is left out of an export.
 * @author Mihai Surdeanu
 * @since 2.2
 */
@Builder
public record WorkflowParameterResponse(String name, WorkflowParameterType type, String value, boolean secret) {

    public static WorkflowParameterResponse of(WorkflowParameter workflowParameter) {
        final var secret = workflowParameter.getType() == WorkflowParameterType.PASS || WorkflowBundle.namesSecret(workflowParameter.getName());
        return WorkflowParameterResponse.builder()
            .name(workflowParameter.getName())
            .type(workflowParameter.getType())
            .value(secret ? null : workflowParameter.getValue())
            .secret(secret)
            .build();
    }

}
