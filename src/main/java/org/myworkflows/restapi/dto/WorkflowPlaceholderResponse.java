package org.myworkflows.restapi.dto;

import lombok.Builder;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.transfer.WorkflowBundle;

/**
 * @param value missing for a secret, see {@code secret}.
 * @param secret whether the name of the placeholder contains {@code password}: its value is left out of every answer.
 * @author Mihai Surdeanu
 * @since 2.2
 */
@Builder
public record WorkflowPlaceholderResponse(String name, String value, boolean secret) {

    public static WorkflowPlaceholderResponse of(WorkflowPlaceholder workflowPlaceholder) {
        final var secret = WorkflowBundle.namesSecret(workflowPlaceholder.getName());
        return WorkflowPlaceholderResponse.builder()
            .name(workflowPlaceholder.getName())
            .value(secret ? null : workflowPlaceholder.getValue())
            .secret(secret)
            .build();
    }

}
