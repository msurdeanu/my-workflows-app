package org.myworkflows.restapi.dto;

import lombok.Builder;
import org.myworkflows.domain.WorkflowRun;

import java.util.Set;
import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
public class WorkflowRunResponse {

    private final UUID id;
    private final Integer workflowTemplateId;
    private final Set<String> printedKeys;
    private final String failureMessage;
    private final long duration;
    private final boolean running;

    public static WorkflowRunResponse of(WorkflowRun workflowRun) {
        return WorkflowRunResponse.builder()
            .id(workflowRun.getId())
            .workflowTemplateId(workflowRun.getWorkflowTemplateId())
            .printedKeys(workflowRun.getPrintedKeys())
            .failureMessage(workflowRun.getFailureMessage())
            .duration(workflowRun.getDuration())
            .running(workflowRun.isRunning())
            .build();
    }

}
