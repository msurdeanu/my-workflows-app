package org.myworkflows.restapi.dto;

import lombok.Builder;
import org.myworkflows.domain.WorkflowReport;

import java.time.Instant;
import java.util.UUID;

/**
 * What is known about a report. Its text is read with {@code /workflow-reports/{run id}/{file name}/content}.
 *
 * @param id {@code <run id>/<file name>}, the path of the report under {@code /workflow-reports}.
 * @param format {@code markdown} or {@code html}, as a workflow names it.
 * @param size the size of the file in bytes.
 * @author Mihai Surdeanu
 * @since 2.3
 */
@Builder
public record WorkflowReportResponse(String id, UUID workflowRunId, Integer workflowTemplateId, Integer userId, String title, String format,
                                     String fileName, long size, Instant created) {

    public static WorkflowReportResponse of(WorkflowReport workflowReport) {
        return WorkflowReportResponse.builder()
            .id(workflowReport.id())
            .workflowRunId(workflowReport.workflowRunId())
            .workflowTemplateId(workflowReport.workflowTemplateId())
            .userId(workflowReport.userId())
            .title(workflowReport.title())
            .format(workflowReport.format().getLabel())
            .fileName(workflowReport.fileName())
            .size(workflowReport.size())
            .created(workflowReport.created())
            .build();
    }

}
