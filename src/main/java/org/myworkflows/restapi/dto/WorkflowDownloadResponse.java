package org.myworkflows.restapi.dto;

import lombok.Builder;
import org.myworkflows.domain.WorkflowDownload;

import java.time.Instant;
import java.util.UUID;

/**
 * What is known about a file a run offers to download. The file itself is read with
 * {@code /workflow-downloads/{run id}/{file name}/content}.
 *
 * @param id {@code <run id>/<file name>}, the path of the file under {@code /workflow-downloads}.
 * @param size the size of the file in bytes.
 * @param zipEntries how many files the zip archive holds, or nothing when it is no zip archive.
 * @param zipContentSize how many bytes these files have once they are unpacked, or nothing when it is no zip archive.
 * @author Mihai Surdeanu
 * @since 2.5
 */
@Builder
public record WorkflowDownloadResponse(String id, UUID workflowRunId, String fileName, long size, Instant created, Integer zipEntries,
                                       Long zipContentSize) {

    public static WorkflowDownloadResponse of(WorkflowDownload workflowDownload) {
        return WorkflowDownloadResponse.builder()
            .id(workflowDownload.id())
            .workflowRunId(workflowDownload.workflowRunId())
            .fileName(workflowDownload.fileName())
            .size(workflowDownload.size())
            .created(workflowDownload.created())
            .zipEntries(workflowDownload.zipEntries())
            .zipContentSize(workflowDownload.zipContentSize())
            .build();
    }

}
