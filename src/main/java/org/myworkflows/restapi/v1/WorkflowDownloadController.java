package org.myworkflows.restapi.v1;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowDownload;
import org.myworkflows.restapi.dto.WorkflowDownloadResponse;
import org.myworkflows.service.WorkflowDownloadService;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.ContentDisposition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.UUID;

import static org.myworkflows.restapi.RestApiUtil.notFound;

/**
 * The files the runs offer to download, which any user downloads, like the runs page shows them to any user.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@RestController
@RequestMapping("/api/v1")
@RequiredArgsConstructor
public class WorkflowDownloadController {

    private final ApplicationManager applicationManager;

    /**
     * @param run the whole id of a run.
     * @return the files the run offers, the oldest first.
     */
    @GetMapping("/workflow-downloads")
    public List<WorkflowDownloadResponse> all(@RequestParam UUID run) {
        return applicationManager.getBeanOfType(WorkflowDownloadService.class).findAllOfRun(run).stream()
            .map(WorkflowDownloadResponse::of)
            .toList();
    }

    @GetMapping("/workflow-downloads/{runId}/{fileName}")
    public WorkflowDownloadResponse one(@PathVariable UUID runId, @PathVariable String fileName) {
        return WorkflowDownloadResponse.of(getWorkflowDownload(runId, fileName));
    }

    /**
     * @return the file, as one to save: whatever it holds, a browser neither shows it nor runs it.
     */
    @GetMapping("/workflow-downloads/{runId}/{fileName}/content")
    public ResponseEntity<Resource> content(@PathVariable UUID runId, @PathVariable String fileName) {
        final var workflowDownload = getWorkflowDownload(runId, fileName);
        final InputStreamResource resource;
        try {
            resource = new InputStreamResource(applicationManager.getBeanOfType(WorkflowDownloadService.class).openStream(workflowDownload));
        } catch (IOException gone) {
            throw notFound("The file of workflow download " + workflowDownload.id() + " is missing");
        }
        return ResponseEntity.ok()
            .contentType(MediaType.APPLICATION_OCTET_STREAM)
            .contentLength(workflowDownload.size())
            .header(HttpHeaders.CONTENT_DISPOSITION, ContentDisposition.attachment()
                .filename(workflowDownload.fileName(), StandardCharsets.UTF_8)
                .build()
                .toString())
            .header("Content-Security-Policy", "sandbox")
            .header("X-Content-Type-Options", "nosniff")
            .body(resource);
    }

    /**
     * A file is found among the ones its run offers, never by reading the path it is asked for from the disk.
     */
    private WorkflowDownload getWorkflowDownload(UUID runId, String fileName) {
        return applicationManager.getBeanOfType(WorkflowDownloadService.class).find(runId, fileName)
            .orElseThrow(() -> notFound("Could not find workflow download " + runId + "/" + fileName));
    }

}
