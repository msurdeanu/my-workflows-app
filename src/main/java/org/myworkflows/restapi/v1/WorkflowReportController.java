package org.myworkflows.restapi.v1;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.restapi.dto.WorkflowReportResponse;
import org.myworkflows.service.WorkflowReportService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.http.ContentDisposition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.UUID;

import static org.myworkflows.restapi.RestApiUtil.notFound;

/**
 * The reports are read and deleted by any user, like the runs which wrote them.
 *
 * @author Mihai Surdeanu
 * @since 2.3
 */
@RestController
@RequestMapping("/api/v1")
@RequiredArgsConstructor
@ConditionalOnProperty(name = "my-workflows.config.feature.restApiEnabled", havingValue = "true")
public class WorkflowReportController {

    private final ApplicationManager applicationManager;

    /**
     * @param run the whole id of a run, to list its reports only.
     * @return the reports, newest first.
     */
    @GetMapping("/workflow-reports")
    public List<WorkflowReportResponse> all(@RequestParam(required = false) UUID run) {
        final var service = applicationManager.getBeanOfType(WorkflowReportService.class);
        return (run == null ? service.getAll() : service.findAllOfRun(run))
            .map(WorkflowReportResponse::of)
            .toList();
    }

    @GetMapping("/workflow-reports/{runId}/{fileName}")
    public WorkflowReportResponse one(@PathVariable UUID runId, @PathVariable String fileName) {
        return WorkflowReportResponse.of(getWorkflowReport(runId, fileName));
    }

    /**
     * @return the text of the report, as a file to save: the answer forbids a browser to run what an HTML report holds,
     *     should somebody open it there.
     */
    @GetMapping("/workflow-reports/{runId}/{fileName}/content")
    public ResponseEntity<String> content(@PathVariable UUID runId, @PathVariable String fileName) {
        final var workflowReport = getWorkflowReport(runId, fileName);
        final var content = applicationManager.getBeanOfType(WorkflowReportService.class).findContent(workflowReport)
            .orElseThrow(() -> notFound("The file of workflow report " + workflowReport.id() + " is missing"));
        return ResponseEntity.ok()
            .contentType(new MediaType(MediaType.parseMediaType(workflowReport.format().getMediaType()), StandardCharsets.UTF_8))
            .header(HttpHeaders.CONTENT_DISPOSITION, ContentDisposition.attachment()
                .filename(workflowReport.fileName(), StandardCharsets.UTF_8)
                .build()
                .toString())
            .header("Content-Security-Policy", "sandbox")
            .header("X-Content-Type-Options", "nosniff")
            .body(content);
    }

    @DeleteMapping("/workflow-reports/{runId}/{fileName}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void delete(@PathVariable UUID runId, @PathVariable String fileName) {
        applicationManager.getBeanOfType(WorkflowReportService.class).delete(getWorkflowReport(runId, fileName));
    }

    /**
     * A report is found among the ones which are listed, never by reading the path it is asked for from the disk.
     */
    private WorkflowReport getWorkflowReport(UUID runId, String fileName) {
        final var id = runId + "/" + fileName;
        return applicationManager.getBeanOfType(WorkflowReportService.class).findById(id)
            .orElseThrow(() -> notFound("Could not find workflow report " + id));
    }

}
