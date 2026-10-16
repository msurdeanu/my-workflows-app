package org.myworkflows.restapi.v1;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.restapi.dto.WorkflowRunPrintResponse;
import org.myworkflows.restapi.dto.WorkflowRunResponse;
import org.myworkflows.service.WorkflowRunService;
import org.myworkflows.util.SecurityUtil;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.UUID;

import static org.myworkflows.restapi.RestApiUtil.conflict;
import static org.myworkflows.restapi.RestApiUtil.forbidden;
import static org.myworkflows.restapi.RestApiUtil.notFound;

/**
 * Cancelling, deleting and replaying a run are open to the users the runs page leaves them to: any user cancels and
 * deletes any run, and only the user who started a run replays it.
 *
 * @author Mihai Surdeanu
 * @since 1.0
 */
@RestController
@RequestMapping("/api/v1")
@RequiredArgsConstructor
@ConditionalOnProperty(name = "my-workflows.config.feature.restApiEnabled", havingValue = "true")
public class WorkflowRunController {

    private final ApplicationManager applicationManager;

    @GetMapping("/workflow-runs")
    public List<WorkflowRunResponse> all() {
        return applicationManager.getBeanOfType(WorkflowRunService.class).getAll()
            .map(WorkflowRunResponse::of)
            .toList();
    }

    @GetMapping("/workflow-runs/{id}")
    public WorkflowRunResponse one(@PathVariable UUID id) {
        return WorkflowRunResponse.of(getWorkflowRunById(id));
    }

    /**
     * @return what the run printed so far, in the order it printed it. A run in debug mode prints everything it holds.
     */
    @GetMapping("/workflow-runs/{id}/prints")
    public List<WorkflowRunPrintResponse> prints(@PathVariable UUID id) {
        return getWorkflowRunById(id).getAllPrints().stream()
            .map(WorkflowRunPrintResponse::of)
            .toList();
    }

    @PostMapping("/workflow-runs/{id}/cancel")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void cancel(@PathVariable UUID id) {
        if (!getWorkflowRunById(id).cancelAndInterruptIfRunning()) {
            throw conflict("Workflow run with id = " + id + " is not in progress");
        }
    }

    /**
     * Starts a failed run again from the command it did not get past, with everything it held then. The run keeps its
     * id, so it is followed as before.
     */
    @PostMapping("/workflow-runs/{id}/replay")
    @ResponseStatus(HttpStatus.ACCEPTED)
    public void replay(@PathVariable UUID id) {
        final var workflowRun = getWorkflowRunById(id);
        final var userId = SecurityUtil.findCurrentUserId().orElse(null);
        if (userId == null || !userId.equals(workflowRun.getUserId())) {
            throw forbidden("Workflow run with id = " + id + " can be replayed only by the user who started it");
        }
        if (!applicationManager.getBeanOfType(WorkflowRunService.class).replay(workflowRun)) {
            throw conflict("Workflow run with id = " + id + " cannot be replayed: only a failed run which kept everything it held can be");
        }
    }

    @DeleteMapping("/workflow-runs/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void delete(@PathVariable UUID id) {
        final var workflowRun = getWorkflowRunById(id);
        // a run in progress has no delete button on the runs page either: deleted now, its next step would put it back
        // into the cache, and its end into the database
        if (workflowRun.isRunning()) {
            throw conflict("Workflow run with id = " + id + " is in progress, and has to be cancelled first");
        }
        applicationManager.getBeanOfType(WorkflowRunService.class).delete(workflowRun);
    }

    /**
     * A run is found by its whole id only: the filter of the runs page finds every run whose id contains what is typed,
     * which is no way to point at the one run to delete.
     */
    private WorkflowRun getWorkflowRunById(UUID id) {
        return applicationManager.getBeanOfType(WorkflowRunService.class).getAllItems()
            .filter(workflowRun -> id.equals(workflowRun.getId()))
            .findFirst()
            .orElseThrow(() -> notFound("Could not find workflow run with id = " + id));
    }

}
