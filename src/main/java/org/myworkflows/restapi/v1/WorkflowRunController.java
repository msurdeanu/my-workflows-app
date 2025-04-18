package org.myworkflows.restapi.v1;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.filter.WorkflowRunFilter;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.restapi.dto.WorkflowRunResponse;
import org.myworkflows.service.WorkflowRunService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RestController
@RequestMapping("/api/v1")
@RequiredArgsConstructor
@ConditionalOnProperty(name = "my-workflows.config.features.rest-api.enabled", havingValue = "true")
public class WorkflowRunController {

    private final ApplicationManager applicationManager;

    @GetMapping("/workflow-runs")
    public List<WorkflowRunResponse> all() {
        return applicationManager.getBeanOfType(WorkflowRunService.class).getAll()
            .map(WorkflowRunResponse::of)
            .toList();
    }

    @GetMapping("/workflow-runs/{id}")
    public WorkflowRunResponse one(@PathVariable String id) {
        return WorkflowRunResponse.of(getWorkflowRunById(id));
    }

    private WorkflowRun getWorkflowRunById(String id) {
        return applicationManager.getBeanOfType(WorkflowRunService.class)
            .getAll(new WorkflowRunFilter().setByRunIdCriteria(id), 0, 1)
            .findFirst()
            .orElseThrow(() -> new WorkflowRuntimeException("Could not find workflow run with id = " + id));
    }

}
