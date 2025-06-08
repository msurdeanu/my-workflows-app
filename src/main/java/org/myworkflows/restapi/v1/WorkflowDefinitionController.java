package org.myworkflows.restapi.v1;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.restapi.dto.WorkflowDefinitionResponse;
import org.myworkflows.service.WorkflowDefinitionService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RestController
@RequestMapping("/api/v1")
@RequiredArgsConstructor
@ConditionalOnProperty(name = "my-workflows.config.feature.restApiEnabled", havingValue = "true")
public class WorkflowDefinitionController {

    private final ApplicationManager applicationManager;

    @GetMapping("/workflow-definitions")
    public List<WorkflowDefinitionResponse> all() {
        return applicationManager.getBeanOfType(WorkflowDefinitionService.class).getAll()
            .map(WorkflowDefinitionResponse::of)
            .toList();
    }

    @GetMapping("/workflow-definitions/{id}")
    public WorkflowDefinitionResponse one(@PathVariable int id) {
        return WorkflowDefinitionResponse.of(getWorkflowDefinitionById(id));
    }

    @PostMapping("/workflow-definitions/{id}/schedule-now")
    public String scheduleNow(@RequestBody Map<String, Object> parameters, @PathVariable int id) {
        final var workflowDefinition = getWorkflowDefinitionById(id);
        final var uuid = UUID.randomUUID();
        applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(WorkflowDefinitionOnSubmitEvent.builder()
            .token(uuid)
            .workflowRun(new WorkflowRun(parameters))
            .workflowDefinitionScript(workflowDefinition.getScript())
            .build());
        return uuid.toString();
    }

    private WorkflowDefinition getWorkflowDefinitionById(int id) {
        return applicationManager.getBeanOfType(WorkflowDefinitionService.class)
            .getAll(new WorkflowDefinitionFilter().idCriteria(id), 0, 1)
            .findFirst()
            .orElseThrow(() -> new WorkflowRuntimeException("Could not find workflow definition with id = " + id));
    }

}
