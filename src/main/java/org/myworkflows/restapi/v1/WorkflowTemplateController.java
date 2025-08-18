package org.myworkflows.restapi.v1;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.filter.WorkflowTemplateFilter;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.restapi.dto.WorkflowTemplateResponse;
import org.myworkflows.service.WorkflowTemplateService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@RestController
@RequestMapping("/api/v1")
@RequiredArgsConstructor
@ConditionalOnProperty(name = "my-workflows.config.feature.restApiEnabled", havingValue = "true")
public class WorkflowTemplateController {

    private final ApplicationManager applicationManager;

    @GetMapping("/workflow-templates")
    public List<WorkflowTemplateResponse> all() {
        return applicationManager.getBeanOfType(WorkflowTemplateService.class).getAll()
            .map(WorkflowTemplateResponse::of)
            .toList();
    }

    @GetMapping("/workflow-templates/{id}")
    public WorkflowTemplateResponse one(@PathVariable int id) {
        return WorkflowTemplateResponse.of(getWorkflowTemplateById(id));
    }

    @PostMapping("/workflow-templates/{id}/schedule-now")
    public void scheduleNow(@PathVariable int id) {
        applicationManager.getBeanOfType(WorkflowTemplateService.class).scheduleNow(getWorkflowTemplateById(id));
    }

    private WorkflowTemplate getWorkflowTemplateById(int id) {
        return applicationManager.getBeanOfType(WorkflowTemplateService.class)
            .getAll(new WorkflowTemplateFilter().idCriteria(id), 0, 1)
            .findFirst()
            .orElseThrow(() -> new WorkflowRuntimeException("Could not find workflow template with id = " + id));
    }

}
