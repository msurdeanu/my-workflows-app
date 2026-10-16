package org.myworkflows.restapi.v1;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.restapi.RestApiUtil;
import org.myworkflows.restapi.dto.WorkflowDuplicateRequest;
import org.myworkflows.restapi.dto.WorkflowTemplateRequest;
import org.myworkflows.restapi.dto.WorkflowTemplateResponse;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.service.WorkflowParameterService;
import org.myworkflows.service.WorkflowTemplateService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.support.CronExpression;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.myworkflows.restapi.RestApiUtil.badRequest;
import static org.myworkflows.restapi.RestApiUtil.notFound;
import static org.myworkflows.restapi.RestApiUtil.requireAll;
import static org.myworkflows.restapi.RestApiUtil.requireText;

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

    @PostMapping("/workflow-templates")
    public ResponseEntity<WorkflowTemplateResponse> create(@RequestBody WorkflowTemplateRequest request) {
        final var name = requireText(request.name(), "name");
        final var cron = readCron(request.cron());
        final var definitions = readDefinitions(request.definitionIds());
        final var parameters = readParameters(request.parameterNames());
        // A new template is saved empty first, as the page saves it: JPA cannot persist a template together with
        // definitions which are saved already. It is then given what it is made of - and scheduled - like an update.
        final var workflowTemplateService = applicationManager.getBeanOfType(WorkflowTemplateService.class);
        final var workflowTemplate = workflowTemplateService.create(WorkflowTemplate.of(name, new ArrayList<>(), new ArrayList<>()), true);
        workflowTemplateService.update(workflowTemplate, name, cron, request.isEnabled(), definitions, parameters);
        return ResponseEntity.created(URI.create("/api/v1/workflow-templates/" + workflowTemplate.getId()))
            .body(WorkflowTemplateResponse.of(workflowTemplate));
    }

    @PutMapping("/workflow-templates/{id}")
    public WorkflowTemplateResponse update(@PathVariable int id, @RequestBody WorkflowTemplateRequest request) {
        final var workflowTemplate = getWorkflowTemplateById(id);
        final var name = requireText(request.name(), "name");
        final var cron = readCron(request.cron());
        final var definitions = readDefinitions(request.definitionIds());
        final var parameters = readParameters(request.parameterNames());
        applicationManager.getBeanOfType(WorkflowTemplateService.class)
            .update(workflowTemplate, name, cron, request.isEnabled(), definitions, parameters);
        return WorkflowTemplateResponse.of(workflowTemplate);
    }

    @DeleteMapping("/workflow-templates/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void delete(@PathVariable int id) {
        applicationManager.getBeanOfType(WorkflowTemplateService.class).delete(getWorkflowTemplateById(id));
    }

    /**
     * Saves a new template which runs what the given one runs, with the same parameters and cron. It is paused.
     */
    @PostMapping("/workflow-templates/{id}/duplicate")
    public ResponseEntity<WorkflowTemplateResponse> duplicate(@PathVariable int id, @RequestBody(required = false) WorkflowDuplicateRequest request) {
        final var workflowTemplate = getWorkflowTemplateById(id);
        final var workflowTemplateService = applicationManager.getBeanOfType(WorkflowTemplateService.class);
        final var name = ofNullable(request).map(WorkflowDuplicateRequest::name)
            .orElseGet(() -> workflowTemplateService.suggestCopyName(workflowTemplate));
        final var withOwnParameters = request == null || request.isWithOwnParameters();
        final var copy = RestApiUtil.duplicate(() -> workflowTemplateService.duplicate(workflowTemplate, name, withOwnParameters));
        return ResponseEntity.created(URI.create("/api/v1/workflow-templates/" + copy.getId()))
            .body(WorkflowTemplateResponse.of(copy));
    }

    @PostMapping("/workflow-templates/{id}/schedule-now")
    public void scheduleNow(@PathVariable int id) {
        applicationManager.getBeanOfType(WorkflowTemplateService.class).scheduleNow(getWorkflowTemplateById(id));
    }

    /**
     * @return the cron, or nothing for a template which runs only when it is asked to. A cron which cannot be read is
     *     refused here, as the scheduler would fail on it once the template is saved.
     */
    private String readCron(String cron) {
        if (isBlank(cron)) {
            return null;
        }
        final var trimmedCron = cron.trim();
        if (!CronExpression.isValidExpression(trimmedCron)) {
            throw badRequest("'cron' is not a valid cron expression: " + trimmedCron);
        }
        return trimmedCron;
    }

    private List<WorkflowDefinition> readDefinitions(List<Integer> definitionIds) {
        final var workflowDefinitionService = applicationManager.getBeanOfType(WorkflowDefinitionService.class);
        return requireAll(definitionIds, "definitionIds", id -> workflowDefinitionService.getAllItems()
            .filter(workflowDefinition -> workflowDefinition.getId().equals(id))
            .findFirst()
            .orElse(null));
    }

    private List<WorkflowParameter> readParameters(List<String> parameterNames) {
        final var workflowParameterService = applicationManager.getBeanOfType(WorkflowParameterService.class);
        return requireAll(parameterNames, "parameterNames", name -> workflowParameterService.getAllItems()
            .filter(workflowParameter -> workflowParameter.getName().equals(name))
            .findFirst()
            .orElse(null));
    }

    private WorkflowTemplate getWorkflowTemplateById(int id) {
        return applicationManager.getBeanOfType(WorkflowTemplateService.class).getAllItems()
            .filter(workflowTemplate -> workflowTemplate.getId() == id)
            .findFirst()
            .orElseThrow(() -> notFound("Could not find workflow template with id = " + id));
    }

}
