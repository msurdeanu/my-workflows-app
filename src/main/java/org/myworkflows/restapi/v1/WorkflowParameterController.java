package org.myworkflows.restapi.v1;

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.restapi.dto.WorkflowParameterRequest;
import org.myworkflows.restapi.dto.WorkflowParameterResponse;
import org.myworkflows.service.WorkflowParameterService;
import org.myworkflows.service.WorkflowTemplateService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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
import java.util.List;
import java.util.Optional;

import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.joining;
import static org.myworkflows.restapi.RestApiUtil.badRequest;
import static org.myworkflows.restapi.RestApiUtil.conflict;
import static org.myworkflows.restapi.RestApiUtil.notFound;
import static org.myworkflows.restapi.RestApiUtil.requireName;

/**
 * The parameters, for admins only, like their page.
 *
 * @author Mihai Surdeanu
 * @since 2.2
 */
@RestController
@RequestMapping("/api/v1")
@RequiredArgsConstructor
@ConditionalOnProperty(name = "my-workflows.config.feature.restApiEnabled", havingValue = "true")
public class WorkflowParameterController {

    /**
     * The characters the parameters page lets through.
     */
    static final String ALLOWED_CHARS_FOR_NAME = "[a-zA-Z0-9_.]";

    private final ApplicationManager applicationManager;

    @GetMapping("/workflow-parameters")
    public List<WorkflowParameterResponse> all() {
        return applicationManager.getBeanOfType(WorkflowParameterService.class).getAll()
            .map(WorkflowParameterResponse::of)
            .toList();
    }

    @GetMapping("/workflow-parameters/{name}")
    public WorkflowParameterResponse one(@PathVariable String name) {
        return WorkflowParameterResponse.of(getWorkflowParameterByName(name));
    }

    @PostMapping("/workflow-parameters")
    public ResponseEntity<WorkflowParameterResponse> create(@RequestBody WorkflowParameterRequest request) {
        final var name = requireName(request.name(), ALLOWED_CHARS_FOR_NAME);
        // the name is what a parameter is saved by, so saving another one under it would overwrite it
        if (findWorkflowParameterByName(name).isPresent()) {
            throw conflict("Workflow parameter with name = " + name + " already exists");
        }
        final var type = ofNullable(request.type()).orElse(WorkflowParameterType.STR);
        final var value = ofNullable(request.value()).orElse(StringUtils.EMPTY);
        validateTypeAndValue(type, value);
        final var workflowParameter = applicationManager.getBeanOfType(WorkflowParameterService.class)
            .create(WorkflowParameter.of(name, type, value), true);
        return ResponseEntity.created(URI.create("/api/v1/workflow-parameters/" + name))
            .body(WorkflowParameterResponse.of(workflowParameter));
    }

    @PutMapping("/workflow-parameters/{name}")
    public WorkflowParameterResponse update(@PathVariable String name, @RequestBody WorkflowParameterRequest request) {
        final var workflowParameter = getWorkflowParameterByName(name);
        if (request.name() != null && !request.name().equals(name)) {
            throw badRequest("A workflow parameter cannot be renamed");
        }
        final var type = ofNullable(request.type()).orElseThrow(() -> badRequest("'type' is missing"));
        final var value = ofNullable(request.value()).orElseThrow(() -> badRequest("'value' is missing"));
        validateTypeAndValue(type, value);
        // the parameter of the cache is changed, like the page changes it: the templates which hold it are told
        workflowParameter.setType(type);
        workflowParameter.setValue(value);
        applicationManager.getBeanOfType(WorkflowParameterService.class).update(workflowParameter);
        return WorkflowParameterResponse.of(workflowParameter);
    }

    /**
     * A parameter which a template hands over is not deleted: the template would keep it, and put it back the next time
     * it is saved.
     */
    @DeleteMapping("/workflow-parameters/{name}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void delete(@PathVariable String name) {
        final var workflowParameter = getWorkflowParameterByName(name);
        final var templateNames = applicationManager.getBeanOfType(WorkflowTemplateService.class).getAllItems()
            .filter(workflowTemplate -> ofNullable(workflowTemplate.getWorkflowParameters()).orElse(List.of()).contains(workflowParameter))
            .map(WorkflowTemplate::getName)
            .collect(joining(", "));
        if (!templateNames.isEmpty()) {
            throw conflict("Workflow parameter with name = " + name + " is used by the templates " + templateNames
                + ", which have to let it go first");
        }
        applicationManager.getBeanOfType(WorkflowParameterService.class).delete(workflowParameter);
    }

    private void validateTypeAndValue(WorkflowParameterType type, String value) {
        WorkflowParameter.validateTypeAndValue(type, value).ifPresent(message -> {
            throw badRequest("'value' does not fit type " + type + ": " + message);
        });
    }

    private Optional<WorkflowParameter> findWorkflowParameterByName(String name) {
        return applicationManager.getBeanOfType(WorkflowParameterService.class).getAllItems()
            .filter(workflowParameter -> workflowParameter.getName().equals(name))
            .findFirst();
    }

    private WorkflowParameter getWorkflowParameterByName(String name) {
        return findWorkflowParameterByName(name)
            .orElseThrow(() -> notFound("Could not find workflow parameter with name = " + name));
    }

}
