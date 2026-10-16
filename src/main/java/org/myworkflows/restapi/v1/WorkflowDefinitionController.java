package org.myworkflows.restapi.v1;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.networknt.schema.Error;
import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.restapi.dto.WorkflowDefinitionRequest;
import org.myworkflows.restapi.RestApiUtil;
import org.myworkflows.restapi.dto.WorkflowDefinitionResponse;
import org.myworkflows.restapi.dto.WorkflowDuplicateRequest;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.service.WorkflowScriptService;
import org.myworkflows.util.SecurityUtil;
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
import java.util.Map;

import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.joining;
import static org.myworkflows.restapi.RestApiUtil.badRequest;
import static org.myworkflows.restapi.RestApiUtil.conflict;
import static org.myworkflows.restapi.RestApiUtil.notFound;
import static org.myworkflows.restapi.RestApiUtil.requireText;
import static org.myworkflows.serializer.SerializerFactory.toObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0
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

    @PostMapping("/workflow-definitions")
    public ResponseEntity<WorkflowDefinitionResponse> create(@RequestBody WorkflowDefinitionRequest request) {
        final var name = requireText(request.name(), "name");
        final var script = request.script() == null ? WorkflowDefinitionScript.empty() : readScript(request.script());
        final var workflowDefinition = applicationManager.getBeanOfType(WorkflowDefinitionService.class)
            .create(WorkflowDefinition.of(name, script), true);
        return ResponseEntity.created(URI.create("/api/v1/workflow-definitions/" + workflowDefinition.getId()))
            .body(WorkflowDefinitionResponse.of(workflowDefinition));
    }

    @PutMapping("/workflow-definitions/{id}")
    public WorkflowDefinitionResponse update(@PathVariable int id, @RequestBody WorkflowDefinitionRequest request) {
        final var workflowDefinition = getWorkflowDefinitionById(id);
        final var name = requireText(request.name(), "name");
        final var script = readScript(request.script());
        applicationManager.getBeanOfType(WorkflowDefinitionService.class).update(workflowDefinition, name, script);
        return WorkflowDefinitionResponse.of(workflowDefinition);
    }

    @DeleteMapping("/workflow-definitions/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void delete(@PathVariable int id) {
        final var workflowDefinition = getWorkflowDefinitionById(id);
        if (applicationManager.getBeanOfType(WorkflowDefinitionService.class).delete(workflowDefinition) == 0) {
            throw conflict("Workflow definition with id = " + id + " is run by a template, which has to let it go first");
        }
    }

    /**
     * Saves a new definition with the script of the given one, which the templates that run it go on with.
     */
    @PostMapping("/workflow-definitions/{id}/duplicate")
    public ResponseEntity<WorkflowDefinitionResponse> duplicate(@PathVariable int id, @RequestBody(required = false) WorkflowDuplicateRequest request) {
        final var workflowDefinition = getWorkflowDefinitionById(id);
        final var workflowDefinitionService = applicationManager.getBeanOfType(WorkflowDefinitionService.class);
        final var name = ofNullable(request).map(WorkflowDuplicateRequest::name)
            .orElseGet(() -> workflowDefinitionService.suggestCopyName(workflowDefinition));
        final var copy = RestApiUtil.duplicate(() -> workflowDefinitionService.duplicate(workflowDefinition, name));
        return ResponseEntity.created(URI.create("/api/v1/workflow-definitions/" + copy.getId()))
            .body(WorkflowDefinitionResponse.of(copy));
    }

    @PostMapping("/workflow-definitions/{id}/schedule-now")
    public String scheduleNow(@RequestBody Map<String, Object> parameters, @PathVariable int id) {
        final var workflowDefinition = getWorkflowDefinitionById(id);
        final var workflowRun = new WorkflowRun(null, parameters, SecurityUtil.findCurrentUserId().orElse(null));
        applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(WorkflowDefinitionOnSubmitEvent.builder()
            .workflowRun(workflowRun)
            .workflowDefinitionScript(workflowDefinition.getScript())
            .build());
        return workflowRun.getId().toString();
    }

    /**
     * A script is checked against the schema before it is saved, which the editor leaves to the first run of it: a
     * caller of the API is told at once what is wrong, and where.
     */
    private WorkflowDefinitionScript readScript(String script) {
        // checked only, as a YAML text keeps the blanks it starts with
        requireText(script, "script");
        final var validationMessages = applicationManager.getBeanOfType(WorkflowScriptService.class).validate(script);
        if (!validationMessages.isEmpty()) {
            throw badRequest("'script' is not a valid workflow: " + validationMessages.stream().map(this::describe).collect(joining("; ")));
        }
        try {
            return toObject(script, WorkflowDefinitionScript.class);
        } catch (WorkflowRuntimeException exception) {
            throw badRequest("'script' cannot be read: " + describe(exception));
        }
    }

    private String describe(WorkflowRuntimeException exception) {
        // the parser knows where it stopped, but a value of the wrong type does not name its line by itself
        if (exception.getCause() instanceof JsonProcessingException cause) {
            final var message = cause.getOriginalMessage();
            return ofNullable(cause.getLocation())
                .filter(location -> !message.contains("line " + location.getLineNr()))
                .map(location -> message + " (line " + location.getLineNr() + ")")
                .orElse(message);
        }
        return exception.getMessage();
    }

    private String describe(Error validationMessage) {
        return validationMessage.getInstanceLocation() == null ? validationMessage.getMessage() : validationMessage.toString();
    }

    private WorkflowDefinition getWorkflowDefinitionById(int id) {
        return applicationManager.getBeanOfType(WorkflowDefinitionService.class).getAllItems()
            .filter(workflowDefinition -> workflowDefinition.getId() == id)
            .findFirst()
            .orElseThrow(() -> notFound("Could not find workflow definition with id = " + id));
    }

}
