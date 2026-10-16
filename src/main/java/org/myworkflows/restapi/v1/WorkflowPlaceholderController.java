package org.myworkflows.restapi.v1;

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.restapi.dto.WorkflowPlaceholderRequest;
import org.myworkflows.restapi.dto.WorkflowPlaceholderResponse;
import org.myworkflows.service.WorkflowPlaceholderService;
import org.myworkflows.util.PlaceholderUtil;
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
import static org.myworkflows.restapi.RestApiUtil.badRequest;
import static org.myworkflows.restapi.RestApiUtil.conflict;
import static org.myworkflows.restapi.RestApiUtil.notFound;
import static org.myworkflows.restapi.RestApiUtil.requireName;

/**
 * The placeholders, for admins only, like their page.
 *
 * @author Mihai Surdeanu
 * @since 2.2
 */
@RestController
@RequestMapping("/api/v1")
@RequiredArgsConstructor
@ConditionalOnProperty(name = "my-workflows.config.feature.restApiEnabled", havingValue = "true")
public class WorkflowPlaceholderController {

    private final ApplicationManager applicationManager;

    @GetMapping("/workflow-placeholders")
    public List<WorkflowPlaceholderResponse> all() {
        return applicationManager.getBeanOfType(WorkflowPlaceholderService.class).getAll()
            .map(WorkflowPlaceholderResponse::of)
            .toList();
    }

    @GetMapping("/workflow-placeholders/{name}")
    public WorkflowPlaceholderResponse one(@PathVariable String name) {
        return WorkflowPlaceholderResponse.of(getWorkflowPlaceholderByName(name));
    }

    @PostMapping("/workflow-placeholders")
    public ResponseEntity<WorkflowPlaceholderResponse> create(@RequestBody WorkflowPlaceholderRequest request) {
        final var name = requireName(request.name(), PlaceholderUtil.ALLOWED_CHARS_FOR_PLACEHOLDER_NAME);
        // the name is what a placeholder is saved by, so saving another one under it would overwrite it
        if (findWorkflowPlaceholderByName(name).isPresent()) {
            throw conflict("Workflow placeholder with name = " + name + " already exists");
        }
        final var workflowPlaceholder = applicationManager.getBeanOfType(WorkflowPlaceholderService.class)
            .create(WorkflowPlaceholder.of(name, ofNullable(request.value()).orElse(StringUtils.EMPTY)), true);
        return ResponseEntity.created(URI.create("/api/v1/workflow-placeholders/" + name))
            .body(WorkflowPlaceholderResponse.of(workflowPlaceholder));
    }

    @PutMapping("/workflow-placeholders/{name}")
    public WorkflowPlaceholderResponse update(@PathVariable String name, @RequestBody WorkflowPlaceholderRequest request) {
        final var workflowPlaceholder = getWorkflowPlaceholderByName(name);
        if (request.name() != null && !request.name().equals(name)) {
            throw badRequest("A workflow placeholder cannot be renamed");
        }
        workflowPlaceholder.setValue(ofNullable(request.value()).orElseThrow(() -> badRequest("'value' is missing")));
        applicationManager.getBeanOfType(WorkflowPlaceholderService.class).update(workflowPlaceholder);
        return WorkflowPlaceholderResponse.of(workflowPlaceholder);
    }

    @DeleteMapping("/workflow-placeholders/{name}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void delete(@PathVariable String name) {
        applicationManager.getBeanOfType(WorkflowPlaceholderService.class).delete(getWorkflowPlaceholderByName(name));
    }

    private Optional<WorkflowPlaceholder> findWorkflowPlaceholderByName(String name) {
        return applicationManager.getBeanOfType(WorkflowPlaceholderService.class).getAllItems()
            .filter(workflowPlaceholder -> workflowPlaceholder.getName().equals(name))
            .findFirst();
    }

    private WorkflowPlaceholder getWorkflowPlaceholderByName(String name) {
        return findWorkflowPlaceholderByName(name)
            .orElseThrow(() -> notFound("Could not find workflow placeholder with name = " + name));
    }

}
