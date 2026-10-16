package org.myworkflows.restapi.v1;

import com.networknt.schema.Error;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.exception.WorkflowDuplicateException;
import org.myworkflows.restapi.dto.WorkflowDefinitionRequest;
import org.myworkflows.restapi.dto.WorkflowDuplicateRequest;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.service.WorkflowScriptService;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.definition;
import static org.myworkflows.restapi.v1.WorkflowRunControllerTest.assertStatus;

/**
 * @author Mihai Surdeanu
 * @since 2.2
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowDefinitionControllerTest {

    private static final String SCRIPT = """
        commands:
        - name: Wait
          class: sleep
          inputs:
          - name: sleep.time
            value: 10
        """;

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private WorkflowDefinitionService workflowDefinitionService;

    @Mock
    private WorkflowScriptService workflowScriptService;

    private final WorkflowDefinition backup = definition(1, "Backup", "commands: []");

    private WorkflowDefinitionController workflowDefinitionController;

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(WorkflowDefinitionService.class)).thenReturn(workflowDefinitionService);
        lenient().when(applicationManager.getBeanOfType(WorkflowScriptService.class)).thenReturn(workflowScriptService);
        lenient().when(workflowDefinitionService.getAllItems()).thenAnswer(_ -> Stream.of(backup));
        workflowDefinitionController = new WorkflowDefinitionController(applicationManager);
    }

    @Test
    public void whenScriptIsNotAValidWorkflowThenTheDefinitionIsNotUpdated() {
        // given
        when(workflowScriptService.validate(anyString())).thenReturn(List.of(Error.builder().message("$.commands[0]: required property 'name' not found").build()));

        // when and then
        final var exception = assertThrows(ResponseStatusException.class,
            () -> workflowDefinitionController.update(1, new WorkflowDefinitionRequest("Backup", "commands:\n- class: sleep\n")));
        assertEquals(HttpStatus.BAD_REQUEST, exception.getStatusCode());
        assertEquals("'script' is not a valid workflow: $.commands[0]: required property 'name' not found", exception.getReason());
        // a script is needed for an update, as an update replaces it
        assertStatus(HttpStatus.BAD_REQUEST, () -> workflowDefinitionController.update(1, new WorkflowDefinitionRequest("Backup", null)));
        verify(workflowDefinitionService, never()).update(any(), anyString(), any());
    }

    @Test
    public void whenScriptIsValidThenNameAndScriptAreUpdatedTogether() {
        // given
        when(workflowScriptService.validate(SCRIPT)).thenReturn(List.of());

        // when
        workflowDefinitionController.update(1, new WorkflowDefinitionRequest(" Nightly backup ", SCRIPT));

        // then
        final var captor = ArgumentCaptor.forClass(WorkflowDefinitionScript.class);
        verify(workflowDefinitionService).update(eq(backup), eq("Nightly backup"), captor.capture());
        assertEquals("Wait", captor.getValue().getCommands().getFirst().getName());
    }

    @Test
    public void whenDefinitionIsCreatedWithoutScriptThenItHasNoCommand() {
        // given a save, which gives the definition its id
        when(workflowDefinitionService.create(any(), eq(true))).thenAnswer(invocation -> {
            final var saved = spy(invocation.<WorkflowDefinition>getArgument(0));
            doReturn(3).when(saved).getId();
            return saved;
        });

        // when
        final var response = workflowDefinitionController.create(new WorkflowDefinitionRequest("Cleanup", null));

        // then
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
        assertEquals("/api/v1/workflow-definitions/3", response.getHeaders().getLocation().toString());
        assertEquals(3, response.getBody().id());
        final var captor = ArgumentCaptor.forClass(WorkflowDefinition.class);
        verify(workflowDefinitionService).create(captor.capture(), eq(true));
        assertEquals("Cleanup", captor.getValue().getName());
        assertTrue(captor.getValue().getScript().getCommands().isEmpty());
        verify(workflowScriptService, never()).validate(anyString());
    }

    @Test
    public void whenDefinitionIsRunByATemplateThenItIsNotDeleted() {
        // given
        when(workflowDefinitionService.delete(backup)).thenReturn(0);

        // when and then
        assertStatus(HttpStatus.CONFLICT, () -> workflowDefinitionController.delete(1));
        assertStatus(HttpStatus.NOT_FOUND, () -> workflowDefinitionController.delete(0));
    }

    @Test
    public void whenDefinitionIsDuplicatedWithoutBodyThenTheCopyGetsTheSuggestedName() {
        // given
        final var copy = definition(31, "Backup (copy)", "commands: []");
        when(workflowDefinitionService.suggestCopyName(backup)).thenReturn("Backup (copy)");
        when(workflowDefinitionService.duplicate(backup, "Backup (copy)")).thenReturn(copy);

        // when
        final var response = workflowDefinitionController.duplicate(1, null);

        // then
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
        assertEquals("/api/v1/workflow-definitions/31", response.getHeaders().getLocation().toString());
        assertEquals("Backup (copy)", response.getBody().name());
    }

    @Test
    public void whenNameOfTheCopyIsTakenOrBlankThenItIsRefused() {
        // given
        when(workflowDefinitionService.duplicate(backup, "Backup"))
            .thenThrow(new WorkflowDuplicateException(WorkflowDuplicateException.Reason.NAME_TAKEN, "Backup"));
        when(workflowDefinitionService.duplicate(backup, " "))
            .thenThrow(new WorkflowDuplicateException(WorkflowDuplicateException.Reason.NAME_MISSING, " "));

        // when and then
        final var taken = assertThrows(ResponseStatusException.class,
            () -> workflowDefinitionController.duplicate(1, new WorkflowDuplicateRequest("Backup", null)));
        assertEquals(HttpStatus.CONFLICT, taken.getStatusCode());
        assertEquals("'name' is taken: Backup", taken.getReason());
        assertStatus(HttpStatus.BAD_REQUEST, () -> workflowDefinitionController.duplicate(1, new WorkflowDuplicateRequest(" ", null)));
        assertStatus(HttpStatus.NOT_FOUND, () -> workflowDefinitionController.duplicate(2, null));
    }

}
