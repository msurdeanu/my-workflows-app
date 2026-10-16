package org.myworkflows.restapi.v1;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.exception.WorkflowDuplicateException;
import org.myworkflows.restapi.dto.WorkflowDuplicateRequest;
import org.myworkflows.restapi.dto.WorkflowTemplateRequest;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.service.WorkflowParameterService;
import org.myworkflows.service.WorkflowTemplateService;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.definition;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.template;
import static org.myworkflows.restapi.v1.WorkflowRunControllerTest.assertStatus;

/**
 * @author Mihai Surdeanu
 * @since 2.2
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowTemplateControllerTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private WorkflowTemplateService workflowTemplateService;

    @Mock
    private WorkflowDefinitionService workflowDefinitionService;

    @Mock
    private WorkflowParameterService workflowParameterService;

    private final WorkflowDefinition backup = definition(1, "Backup", "commands: []");

    private final WorkflowDefinition cleanup = definition(2, "Cleanup", "commands: []");

    private final WorkflowParameter timeout = WorkflowParameter.of("timeout");

    private WorkflowTemplateController workflowTemplateController;

    @BeforeEach
    public void init() {
        lenient().when(applicationManager.getBeanOfType(WorkflowTemplateService.class)).thenReturn(workflowTemplateService);
        lenient().when(applicationManager.getBeanOfType(WorkflowDefinitionService.class)).thenReturn(workflowDefinitionService);
        lenient().when(applicationManager.getBeanOfType(WorkflowParameterService.class)).thenReturn(workflowParameterService);
        lenient().when(workflowDefinitionService.getAllItems()).thenAnswer(_ -> Stream.of(backup, cleanup));
        lenient().when(workflowParameterService.getAllItems()).thenAnswer(_ -> Stream.of(timeout));
        workflowTemplateController = new WorkflowTemplateController(applicationManager);
    }

    @Test
    public void whenTemplateIsCreatedThenItRunsItsDefinitionsInTheOrderTheyAreGiven() {
        // given a save, which gives the template its id
        final var saved = new AtomicReference<WorkflowTemplate>();
        when(workflowTemplateService.create(any(), eq(true))).thenAnswer(invocation -> {
            saved.set(spy(invocation.<WorkflowTemplate>getArgument(0)));
            doReturn(9).when(saved.get()).getId();
            return saved.get();
        });

        // when
        final var response = workflowTemplateController.create(new WorkflowTemplateRequest(" Nightly ", " 0 0 2 * * * ", true, List.of(2, 1),
            List.of("timeout")));

        // then
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
        assertEquals("/api/v1/workflow-templates/9", response.getHeaders().getLocation().toString());
        // it is saved empty and disabled, as JPA cannot persist it with definitions which are saved already
        final var captor = ArgumentCaptor.forClass(WorkflowTemplate.class);
        verify(workflowTemplateService).create(captor.capture(), eq(true));
        assertEquals("Nightly", captor.getValue().getName());
        assertTrue(captor.getValue().getWorkflowDefinitions().isEmpty());
        assertFalse(captor.getValue().isEnabled());
        // then it is given everything else, and scheduled, like an update
        verify(workflowTemplateService).update(saved.get(), "Nightly", "0 0 2 * * *", true, List.of(cleanup, backup), List.of(timeout));
    }

    @Test
    public void whenCronOrDefinitionsAreNotAcceptedThenNoTemplateIsCreated() {
        // when and then
        final var badCron = assertThrows(ResponseStatusException.class,
            () -> workflowTemplateController.create(new WorkflowTemplateRequest("Nightly", "every night", false, List.of(), List.of())));
        assertEquals(HttpStatus.BAD_REQUEST, badCron.getStatusCode());
        assertEquals("'cron' is not a valid cron expression: every night", badCron.getReason());
        final var unknownDefinition = assertThrows(ResponseStatusException.class,
            () -> workflowTemplateController.create(new WorkflowTemplateRequest("Nightly", null, false, List.of(1, 3), List.of())));
        assertEquals("'definitionIds' holds 3, which is not known", unknownDefinition.getReason());
        final var twice = assertThrows(ResponseStatusException.class,
            () -> workflowTemplateController.create(new WorkflowTemplateRequest("Nightly", null, false, List.of(1, 1), List.of())));
        assertEquals("'definitionIds' holds 1 twice", twice.getReason());
        assertStatus(HttpStatus.BAD_REQUEST,
            () -> workflowTemplateController.create(new WorkflowTemplateRequest("Nightly", null, false, List.of(), List.of("retries"))));
        assertStatus(HttpStatus.BAD_REQUEST, () -> workflowTemplateController.create(new WorkflowTemplateRequest(null, null, false, null, null)));
        verify(workflowTemplateService, never()).create(any(), anyBoolean());
    }

    @Test
    public void whenTemplateIsUpdatedThenEverythingItIsMadeOfIsReplaced() {
        // given
        final var nightly = template(5, "Nightly", "0 0 2 * * *", true, List.of(backup), List.of(timeout));
        when(workflowTemplateService.getAllItems()).thenAnswer(_ -> Stream.of(nightly));

        // when a blank cron is no cron, and what is left out is gone
        workflowTemplateController.update(5, new WorkflowTemplateRequest("Weekly", " ", false, List.of(2), null));

        // then
        verify(workflowTemplateService).update(nightly, "Weekly", null, false, List.of(cleanup), List.of());
    }

    @Test
    public void whenTemplateIsDuplicatedThenItsOwnParametersAreCopiedUnlessTheBodySaysNo() {
        // given
        final var nightly = template(5, "Nightly", "0 0 2 * * *", true, List.of(backup), List.of(timeout));
        final var copy = template(9, "Nightly (copy)", "0 0 2 * * *", false, List.of(backup), List.of(timeout));
        when(workflowTemplateService.getAllItems()).thenAnswer(_ -> Stream.of(nightly));
        when(workflowTemplateService.suggestCopyName(nightly)).thenReturn("Nightly (copy)");
        when(workflowTemplateService.duplicate(eq(nightly), any(), anyBoolean())).thenReturn(copy);

        // when
        final var response = workflowTemplateController.duplicate(5, null);
        workflowTemplateController.duplicate(5, new WorkflowDuplicateRequest("Nightly EU", false));
        workflowTemplateController.duplicate(5, new WorkflowDuplicateRequest(null, null));

        // then
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
        assertEquals("/api/v1/workflow-templates/9", response.getHeaders().getLocation().toString());
        assertFalse(response.getBody().enabled());
        verify(workflowTemplateService, times(2)).duplicate(nightly, "Nightly (copy)", true);
        verify(workflowTemplateService).duplicate(nightly, "Nightly EU", false);
    }

    @Test
    public void whenOriginalChangedThenTheCopyIsRefused() {
        // given
        final var nightly = template(5, "Nightly", null, false, List.of(backup), List.of());
        when(workflowTemplateService.getAllItems()).thenAnswer(_ -> Stream.of(nightly));
        when(workflowTemplateService.duplicate(nightly, "Nightly v2", true))
            .thenThrow(new WorkflowDuplicateException(WorkflowDuplicateException.Reason.ORIGINAL_CHANGED, "Nightly"));

        // when and then
        assertStatus(HttpStatus.CONFLICT, () -> workflowTemplateController.duplicate(5, new WorkflowDuplicateRequest("Nightly v2", null)));
    }

    @Test
    public void whenIdIsNotTheOneOfATemplateThenNothingIsFound() {
        // given a template, which a filter without an id would find first
        final var nightly = template(5, "Nightly", null, false, List.of(), List.of());
        when(workflowTemplateService.getAllItems()).thenAnswer(_ -> Stream.of(nightly));

        // when and then
        assertEquals(5, workflowTemplateController.one(5).id());
        assertNull(workflowTemplateController.one(5).cron());
        assertStatus(HttpStatus.NOT_FOUND, () -> workflowTemplateController.one(0));
        assertStatus(HttpStatus.NOT_FOUND, () -> workflowTemplateController.delete(6));
        verify(workflowTemplateService, never()).delete(any());
    }

}
