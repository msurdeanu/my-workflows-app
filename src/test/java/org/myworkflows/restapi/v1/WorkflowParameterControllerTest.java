package org.myworkflows.restapi.v1;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.restapi.dto.WorkflowParameterRequest;
import org.myworkflows.service.WorkflowParameterService;
import org.myworkflows.service.WorkflowTemplateService;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.myworkflows.restapi.v1.WorkflowRunControllerTest.assertStatus;

/**
 * @author Mihai Surdeanu
 * @since 2.2
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowParameterControllerTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private WorkflowParameterService workflowParameterService;

    @Mock
    private WorkflowTemplateService workflowTemplateService;

    private final WorkflowParameter timeout = WorkflowParameter.of("timeout", WorkflowParameterType.INT, "30");

    private WorkflowParameterController workflowParameterController;

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(WorkflowParameterService.class)).thenReturn(workflowParameterService);
        workflowParameterController = new WorkflowParameterController(applicationManager);
    }

    @Test
    public void whenNameIsTakenThenNoParameterIsCreated() {
        // given
        whenParametersAre(timeout);

        // when and then the existing one is not overwritten
        assertStatus(HttpStatus.CONFLICT, () -> workflowParameterController.create(new WorkflowParameterRequest("timeout", null, null)));
        verify(workflowParameterService, never()).create(any(), anyBoolean());
    }

    @Test
    public void whenNameOrValueIsNotAcceptedThenNoParameterIsCreated() {
        // given
        whenParametersAre();

        // when and then: a name the page would not let through, none at all, and a value which does not fit its type
        final var badName = assertThrows(ResponseStatusException.class,
            () -> workflowParameterController.create(new WorkflowParameterRequest("time out", null, null)));
        assertEquals(HttpStatus.BAD_REQUEST, badName.getStatusCode());
        assertEquals("'name' accepts [a-zA-Z0-9_.] only", badName.getReason());
        assertStatus(HttpStatus.BAD_REQUEST, () -> workflowParameterController.create(new WorkflowParameterRequest(" ", null, null)));
        final var badValue = assertThrows(ResponseStatusException.class,
            () -> workflowParameterController.create(new WorkflowParameterRequest("retries", WorkflowParameterType.INT, "three")));
        assertEquals("'value' does not fit type INT: Value is not an int", badValue.getReason());
        verify(workflowParameterService, never()).create(any(), anyBoolean());
    }

    @Test
    public void whenParameterIsCreatedWithoutTypeAndValueThenItIsAnEmptyText() {
        // given
        whenParametersAre();
        when(workflowParameterService.create(any(), eq(true))).thenAnswer(invocation -> invocation.getArgument(0));

        // when
        final var response = workflowParameterController.create(new WorkflowParameterRequest("timeout.42", null, null));

        // then
        assertEquals(HttpStatus.CREATED, response.getStatusCode());
        assertEquals("/api/v1/workflow-parameters/timeout.42", response.getHeaders().getLocation().toString());
        final var captor = ArgumentCaptor.forClass(WorkflowParameter.class);
        verify(workflowParameterService).create(captor.capture(), eq(true));
        assertEquals("timeout.42", captor.getValue().getName());
        assertEquals(WorkflowParameterType.STR, captor.getValue().getType());
        assertEquals("", captor.getValue().getValue());
    }

    @Test
    public void whenParameterIsUpdatedThenTypeAndValueAreBothNeeded() {
        // given
        whenParametersAre(timeout);

        // when and then
        assertStatus(HttpStatus.BAD_REQUEST, () -> workflowParameterController.update("timeout", new WorkflowParameterRequest(null, WorkflowParameterType.INT, null)));
        assertStatus(HttpStatus.BAD_REQUEST, () -> workflowParameterController.update("timeout", new WorkflowParameterRequest(null, null, "60")));
        assertStatus(HttpStatus.BAD_REQUEST, () -> workflowParameterController.update("timeout", new WorkflowParameterRequest("delay", WorkflowParameterType.INT, "60")));
        assertStatus(HttpStatus.BAD_REQUEST, () -> workflowParameterController.update("timeout", new WorkflowParameterRequest(null, WorkflowParameterType.BOOL, "60")));
        assertStatus(HttpStatus.NOT_FOUND, () -> workflowParameterController.update("delay", new WorkflowParameterRequest(null, WorkflowParameterType.INT, "60")));
        verify(workflowParameterService, never()).update(any());
        assertEquals("30", timeout.getValue());

        // when
        final var response = workflowParameterController.update("timeout", new WorkflowParameterRequest("timeout", WorkflowParameterType.DOUBLE, "1.5"));

        // then the parameter the templates hold is the one which changed
        verify(workflowParameterService).update(timeout);
        assertEquals(WorkflowParameterType.DOUBLE, timeout.getType());
        assertEquals("1.5", timeout.getValue());
        assertEquals("1.5", response.value());
    }

    @Test
    public void whenParameterHoldsASecretThenItsValueIsLeftOut() {
        // given
        whenParametersAre(WorkflowParameter.of("token", WorkflowParameterType.PASS, "s3cret"), WorkflowParameter.of("dbPassword", WorkflowParameterType.STR, "s3cret"),
            timeout);

        // when
        final var responses = workflowParameterController.all();

        // then
        assertNull(responses.get(0).value());
        assertTrue(responses.get(0).secret());
        assertNull(responses.get(1).value());
        assertTrue(responses.get(1).secret());
        assertEquals("30", responses.get(2).value());
        assertFalse(responses.get(2).secret());
    }

    @Test
    public void whenParameterIsHandedOverByATemplateThenItIsNotDeleted() {
        // given
        final var delay = WorkflowParameter.of("delay");
        whenParametersAre(timeout, delay);
        when(applicationManager.getBeanOfType(WorkflowTemplateService.class)).thenReturn(workflowTemplateService);
        final var nightly = WorkflowTemplate.of("Nightly", new ArrayList<>(), new ArrayList<>(List.of(timeout)));
        when(workflowTemplateService.getAllItems()).thenAnswer(_ -> Stream.of(nightly));

        // when and then
        final var exception = assertThrows(ResponseStatusException.class, () -> workflowParameterController.delete("timeout"));
        assertEquals(HttpStatus.CONFLICT, exception.getStatusCode());
        assertEquals("Workflow parameter with name = timeout is used by the templates Nightly, which have to let it go first", exception.getReason());
        verify(workflowParameterService, never()).delete(any());

        // when
        workflowParameterController.delete("delay");

        // then
        verify(workflowParameterService).delete(delay);
    }

    private void whenParametersAre(WorkflowParameter... workflowParameters) {
        // a lookup reads the items, the list reads all of them through the filter of the service
        lenient().when(workflowParameterService.getAllItems()).thenAnswer(_ -> Stream.of(workflowParameters));
        lenient().when(workflowParameterService.getAll()).thenAnswer(_ -> Stream.of(workflowParameters));
    }

}
