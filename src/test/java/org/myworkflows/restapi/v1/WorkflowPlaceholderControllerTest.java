package org.myworkflows.restapi.v1;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.restapi.dto.WorkflowPlaceholderRequest;
import org.myworkflows.service.WorkflowPlaceholderService;
import org.springframework.http.HttpStatus;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
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
public final class WorkflowPlaceholderControllerTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private WorkflowPlaceholderService workflowPlaceholderService;

    private final WorkflowPlaceholder apiUrl = WorkflowPlaceholder.of("API.URL", "https://example.org");

    private final WorkflowPlaceholder apiPassword = WorkflowPlaceholder.of("API.PASSWORD", "s3cret");

    private WorkflowPlaceholderController workflowPlaceholderController;

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(WorkflowPlaceholderService.class)).thenReturn(workflowPlaceholderService);
        lenient().when(workflowPlaceholderService.getAllItems()).thenAnswer(_ -> Stream.of(apiUrl, apiPassword));
        workflowPlaceholderController = new WorkflowPlaceholderController(applicationManager);
    }

    @Test
    public void whenNameIsTakenOrNotAcceptedThenNoPlaceholderIsCreated() {
        // when and then: a name in lower case is one the page would not let through
        assertStatus(HttpStatus.CONFLICT, () -> workflowPlaceholderController.create(new WorkflowPlaceholderRequest("API.URL", "https://example.com")));
        assertStatus(HttpStatus.BAD_REQUEST, () -> workflowPlaceholderController.create(new WorkflowPlaceholderRequest("api.url", "https://example.com")));
        verify(workflowPlaceholderService, never()).create(any(), anyBoolean());
        assertEquals("https://example.org", apiUrl.getValue());
    }

    @Test
    public void whenPlaceholderIsUpdatedThenItsValueIsNeeded() {
        // when and then
        assertStatus(HttpStatus.BAD_REQUEST, () -> workflowPlaceholderController.update("API.URL", new WorkflowPlaceholderRequest(null, null)));
        assertStatus(HttpStatus.BAD_REQUEST, () -> workflowPlaceholderController.update("API.URL", new WorkflowPlaceholderRequest("API.HOST", "x")));
        verify(workflowPlaceholderService, never()).update(any());

        // when
        workflowPlaceholderController.update("API.URL", new WorkflowPlaceholderRequest(null, ""));

        // then
        verify(workflowPlaceholderService).update(apiUrl);
        assertEquals("", apiUrl.getValue());
    }

    @Test
    public void whenPlaceholderHoldsASecretThenItsValueIsLeftOut() {
        // when
        final var response = workflowPlaceholderController.update("API.PASSWORD", new WorkflowPlaceholderRequest(null, "n3w"));

        // then it is saved, but not answered
        assertEquals("n3w", apiPassword.getValue());
        assertNull(response.value());
        assertTrue(response.secret());
        assertEquals("https://example.org", workflowPlaceholderController.one("API.URL").value());
    }

}
