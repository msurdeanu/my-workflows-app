package org.myworkflows.restapi.v1;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowReportFormat;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.service.WorkflowReportService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.3
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowReportControllerTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private WorkflowReportService workflowReportService;

    private WorkflowReportController workflowReportController;

    private final WorkflowRun workflowRun = new WorkflowRun(4, null, 2);

    private final WorkflowReport workflowReport = new WorkflowReport(workflowRun.getId(), "Health check.html", WorkflowReportFormat.HTML, 4, 2, 12,
        Instant.now());

    private final String id = workflowRun.getId() + "/Health check.html";

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(WorkflowReportService.class)).thenReturn(workflowReportService);
        workflowReportController = new WorkflowReportController(applicationManager);
    }

    @Test
    public void whenReportsAreListedThenTheyCanBeTheOnesOfARun() {
        // given
        when(workflowReportService.getAll()).thenReturn(Stream.of(workflowReport));
        when(workflowReportService.findAllOfRun(workflowRun.getId())).thenReturn(Stream.of(workflowReport));

        // when and then
        assertEquals(List.of("Health check"), workflowReportController.all(null).stream().map(response -> response.title()).toList());
        final var response = workflowReportController.all(workflowRun.getId()).getFirst();
        assertEquals(workflowRun.getId(), response.workflowRunId());
        assertEquals(4, response.workflowTemplateId());
        assertEquals(2, response.userId());
        assertEquals("html", response.format());
        assertEquals(id, response.id());
        assertEquals("Health check", response.title());
        assertEquals(12, response.size());
    }

    @Test
    public void whenReportIsNotKnownThenNothingIsFoundAndNothingIsDeleted() {
        // given
        when(workflowReportService.findById(workflowRun.getId() + "/Gone.md")).thenReturn(Optional.empty());

        // when and then
        assertStatus(HttpStatus.NOT_FOUND, () -> workflowReportController.one(workflowRun.getId(), "Gone.md"));
        assertStatus(HttpStatus.NOT_FOUND, () -> workflowReportController.content(workflowRun.getId(), "Gone.md"));
        assertStatus(HttpStatus.NOT_FOUND, () -> workflowReportController.delete(workflowRun.getId(), "Gone.md"));
        verify(workflowReportService, never()).delete(any());
    }

    @Test
    public void whenContentIsAskedForThenItIsAFileWhichNoBrowserRuns() {
        // given
        when(workflowReportService.findById(id)).thenReturn(Optional.of(workflowReport));
        when(workflowReportService.findContent(workflowReport)).thenReturn(Optional.of("<script>alert(1)</script>"));

        // when
        final var response = workflowReportController.content(workflowRun.getId(), "Health check.html");

        // then
        assertEquals("<script>alert(1)</script>", response.getBody());
        assertEquals("text/html;charset=UTF-8", response.getHeaders().getContentType().toString());
        assertTrue(response.getHeaders().getFirst(HttpHeaders.CONTENT_DISPOSITION).startsWith("attachment"));
        assertTrue(response.getHeaders().getFirst(HttpHeaders.CONTENT_DISPOSITION).contains("Health check.html"));
        assertEquals("sandbox", response.getHeaders().getFirst("Content-Security-Policy"));
        assertEquals("nosniff", response.getHeaders().getFirst("X-Content-Type-Options"));
    }

    @Test
    public void whenFileOfAReportIsGoneThenItsContentIsNotFound() {
        // given
        when(workflowReportService.findById(id)).thenReturn(Optional.of(workflowReport));
        when(workflowReportService.findContent(workflowReport)).thenReturn(Optional.empty());

        // when and then
        assertStatus(HttpStatus.NOT_FOUND, () -> workflowReportController.content(workflowRun.getId(), "Health check.html"));
    }

    @Test
    public void whenReportIsDeletedThenTheServiceDeletesIt() {
        // given
        when(workflowReportService.findById(id)).thenReturn(Optional.of(workflowReport));

        // when
        workflowReportController.delete(workflowRun.getId(), "Health check.html");

        // then
        verify(workflowReportService).delete(workflowReport);
    }

    private static void assertStatus(HttpStatus status, Runnable call) {
        assertEquals(status, assertThrows(ResponseStatusException.class, call::run).getStatusCode());
    }

}
