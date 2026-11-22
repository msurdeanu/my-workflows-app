package org.myworkflows.restapi.v1;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowDownload;
import org.myworkflows.service.WorkflowDownloadService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.server.ResponseStatusException;

import java.io.ByteArrayInputStream;
import java.nio.file.NoSuchFileException;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowDownloadControllerTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private WorkflowDownloadService workflowDownloadService;

    private WorkflowDownloadController workflowDownloadController;

    private final UUID runId = UUID.randomUUID();

    private final WorkflowDownload workflowDownload = new WorkflowDownload(runId, "page.html", 25, Instant.now(), null, null);

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(WorkflowDownloadService.class)).thenReturn(workflowDownloadService);
        workflowDownloadController = new WorkflowDownloadController(applicationManager);
    }

    @Test
    public void whenFilesOfARunAreListedThenTheyAreDescribed() {
        // given
        when(workflowDownloadService.findAllOfRun(runId)).thenReturn(List.of(workflowDownload));

        // when
        final var response = workflowDownloadController.all(runId).getFirst();

        // then
        assertEquals(runId + "/page.html", response.id());
        assertEquals("page.html", response.fileName());
        assertEquals(25, response.size());
    }

    @Test
    public void whenContentIsAskedForThenItIsAFileToSaveWhateverItHolds() throws Exception {
        // given
        when(workflowDownloadService.find(runId, "page.html")).thenReturn(Optional.of(workflowDownload));
        when(workflowDownloadService.openStream(workflowDownload)).thenReturn(new ByteArrayInputStream("<script>alert(1)</script>".getBytes()));

        // when
        final var response = workflowDownloadController.content(runId, "page.html");

        // then
        assertEquals(MediaType.APPLICATION_OCTET_STREAM, response.getHeaders().getContentType());
        assertEquals(25, response.getHeaders().getContentLength());
        assertTrue(response.getHeaders().getFirst(HttpHeaders.CONTENT_DISPOSITION).startsWith("attachment"));
        assertEquals("sandbox", response.getHeaders().getFirst("Content-Security-Policy"));
        assertEquals("nosniff", response.getHeaders().getFirst("X-Content-Type-Options"));
        assertEquals("<script>alert(1)</script>", new String(response.getBody().getInputStream().readAllBytes()));
    }

    @Test
    public void whenFileIsNotKnownOrGoneThenItIsNotFound() throws Exception {
        // given
        when(workflowDownloadService.find(runId, "gone.txt")).thenReturn(Optional.empty());
        when(workflowDownloadService.find(runId, "page.html")).thenReturn(Optional.of(workflowDownload));
        when(workflowDownloadService.openStream(workflowDownload)).thenThrow(new NoSuchFileException("page.html"));

        // when and then
        assertStatus(() -> workflowDownloadController.one(runId, "gone.txt"));
        assertStatus(() -> workflowDownloadController.content(runId, "gone.txt"));
        assertStatus(() -> workflowDownloadController.content(runId, "page.html"));
    }

    private static void assertStatus(Runnable call) {
        assertEquals(HttpStatus.NOT_FOUND, assertThrows(ResponseStatusException.class, call::run).getStatusCode());
    }

}
