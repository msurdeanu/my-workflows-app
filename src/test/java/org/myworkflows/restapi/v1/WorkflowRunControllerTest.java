package org.myworkflows.restapi.v1;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.User;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.service.WorkflowRunService;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.server.ResponseStatusException;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.2
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowRunControllerTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private WorkflowRunService workflowRunService;

    private WorkflowRunController workflowRunController;

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(WorkflowRunService.class)).thenReturn(workflowRunService);
        workflowRunController = new WorkflowRunController(applicationManager);
    }

    @AfterEach
    public void clearContext() {
        SecurityContextHolder.clearContext();
    }

    @Test
    public void whenIdIsNotTheWholeIdOfARunThenNothingIsFound() {
        // given two runs, whose ids have at least one character in common
        final var firstRun = new WorkflowRun();
        final var secondRun = new WorkflowRun();
        whenRunsAre(firstRun, secondRun);

        // when and then
        assertEquals(secondRun.getId(), workflowRunController.one(secondRun.getId()).id());
        assertStatus(HttpStatus.NOT_FOUND, () -> workflowRunController.one(UUID.randomUUID()));
        assertStatus(HttpStatus.NOT_FOUND, () -> workflowRunController.delete(UUID.randomUUID()));
        verify(workflowRunService, never()).delete(any());
    }

    @Test
    public void whenRunIsNotInProgressThenItCannotBeCancelled() {
        // given
        final var workflowRun = new WorkflowRun();
        whenRunsAre(workflowRun);

        // when and then
        assertStatus(HttpStatus.CONFLICT, () -> workflowRunController.cancel(workflowRun.getId()));
    }

    @Test
    public void whenRunIsInProgressThenItIsCancelledButNotDeleted() {
        // given
        final var workflowRun = new WorkflowRun();
        final var future = new CompletableFuture<Void>();
        workflowRun.setFuture(future);
        whenRunsAre(workflowRun);

        // when and then
        assertStatus(HttpStatus.CONFLICT, () -> workflowRunController.delete(workflowRun.getId()));
        verify(workflowRunService, never()).delete(any());
        workflowRunController.cancel(workflowRun.getId());
        assertTrue(future.isCancelled());

        // when it is over
        workflowRunController.delete(workflowRun.getId());

        // then
        verify(workflowRunService).delete(workflowRun);
    }

    @Test
    public void whenRunWasStartedByAnotherUserThenItIsNotReplayed() {
        // given a run of user 7, and one the cron of its template started
        final var runOfAnotherUser = new WorkflowRun(null, Map.of(), 7);
        final var runOfTheScheduler = new WorkflowRun(1);
        whenRunsAre(runOfAnotherUser, runOfTheScheduler);
        authenticateAs(8);

        // when and then
        assertStatus(HttpStatus.FORBIDDEN, () -> workflowRunController.replay(runOfAnotherUser.getId()));
        assertStatus(HttpStatus.FORBIDDEN, () -> workflowRunController.replay(runOfTheScheduler.getId()));
        verify(workflowRunService, never()).replay(any());
    }

    @Test
    public void whenRunOfTheUserCannotBeReplayedThenTheCallIsAConflict() {
        // given
        final var workflowRun = new WorkflowRun(null, Map.of(), 7);
        whenRunsAre(workflowRun);
        authenticateAs(7);
        when(workflowRunService.replay(workflowRun)).thenReturn(false);

        // when and then
        assertStatus(HttpStatus.CONFLICT, () -> workflowRunController.replay(workflowRun.getId()));
    }

    @Test
    public void whenRunOfTheUserCanBeReplayedThenItIs() {
        // given
        final var workflowRun = new WorkflowRun(null, Map.of(), 7);
        whenRunsAre(workflowRun);
        authenticateAs(7);
        when(workflowRunService.replay(workflowRun)).thenReturn(true);

        // when
        workflowRunController.replay(workflowRun.getId());

        // then
        verify(workflowRunService).replay(workflowRun);
    }

    @Test
    public void whenRunPrintedValuesThenTheyAreReadInTheOrderTheyWerePrinted() {
        // given
        final var workflowRun = new WorkflowRun(Map.of("total", 42, "dbPassword", "s3cret", "unseen", 1));
        workflowRun.markKeyAsPrinted("total");
        workflowRun.markKeyAsPrinted("dbPassword");
        whenRunsAre(workflowRun);

        // when
        final var prints = workflowRunController.prints(workflowRun.getId());

        // then
        assertEquals(2, prints.size());
        assertEquals("total", prints.get(0).name());
        assertEquals("42", prints.get(0).value());
        assertEquals("dbPassword", prints.get(1).name());
        assertEquals("******", prints.get(1).value());
    }

    private void whenRunsAre(WorkflowRun... workflowRuns) {
        when(workflowRunService.getAllItems()).thenAnswer(_ -> Stream.of(workflowRuns));
    }

    private void authenticateAs(int userId) {
        final var user = mock(User.class);
        when(user.getId()).thenReturn(userId);
        SecurityContextHolder.getContext().setAuthentication(new UsernamePasswordAuthenticationToken(user, null, List.of()));
    }

    static void assertStatus(HttpStatus status, Runnable call) {
        assertEquals(status, assertThrows(ResponseStatusException.class, call::run).getStatusCode());
    }

}
