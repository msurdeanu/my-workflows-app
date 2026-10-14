package org.myworkflows.service;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.domain.User;
import org.myworkflows.domain.UserAccountDetails;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunApproval;
import org.myworkflows.domain.WorkflowRunDebugger;
import org.myworkflows.domain.WorkflowRunPause;
import org.myworkflows.domain.WorkflowRunPause.Action;
import org.myworkflows.domain.WorkflowRunPause.Resume;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.repository.UserRepository;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static java.util.Optional.ofNullable;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.0
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowRunServiceTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private InternalCacheManager internalCacheManager;

    @Mock
    private SettingProvider settingProvider;

    @Mock
    private UserRepository userRepository;

    @Mock
    private EventBroadcaster eventBroadcaster;

    private WorkflowRunService workflowRunService;

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(InternalCacheManager.class)).thenReturn(internalCacheManager);
        when(applicationManager.getBeanOfType(SettingProvider.class)).thenReturn(settingProvider);
        when(internalCacheManager.getCache(anyString()))
            .thenReturn(new InternalCache(CacheNameEnum.WORKFLOW_RUN.getName(), 10, InternalCache.InternalCacheOrder.FIFO));
        workflowRunService = new WorkflowRunService(applicationManager);
    }

    @AfterEach
    public void clearContext() {
        SecurityContextHolder.clearContext();
    }

    @Test
    public void whenRunHasUserThenUsernameIsReadOnlyOnce() {
        // given
        final var user = new User();
        user.setUsername("john");
        when(applicationManager.getBeanOfType(UserRepository.class)).thenReturn(userRepository);
        when(userRepository.findById(7)).thenReturn(Optional.of(user));

        // when and then
        final var workflowRun = new WorkflowRun(null, Map.of(), 7);
        assertEquals(Optional.of("john"), workflowRunService.findUsername(workflowRun));
        assertEquals(Optional.of("john"), workflowRunService.findUsername(workflowRun));
        verify(userRepository, times(1)).findById(7);
    }

    @Test
    public void whenRunHasNoUserOrUserIsGoneThenNoUsernameIsFound() {
        // given
        when(applicationManager.getBeanOfType(UserRepository.class)).thenReturn(userRepository);
        when(userRepository.findById(7)).thenReturn(Optional.empty());

        // when and then
        assertEquals(Optional.empty(), workflowRunService.findUsername(new WorkflowRun(1, Map.of())));
        assertEquals(Optional.empty(), workflowRunService.findUsername(new WorkflowRun(null, Map.of(), 7)));
    }

    @Test
    public void whenRunIsReplayedByItsUserThenRunIsSubmittedAgain() {
        // given
        when(applicationManager.getBeanOfType(EventBroadcaster.class)).thenReturn(eventBroadcaster);
        final var workflowRun = createFailedRun(7);
        authenticateAs(7);

        // when and then
        assertTrue(workflowRunService.replay(workflowRun));
        assertFalse(workflowRun.isEligibleForReplay());
        verify(eventBroadcaster).broadcast(any(WorkflowDefinitionOnSubmitEvent.class));
    }

    @Test
    public void whenRunIsReplayedByAnotherUserThenRunIsLeftUntouched() {
        // given
        final var workflowRun = createFailedRun(7);
        authenticateAs(8);

        // when and then
        assertFalse(workflowRunService.replay(workflowRun));
        assertNotNull(workflowRun.getFailureMessage());
        verify(applicationManager, never()).getBeanOfType(EventBroadcaster.class);
    }

    @Test
    public void whenRunWithoutUserIsReplayedThenRunIsLeftUntouched() {
        // given a run scheduled by its template
        final var workflowRun = createFailedRun(null);
        authenticateAs(7);

        // when and then
        assertFalse(workflowRunService.replay(workflowRun));
        assertNotNull(workflowRun.getFailureMessage());
        verify(applicationManager, never()).getBeanOfType(EventBroadcaster.class);
    }

    @Test
    public void whenApprovalIsDecidedThenTheFirstAnswerOfASignedInUserCounts() throws Exception {
        // given a run which waits for an approval
        final var workflowRun = new WorkflowRun();
        final var pendingApproval = new CompletableFuture<WorkflowRunApproval>();
        workflowRun.setChangeListener(() -> ofNullable(workflowRun.getApproval()).ifPresent(pendingApproval::complete));

        try (var executor = Executors.newSingleThreadExecutor()) {
            final var approvedBy = executor.submit(() -> workflowRun.awaitApproval("Approve", "Go live?", Duration.ofSeconds(30)));
            final var approval = pendingApproval.get(30, TimeUnit.SECONDS);

            // when nobody is signed in, then nothing is decided
            assertFalse(workflowRunService.decideApproval(workflowRun, approval, true));
            assertTrue(workflowRun.isWaitingForApproval());

            // when a user approves, then the run goes on in their name and any other answer is refused
            final var user = new User();
            user.setUsername("john");
            SecurityContextHolder.getContext().setAuthentication(new UsernamePasswordAuthenticationToken(new UserAccountDetails(user), null, List.of()));
            assertTrue(workflowRunService.decideApproval(workflowRun, approval, true));
            assertEquals("john", approvedBy.get(30, TimeUnit.SECONDS));
            assertFalse(workflowRunService.decideApproval(workflowRun, approval, false));
        }
    }

    @Test
    public void whenPausedRunIsToldHowToGoOnThenOnlyTheUserWhoStartedItIsFollowed() throws Exception {
        // given a run of user 7 which is paused
        final var workflowRun = new WorkflowRun(null, Map.of(), 7);
        workflowRun.setDebugger(WorkflowRunDebugger.pausingBefore(Set.of()));
        final var pendingPause = new CompletableFuture<WorkflowRunPause>();
        workflowRun.setChangeListener(() -> ofNullable(workflowRun.getPause()).ifPresent(pendingPause::complete));

        try (var executor = Executors.newSingleThreadExecutor()) {
            final var told = executor.submit(() -> workflowRun.awaitResume(0, "Start", Duration.ofSeconds(30)));
            final var pause = pendingPause.get(30, TimeUnit.SECONDS);
            final var step = new Resume(Action.STEP, 0, null, Set.of(), "john");

            // when another user answers, or nobody is signed in, then the run stays paused
            assertFalse(workflowRunService.resume(workflowRun, pause, step));
            authenticateAs(8);
            assertFalse(workflowRunService.resume(workflowRun, pause, step));
            assertTrue(workflowRun.isPaused());

            // when its user answers, then the run follows and any other answer is refused
            authenticateAs(7);
            assertTrue(workflowRunService.resume(workflowRun, pause, step));
            assertEquals(step, told.get(30, TimeUnit.SECONDS));
            assertFalse(workflowRunService.resume(workflowRun, pause, Resume.stop("john")));
        }
    }

    @Test
    public void whenRunIsStartedAgainFromACommandThenItIsSubmittedWithTheGivenScript() {
        // given a debugged run which is over
        when(applicationManager.getBeanOfType(EventBroadcaster.class)).thenReturn(eventBroadcaster);
        final var workflowRun = createFailedRun(7);
        workflowRun.markAsCompleted(10);
        workflowRun.setDebugger(WorkflowRunDebugger.pausingBefore(Set.of()));
        final var script = WorkflowDefinitionScript.empty();
        authenticateAs(7);

        // when
        assertTrue(workflowRunService.runFrom(workflowRun, 2, script, Set.of(2, 3)));

        // then it is submitted as it is, ready to run its third command
        final var submitted = ArgumentCaptor.forClass(WorkflowDefinitionOnSubmitEvent.class);
        verify(eventBroadcaster).broadcast(submitted.capture());
        assertSame(workflowRun, submitted.getValue().workflowRun());
        assertSame(script, submitted.getValue().workflowDefinitionScript());
        assertNull(workflowRun.getFailureMessage());
        assertEquals(1, workflowRun.getLastSuccessfulIndex());
        // and the breakpoint of the command it starts from does not hold it back
        assertFalse(workflowRun.getDebugger().pausesBefore(2));
        assertTrue(workflowRun.getDebugger().pausesBefore(3));

        // when it is asked for again before the run is over, then it is refused: a run does not run twice at a time
        assertFalse(workflowRunService.runFrom(workflowRun, 0, script, Set.of()));
        assertEquals(1, workflowRun.getLastSuccessfulIndex());
    }

    @Test
    public void whenRunIsStartedAgainByAnotherUserThenRunIsLeftUntouched() {
        // given
        final var workflowRun = createFailedRun(7);
        workflowRun.markAsCompleted(10);
        authenticateAs(8);

        // when and then
        assertFalse(workflowRunService.runFrom(workflowRun, 0, WorkflowDefinitionScript.empty(), Set.of()));
        assertNotNull(workflowRun.getFailureMessage());
        assertEquals(-1, workflowRun.getLastSuccessfulIndex());
        verify(applicationManager, never()).getBeanOfType(EventBroadcaster.class);
    }

    private WorkflowRun createFailedRun(Integer userId) {
        final var workflowRun = new WorkflowRun(1, Map.of(), userId);
        workflowRun.markAsFailed(new WorkflowRuntimeException("test"), WorkflowDefinitionScript.empty());
        return workflowRun;
    }

    private void authenticateAs(int userId) {
        final var user = mock(User.class);
        when(user.getId()).thenReturn(userId);
        SecurityContextHolder.getContext().setAuthentication(new UsernamePasswordAuthenticationToken(user, null, List.of()));
    }

}
