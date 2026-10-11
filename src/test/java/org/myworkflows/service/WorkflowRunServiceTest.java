package org.myworkflows.service;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.domain.User;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.repository.UserRepository;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
