package org.myworkflows.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.SimpleTransactionStatus;
import org.springframework.transaction.support.TransactionTemplate;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.0
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowRunProgressServiceTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private WorkflowRunService workflowRunService;

    @Mock
    private PlatformTransactionManager transactionManager;

    private WorkflowRunProgressService workflowRunProgressService;

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(WorkflowRunService.class)).thenReturn(workflowRunService);
        workflowRunProgressService = new WorkflowRunProgressService(applicationManager, new TransactionTemplate(transactionManager));
    }

    @Test
    public void whenRunIsInProgressThenNoTransactionIsOpened() {
        // given
        final var workflowRun = new WorkflowRun();

        // when
        workflowRunProgressService.onEventReceived(WorkflowDefinitionOnProgressEvent.of(workflowRun, false));

        // then the run is only refreshed in the cache
        verify(workflowRunService).create(workflowRun, false);
        verify(workflowRunService, never()).deleteOldEntriesIfNeeded(anyBoolean());
        verifyNoInteractions(transactionManager);
    }

    @Test
    public void whenRunIsCompletedThenRunIsSavedAndOldRunsAreDeletedInsideTheSameTransaction() {
        // given
        final var workflowRun = new WorkflowRun();
        final var transactionStatus = new SimpleTransactionStatus();
        when(transactionManager.getTransaction(any())).thenReturn(transactionStatus);

        // when
        workflowRunProgressService.onEventReceived(WorkflowDefinitionOnProgressEvent.of(workflowRun, true));

        // then
        final var inOrder = inOrder(transactionManager, workflowRunService);
        inOrder.verify(transactionManager).getTransaction(any());
        inOrder.verify(workflowRunService).create(workflowRun, true);
        inOrder.verify(workflowRunService).deleteOldEntriesIfNeeded(false);
        inOrder.verify(transactionManager).commit(transactionStatus);
    }

    @Test
    public void whenRunCannotBeSavedThenTransactionIsRolledBack() {
        // given
        final var workflowRun = new WorkflowRun();
        final var transactionStatus = new SimpleTransactionStatus();
        when(transactionManager.getTransaction(any())).thenReturn(transactionStatus);
        doThrow(new IllegalStateException("database is gone")).when(workflowRunService).create(workflowRun, true);

        // when and then
        final var event = WorkflowDefinitionOnProgressEvent.of(workflowRun, true);
        assertThrows(IllegalStateException.class, () -> workflowRunProgressService.onEventReceived(event));
        verify(transactionManager).rollback(transactionStatus);
        verify(transactionManager, never()).commit(any());
        verify(workflowRunService, never()).deleteOldEntriesIfNeeded(anyBoolean());
    }

}
