package org.myworkflows.service;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Service
@RequiredArgsConstructor
public class WorkflowRunProgressService implements EventListener<WorkflowDefinitionOnProgressEvent> {

    private final ApplicationManager applicationManager;

    private final TransactionTemplate transactionTemplate;

    @Override
    public void onEventReceived(WorkflowDefinitionOnProgressEvent event) {
        final var service = applicationManager.getBeanOfType(WorkflowRunService.class);
        if (!event.persisted()) {
            // A run in progress lives in the cache only. No transaction is opened for it, because a transaction
            // takes the write lock of the database as soon as it begins: every command of every run would
            // queue behind the writers, and would fail together with them while the database is locked.
            service.create(event.workflowRun(), false);
            return;
        }

        transactionTemplate.executeWithoutResult(_ -> {
            service.create(event.workflowRun(), true);
            service.deleteOldEntriesIfNeeded(false);
        });
    }

    @Override
    public Class<WorkflowDefinitionOnProgressEvent> getEventType() {
        return WorkflowDefinitionOnProgressEvent.class;
    }

}
