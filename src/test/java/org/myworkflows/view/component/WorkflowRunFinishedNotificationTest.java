package org.myworkflows.view.component;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRun;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class WorkflowRunFinishedNotificationTest {

    private static final int USER_ID = 7;

    @Test
    public void testOnlyTheUserWhoStartedARunIsToldThatItIsOver() {
        // given
        final var workflowRun = new WorkflowRun(null, Map.of(), USER_ID);

        // then
        assertTrue(WorkflowRunFinishedNotification.isFor(USER_ID, workflowRun, true));
        assertFalse(WorkflowRunFinishedNotification.isFor(USER_ID + 1, workflowRun, true));
        assertFalse(WorkflowRunFinishedNotification.isFor(null, workflowRun, true));
    }

    @Test
    public void testNobodyIsToldWhileTheRunGoesOn() {
        // given
        final var workflowRun = new WorkflowRun(null, Map.of(), USER_ID);

        // then
        assertFalse(WorkflowRunFinishedNotification.isFor(USER_ID, workflowRun, false));
    }

    @Test
    public void testNobodyIsToldAboutARunOfTheCron() {
        // given a run of a template started by its cron, which belongs to no user
        final var workflowRun = new WorkflowRun(42, Map.of(), null);

        // then
        assertFalse(WorkflowRunFinishedNotification.isFor(USER_ID, workflowRun, true));
        assertFalse(WorkflowRunFinishedNotification.isFor(null, workflowRun, true));
    }

    @Test
    public void testMessageSaysHowTheRunEnded() {
        // given
        final var succeeded = new WorkflowRun(null, Map.of(), USER_ID);
        succeeded.markAsCompleted(120);
        final var failed = new WorkflowRun(null, Map.of(), USER_ID);
        failed.markAsFailed(new IllegalStateException("boom"), null);
        failed.markAsCompleted(120);

        // then
        assertEquals("workflow-runs.notification.finished.success", WorkflowRunFinishedNotification.messageKeyOf(succeeded, false));
        assertEquals("workflow-runs.notification.finished.success.template", WorkflowRunFinishedNotification.messageKeyOf(succeeded, true));
        assertEquals("workflow-runs.notification.finished.error", WorkflowRunFinishedNotification.messageKeyOf(failed, false));
        assertEquals("workflow-runs.notification.finished.error.template", WorkflowRunFinishedNotification.messageKeyOf(failed, true));
    }

    @Test
    public void testRunIsNamedByTheShortIdOfTheRunsPage() {
        // given
        final var workflowRun = new WorkflowRun(null, Map.of(), USER_ID);

        // then
        assertEquals("#" + workflowRun.getId().toString().substring(0, 8), WorkflowRunFinishedNotification.shortIdOf(workflowRun));
    }

}
