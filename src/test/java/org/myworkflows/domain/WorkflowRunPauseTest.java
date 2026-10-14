package org.myworkflows.domain;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRunPause.Action;
import org.myworkflows.domain.WorkflowRunPause.Resume;

import java.time.Instant;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowRunPauseTest {

    private static final int WAIT_IN_SECONDS = 30;

    @Test
    public void whenRunIsToldHowToGoOnThenOnlyTheFirstAnswerCounts() throws Exception {
        // given
        final var pause = new WorkflowRunPause(2, "Build the report", Instant.now().plusSeconds(WAIT_IN_SECONDS));
        final var step = new Resume(Action.STEP, 2, null, Set.of(4), "john");

        // when
        assertTrue(pause.resume(step));
        assertFalse(pause.resume(Resume.stop("jane")));

        // then
        final var resume = pause.await();
        assertSame(step, resume);
        assertFalse(resume.isTimedOut());
        assertEquals(2, pause.getCommandIndex());
        assertEquals("Build the report", pause.getCommandName());
    }

    @Test
    public void whenNobodyAnswersInTimeThenRunIsToldToStop() throws Exception {
        // given
        final var pause = new WorkflowRunPause(0, "Start", Instant.now().plusMillis(50));

        // when
        final var resume = pause.await();

        // then
        assertEquals(Action.STOP, resume.action());
        assertTrue(resume.isTimedOut());
        // whoever answers too late is told so
        assertFalse(pause.resume(new Resume(Action.CONTINUE, 0, null, Set.of(), "john")));
    }

    @Test
    public void whenRunIsStoppedThenItIsNotTakenForATimeout() throws Exception {
        // given
        final var pause = new WorkflowRunPause(0, "Start", Instant.now().plusSeconds(WAIT_IN_SECONDS));

        // when
        assertTrue(pause.resume(Resume.stop("john")));

        // then
        final var resume = pause.await();
        assertEquals(Action.STOP, resume.action());
        assertEquals("john", resume.username());
        assertFalse(resume.isTimedOut());
    }

    @Test
    public void whenRunIsCancelledWhileItIsPausedThenLaterAnswersAreRefused() throws Exception {
        // given
        final var pause = new WorkflowRunPause(0, "Start", Instant.now().plusSeconds(WAIT_IN_SECONDS));
        final var waiting = new CountDownLatch(1);

        try (var executor = Executors.newSingleThreadExecutor()) {
            // when
            final var paused = executor.submit(() -> {
                waiting.countDown();
                return pause.await();
            });
            assertTrue(waiting.await(WAIT_IN_SECONDS, TimeUnit.SECONDS));
            assertTrue(paused.cancel(true));
            executor.shutdown();
            assertTrue(executor.awaitTermination(WAIT_IN_SECONDS, TimeUnit.SECONDS));

            // then
            assertFalse(pause.resume(new Resume(Action.CONTINUE, 0, null, Set.of(), "john")));
        }
    }

}
