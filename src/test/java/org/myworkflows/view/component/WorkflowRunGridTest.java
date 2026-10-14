package org.myworkflows.view.component;

import com.vaadin.flow.data.provider.DataProvider;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunApproval;
import org.myworkflows.domain.WorkflowRunPause;
import org.myworkflows.domain.WorkflowRunPause.Action;
import org.myworkflows.domain.WorkflowRunPause.Resume;
import org.myworkflows.service.WorkflowRunService;

import java.time.Duration;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowRunGridTest {

    @Test
    public void testPageIsReadAgainOnlyWhenARunStarts() {
        // given
        final var workflowRun = new WorkflowRun();
        final var pageReads = new AtomicInteger();
        final var grid = new WorkflowRunGrid(mock(WorkflowRunService.class));
        grid.setDataProvider(DataProvider.fromCallbacks(query -> {
            pageReads.incrementAndGet();
            return Stream.of(workflowRun).skip(query.getOffset()).limit(query.getLimit());
        }, _ -> 1));
        pageReads.set(0);

        // when the run starts
        grid.onRunChange(workflowRun, false);

        // then its row has to be found
        assertEquals(1, pageReads.get());

        // when two of its commands pass and it is completed
        grid.onRunChange(workflowRun, false);
        grid.onRunChange(workflowRun, false);
        grid.onRunChange(workflowRun, true);

        // then the rows stay as they are
        assertEquals(1, pageReads.get());

        // when it is replayed
        grid.onRunChange(workflowRun, false);

        // then
        assertEquals(2, pageReads.get());
    }

    @Test
    public void testPageIsNotReadAgainWhenARunStartsOrStopsToWaitForAnApproval() throws Exception {
        // given a run in progress which the grid already follows
        final var workflowRun = new WorkflowRun();
        final var pageReads = new AtomicInteger();
        final var grid = new WorkflowRunGrid(mock(WorkflowRunService.class));
        grid.setDataProvider(DataProvider.fromCallbacks(query -> {
            pageReads.incrementAndGet();
            return Stream.of(workflowRun).skip(query.getOffset()).limit(query.getLimit());
        }, _ -> 1));
        grid.onRunChange(workflowRun, false);
        pageReads.set(0);

        try (var executor = Executors.newSingleThreadExecutor()) {
            // when the run starts to wait for an approval, then is approved
            final var pendingApproval = new CompletableFuture<WorkflowRunApproval>();
            workflowRun.setChangeListener(() -> ofNullable(workflowRun.getApproval()).ifPresent(pendingApproval::complete));
            final var approvedBy = executor.submit(() -> workflowRun.awaitApproval("Approve", "", Duration.ofSeconds(30)));
            final var approval = pendingApproval.get(30, TimeUnit.SECONDS);
            grid.onRunChange(workflowRun, false);
            approval.approve("john");
            approvedBy.get(30, TimeUnit.SECONDS);
            grid.onRunChange(workflowRun, false);
        }

        // then only its row is drawn again: the rows stay where they are
        assertEquals(0, pageReads.get());
    }

    @Test
    public void testPageIsNotReadAgainWhenARunIsPausedOrToldHowToGoOn() throws Exception {
        // given a debugged run in progress which the grid already follows
        final var workflowRun = new WorkflowRun();
        final var pageReads = new AtomicInteger();
        final var grid = new WorkflowRunGrid(mock(WorkflowRunService.class));
        grid.setDataProvider(DataProvider.fromCallbacks(query -> {
            pageReads.incrementAndGet();
            return Stream.of(workflowRun).skip(query.getOffset()).limit(query.getLimit());
        }, _ -> 1));
        grid.onRunChange(workflowRun, false);
        pageReads.set(0);

        try (var executor = Executors.newSingleThreadExecutor()) {
            // when the run pauses before a command, then is told to go on
            final var pendingPause = new CompletableFuture<WorkflowRunPause>();
            workflowRun.setChangeListener(() -> ofNullable(workflowRun.getPause()).ifPresent(pendingPause::complete));
            final var told = executor.submit(() -> workflowRun.awaitResume(0, "Start", Duration.ofSeconds(30)));
            final var pause = pendingPause.get(30, TimeUnit.SECONDS);
            grid.onRunChange(workflowRun, false);
            pause.resume(new Resume(Action.CONTINUE, 0, null, Set.of(), "john"));
            told.get(30, TimeUnit.SECONDS);
            grid.onRunChange(workflowRun, false);
        }

        // then only its row is drawn again: the rows stay where they are
        assertEquals(0, pageReads.get());
    }

}
