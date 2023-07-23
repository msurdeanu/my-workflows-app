package org.myworkflows.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.ExecutionContext;
import org.myworkflows.domain.Workflow;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowResultEvent;
import org.myworkflows.domain.event.WorkflowScheduleEvent;
import org.myworkflows.domain.event.WorkflowSubmitEvent;
import org.springframework.stereotype.Service;

import java.util.concurrent.ThreadPoolExecutor;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class WorkflowService implements EventListener<WorkflowSubmitEvent> {

    private final EventBroadcaster eventBroadcaster;

    private final ThreadPoolExecutor threadPoolExecutor;

    @Override
    public void onEventReceived(WorkflowSubmitEvent event) {
        final var workflow = event.getWorkflow();
        final var executionContext = ofNullable(event.getExecutionContext())
            .orElseGet(ExecutionContext::new);
        final var contextFuture = threadPoolExecutor.submit(() -> {
            runSynchronously(workflow, executionContext);
            eventBroadcaster.broadcast(WorkflowResultEvent.builder().executionContext(executionContext).build());
            return executionContext;
        });
        eventBroadcaster.broadcast(WorkflowScheduleEvent.builder().executionContextFuture(contextFuture).build());
    }

    @Override
    public Class<WorkflowSubmitEvent> getEventType() {
        return WorkflowSubmitEvent.class;
    }

    private void runSynchronously(final Workflow workflow,
                                  final ExecutionContext executionContext) {
        final var startTime = System.currentTimeMillis();
        try {
            workflow.getCommands().forEach(command -> command.run(executionContext));
        } catch (Exception exception) {
            executionContext.markCommandAsFailed(exception);
        } finally {
            try {
                workflow.getFinallyCommands().forEach(command -> command.run(executionContext));
            } catch (Exception exception) {
                executionContext.markCommandAsFailed(exception);
            }
            executionContext.markAsCompleted(System.currentTimeMillis() - startTime);
        }
    }

}
