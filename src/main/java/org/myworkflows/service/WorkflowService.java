package org.myworkflows.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.networknt.schema.JsonSchema;
import com.networknt.schema.ValidationMessage;
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

import java.util.Set;
import java.util.concurrent.ThreadPoolExecutor;

import static java.util.Optional.ofNullable;
import static org.myworkflows.serializer.JsonFactory.fromJsonToObject;
import static org.myworkflows.serializer.JsonFactory.fromJsonToSchema;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class WorkflowService implements EventListener<WorkflowSubmitEvent> {

    private static final JsonSchema WORKFLOW_SCHEMA;

    private final EventBroadcaster eventBroadcaster;

    private final ThreadPoolExecutor threadPoolExecutor;

    static {
        final var schemaNode = fromJsonToObject("""
                {
                "$schema": "http://json-schema.org/draft-06/schema#",
                "properties": { "id": {"type": "number"}}
                }
                """, JsonNode.class);
        WORKFLOW_SCHEMA = fromJsonToSchema(schemaNode);
        WORKFLOW_SCHEMA.initializeValidators();
    }

    @Override
    public void onEventReceived(final WorkflowSubmitEvent event) {
        final var workflowAsString = event.getWorkflowAsString();
        final var validationMessages = validateWorkflow(workflowAsString);
        final var workflowScheduleEventBuilder = WorkflowScheduleEvent.builder();
        workflowScheduleEventBuilder.validationMessages(validationMessages);
        if (validationMessages.isEmpty()) {
            final var executionContext = ofNullable(event.getExecutionContext())
                    .orElseGet(ExecutionContext::new);
            final var contextFuture = threadPoolExecutor.submit(() -> {
                runSynchronously(fromJsonToObject(workflowAsString, Workflow.class), executionContext);
                eventBroadcaster.broadcast(WorkflowResultEvent.builder().executionContext(executionContext).build());
                return executionContext;
            });
            workflowScheduleEventBuilder.executionContextFuture(contextFuture);
        }
        eventBroadcaster.broadcast(workflowScheduleEventBuilder.build());
    }

    @Override
    public Class<WorkflowSubmitEvent> getEventType() {
        return WorkflowSubmitEvent.class;
    }

    private Set<ValidationMessage> validateWorkflow(final String wokflowAsString) {
        return WORKFLOW_SCHEMA.validate(fromJsonToObject(wokflowAsString, JsonNode.class));
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
