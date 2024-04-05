package org.myworkflows.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.networknt.schema.JsonSchema;
import com.networknt.schema.ValidationMessage;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.ExecutionContext;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.domain.command.AbstractSubCommand;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowOnProgressEvent;
import org.myworkflows.domain.event.WorkflowOnSubmitEvent;
import org.myworkflows.domain.event.WorkflowOnSubmittedEvent;
import org.myworkflows.repository.PlaceholderRepository;
import org.myworkflows.util.PlaceholderUtil;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.stream.Collectors;

import static java.util.Optional.ofNullable;
import static org.myworkflows.serializer.JsonFactory.fromJsonToObject;
import static org.myworkflows.serializer.JsonFactory.fromJsonToSchema;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
public class WorkflowService implements EventListener<WorkflowOnSubmitEvent> {

    private static final JsonSchema WORKFLOW_SCHEMA;

    private final EventBroadcaster eventBroadcaster;

    private final ThreadPoolExecutor threadPoolExecutor;

    private final PlaceholderRepository placeholderRepository;

    public WorkflowService(final EventBroadcaster eventBroadcaster,
                           final @Qualifier("workflow-pool") ThreadPoolExecutor threadPoolExecutor,
                           final PlaceholderRepository placeholderRepository) {
        this.eventBroadcaster = eventBroadcaster;
        this.threadPoolExecutor = threadPoolExecutor;
        this.placeholderRepository = placeholderRepository;
    }

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
    public void onEventReceived(final WorkflowOnSubmitEvent onSubmitEvent) {
        final var workflowObject = onSubmitEvent.getWorkflow();

        final var onSubmittedEventBuilder = WorkflowOnSubmittedEvent.builder();
        onSubmittedEventBuilder.token(onSubmitEvent.getToken());

        if (workflowObject instanceof String workflowAsString) {
            final var validationMessages = validateWorkflow(workflowAsString);
            onSubmittedEventBuilder.validationMessages(validationMessages);
            if (!validationMessages.isEmpty()) {
                eventBroadcaster.broadcast(onSubmittedEventBuilder.build());
                return;
            }
            onSubmittedEventBuilder.executionContextFuture(submit(fromJsonToObject(workflowAsString, WorkflowDefinition.class),
                    onSubmitEvent));
        } else if (workflowObject instanceof WorkflowDefinition workflowDefinition) {
            onSubmittedEventBuilder.executionContextFuture(submit(workflowDefinition, onSubmitEvent));
        }

        eventBroadcaster.broadcast(onSubmittedEventBuilder.build());
    }

    @Override
    public Class<WorkflowOnSubmitEvent> getEventType() {
        return WorkflowOnSubmitEvent.class;
    }

    private Set<ValidationMessage> validateWorkflow(final String wokflowAsString) {
        return WORKFLOW_SCHEMA.validate(fromJsonToObject(wokflowAsString, JsonNode.class));
    }

    private Future<?> submit(final WorkflowDefinition workflowDefinition,
                             final WorkflowOnSubmitEvent onSubmitEvent) {
        final var executionContext = ofNullable(onSubmitEvent.getExecutionContext())
                .orElseGet(() -> new ExecutionContext(workflowDefinition));
        final var onProgressEventBuilder = WorkflowOnProgressEvent.builder();
        onProgressEventBuilder.token(onSubmitEvent.getToken());
        onProgressEventBuilder.executionContext(executionContext);
        return threadPoolExecutor.submit(() -> runSynchronously(workflowDefinition, onProgressEventBuilder.build()));
    }

    private void runSynchronously(final WorkflowDefinition workflowDefinition,
                                  final WorkflowOnProgressEvent workflowResultEvent) {
        final var executionContext = workflowResultEvent.getExecutionContext();
        final var startTime = System.currentTimeMillis();
        try {
            resolveGlobalPlaceholders(workflowDefinition);
            workflowDefinition.getCommands().forEach(command -> {
                command.run(executionContext);
                executionContext.markCommandAsCompleted();
                eventBroadcaster.broadcast(workflowResultEvent);
            });
        } catch (Exception exception) {
            executionContext.markCommandAsFailed(exception);
        } finally {
            try {
                workflowDefinition.getFinallyCommands().forEach(command -> {
                    command.run(executionContext);
                    executionContext.markCommandAsCompleted();
                    eventBroadcaster.broadcast(workflowResultEvent);
                });
            } catch (Exception exception) {
                executionContext.markCommandAsFailed(exception);
            }
            executionContext.markAsCompleted(System.currentTimeMillis() - startTime);
            eventBroadcaster.broadcast(workflowResultEvent);
        }
    }

    private void resolveGlobalPlaceholders(final WorkflowDefinition workflowDefinition) {
        workflowDefinition.getCommands().forEach(this::resolveGlobalCommandPlaceholders);
        workflowDefinition.getFinallyCommands().forEach(this::resolveGlobalCommandPlaceholders);
    }

    private void resolveGlobalCommandPlaceholders(final AbstractCommand abstractCommand) {
        resolveItemPlaceholders(abstractCommand.getInputs());
        resolveItemPlaceholders(abstractCommand.getAsserts());
        resolveItemPlaceholders(abstractCommand.getOutputs());
        if (abstractCommand instanceof AbstractSubCommand subCommand) {
            subCommand.getSubcommands().forEach(this::resolveGlobalCommandPlaceholders);
        }
    }

    private void resolveItemPlaceholders(final Collection<ExpressionNameValue> items) {
        items.forEach(item -> {
            item.setName((String) resolvePlaceholders(item.getName()));
            item.setValue(resolvePlaceholders(item.getValue()));
        });
    }

    private Object resolvePlaceholders(final Object value) {
        if (value instanceof String valueAsString) {
            return PlaceholderUtil.resolvePlaceholders(valueAsString, placeholderRepository.getAllAsMap());
        } else if (value instanceof List<?> valueAsList) {
            return valueAsList.stream().map(this::resolvePlaceholders).collect(Collectors.toList());
        } else if (value instanceof Map<?, ?> valueAsMap) {
            return valueAsMap.entrySet().stream()
                    .collect(Collectors.toMap(
                            Map.Entry::getKey,
                            entry -> resolvePlaceholders(entry.getValue())
                    ));
        }

        return value;
    }

}
