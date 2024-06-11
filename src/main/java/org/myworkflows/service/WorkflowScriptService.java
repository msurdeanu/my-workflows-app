package org.myworkflows.service;

import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.domain.command.AbstractSubCommand;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmittedEvent;
import org.myworkflows.repository.PlaceholderRepository;
import org.myworkflows.util.PlaceholderUtil;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.stream.Collectors;

import static java.util.Optional.ofNullable;
import static org.myworkflows.serializer.JsonFactory.fromJsonToObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
public final class WorkflowScriptService implements EventListener<WorkflowDefinitionOnSubmitEvent> {

    private final ThreadPoolExecutor threadPoolExecutor;

    private final ApplicationManager applicationManager;

    public WorkflowScriptService(final @Qualifier("workflow-pool") ThreadPoolExecutor threadPoolExecutor,
                                 final ApplicationManager applicationManager) {
        this.threadPoolExecutor = threadPoolExecutor;
        this.applicationManager = applicationManager;
    }

    @Override
    public void onEventReceived(WorkflowDefinitionOnSubmitEvent onSubmitEvent) {
        final var workflowDefScriptObject = onSubmitEvent.getWorkflowDefinitionScript();

        final var onSubmittedEventBuilder = WorkflowDefinitionOnSubmittedEvent.builder();
        onSubmittedEventBuilder.token(onSubmitEvent.getToken());

        if (workflowDefScriptObject instanceof String workflowAsString) {
            final var validationMessages = applicationManager.getBeanOfType(WorkflowDefinitionValidatorService.class)
                .validate(workflowAsString);
            onSubmittedEventBuilder.validationMessages(validationMessages);
            if (!validationMessages.isEmpty()) {
                applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(onSubmittedEventBuilder.build());
                return;
            }
            onSubmittedEventBuilder.workflowRun(submit(fromJsonToObject(workflowAsString, WorkflowDefinitionScript.class), onSubmitEvent));
        } else if (workflowDefScriptObject instanceof WorkflowDefinitionScript workflowDefinitionScript) {
            onSubmittedEventBuilder.workflowRun(submit(workflowDefinitionScript, onSubmitEvent));
        }

        applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(onSubmittedEventBuilder.build());
    }

    @Override
    public Class<WorkflowDefinitionOnSubmitEvent> getEventType() {
        return WorkflowDefinitionOnSubmitEvent.class;
    }

    private WorkflowRun submit(WorkflowDefinitionScript workflowDefinitionScript, WorkflowDefinitionOnSubmitEvent onSubmitEvent) {
        final var workflowRun = ofNullable(onSubmitEvent.getWorkflowRun())
            .orElseGet(WorkflowRun::new);
        final var onProgressEventBuilder = WorkflowDefinitionOnProgressEvent.builder();
        onProgressEventBuilder.token(onSubmitEvent.getToken());
        onProgressEventBuilder.workflowRun(workflowRun);
        threadPoolExecutor.submit(() -> runSynchronously(workflowDefinitionScript, onProgressEventBuilder.build()));
        return workflowRun;
    }

    private void runSynchronously(WorkflowDefinitionScript workflowDefinitionScript,
                                  WorkflowDefinitionOnProgressEvent workflowResultEvent) {
        final var workflowRun = workflowResultEvent.getWorkflowRun();
        final var startTime = System.currentTimeMillis();
        applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(workflowResultEvent);
        try {
            workflowDefinitionScript.getCommands().forEach(command -> {
                resolveCommandPlaceholders(command);
                command.run(workflowRun);
                // TODO
                //workflowRun.markCommandAsCompleted();
                applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(workflowResultEvent);
            });
        } catch (Exception exception) {
            workflowRun.markAsFailed(exception);
        } finally {
            try {
                workflowDefinitionScript.getFinallyCommands().forEach(command -> {
                    resolveCommandPlaceholders(command);
                    command.run(workflowRun);
                    // TODO
                    //workflowRun.markCommandAsCompleted();
                    applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(workflowResultEvent);
                });
            } catch (Exception exception) {
                workflowRun.markAsFailed(exception);
            }
            workflowRun.markAsCompleted(System.currentTimeMillis() - startTime);
            applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(workflowResultEvent);
        }
    }

    private void resolveCommandPlaceholders(AbstractCommand abstractCommand) {
        resolveItemPlaceholders(abstractCommand.getInputs());
        resolveItemPlaceholders(abstractCommand.getAsserts());
        resolveItemPlaceholders(abstractCommand.getOutputs());
        if (abstractCommand instanceof AbstractSubCommand subCommand) {
            subCommand.getSubcommands().forEach(this::resolveCommandPlaceholders);
        }
    }

    private void resolveItemPlaceholders(Collection<ExpressionNameValue> items) {
        items.forEach(item -> {
            item.setName((String) resolvePlaceholders(item.getName()));
            item.setValue(resolvePlaceholders(item.getValue()));
        });
    }

    private Object resolvePlaceholders(Object value) {
        if (value instanceof String valueAsString) {
            return PlaceholderUtil.resolvePlaceholders(valueAsString,
                applicationManager.getBeanOfType(PlaceholderRepository.class).getAllAsMap());
        } else if (value instanceof List<?> valueAsList) {
            return valueAsList.stream()
                .map(this::resolvePlaceholders)
                .collect(Collectors.toList());
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
