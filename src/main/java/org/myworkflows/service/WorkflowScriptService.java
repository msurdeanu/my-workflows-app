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
import java.util.UUID;
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
        final var workflowParameters = onSubmitEvent.getWorkflowParameters();

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
            onSubmittedEventBuilder.workflowRun(submit(fromJsonToObject(workflowAsString, WorkflowDefinitionScript.class), workflowParameters, onSubmitEvent));
        } else if (workflowDefScriptObject instanceof WorkflowDefinitionScript workflowDefinitionScript) {
            onSubmittedEventBuilder.workflowRun(submit(workflowDefinitionScript, workflowParameters, onSubmitEvent));
        }

        applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(onSubmittedEventBuilder.build());
    }

    @Override
    public Class<WorkflowDefinitionOnSubmitEvent> getEventType() {
        return WorkflowDefinitionOnSubmitEvent.class;
    }

    private WorkflowRun submit(WorkflowDefinitionScript workflowDefinitionScript,
                               Map<String, Object> workflowParameters,
                               WorkflowDefinitionOnSubmitEvent onSubmitEvent) {
        final var workflowRun = ofNullable(onSubmitEvent.getWorkflowRun())
            .orElseGet(WorkflowRun::new);
        injectParametersIntoRun(workflowParameters, workflowRun);
        threadPoolExecutor.submit(() -> runSynchronously(workflowDefinitionScript, workflowRun, onSubmitEvent.getToken()));
        return workflowRun;
    }

    private void injectParametersIntoRun(Map<String, Object> workflowParameters, WorkflowRun workflowRun) {
        final var workflowRunCache = workflowRun.getCache();
        ofNullable(workflowParameters).orElse(Map.of()).forEach(workflowRunCache::put);
    }

    private void runSynchronously(WorkflowDefinitionScript workflowDefinitionScript,
                                  WorkflowRun workflowRun,
                                  UUID token) {
        final var startTime = System.currentTimeMillis();
        applicationManager.getBeanOfType(EventBroadcaster.class)
            .broadcast(createWorkflowDefinitionOnProgressEvent(workflowRun, token, false));
        try {
            workflowDefinitionScript.getCommands().forEach(command -> {
                resolveCommandPlaceholders(command);
                command.run(workflowRun);
                applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(createWorkflowDefinitionOnProgressEvent(workflowRun, token, false));
            });
        } catch (Exception exception) {
            workflowRun.markAsFailed(exception);
        } finally {
            try {
                workflowDefinitionScript.getFinallyCommands().forEach(command -> {
                    resolveCommandPlaceholders(command);
                    command.run(workflowRun);
                    applicationManager.getBeanOfType(EventBroadcaster.class)
                        .broadcast(createWorkflowDefinitionOnProgressEvent(workflowRun, token, false));
                });
            } catch (Exception exception) {
                workflowRun.markAsFailed(exception);
            }
            workflowRun.markAsCompleted(System.currentTimeMillis() - startTime);
            applicationManager.getBeanOfType(EventBroadcaster.class)
                .broadcast(createWorkflowDefinitionOnProgressEvent(workflowRun, token, true));
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
        items.forEach(item -> item.setValue(resolvePlaceholders(item.getValue())));
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

    private WorkflowDefinitionOnProgressEvent createWorkflowDefinitionOnProgressEvent(WorkflowRun workflowRun, UUID token, boolean persisted) {
        final var onProgressEventBuilder = WorkflowDefinitionOnProgressEvent.builder();
        onProgressEventBuilder.workflowRun(workflowRun);
        onProgressEventBuilder.token(token);
        onProgressEventBuilder.persisted(persisted);
        return onProgressEventBuilder.build();
    }

}
