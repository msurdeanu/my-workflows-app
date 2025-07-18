package org.myworkflows.service;

import com.networknt.schema.ValidationMessage;
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
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.util.PlaceholderUtil;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;

import static org.myworkflows.serializer.SerializerFactory.toObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
public final class WorkflowScriptService implements EventListener<WorkflowDefinitionOnSubmitEvent> {

    private final ExecutorService executorService;

    private final ApplicationManager applicationManager;

    public WorkflowScriptService(@Qualifier("workflow-pool") ExecutorService executorService,
                                 ApplicationManager applicationManager) {
        this.executorService = executorService;
        this.applicationManager = applicationManager;
    }

    @Override
    public void onEventReceived(WorkflowDefinitionOnSubmitEvent onSubmitEvent) {
        final var workflowDefScriptObject = onSubmitEvent.getWorkflowDefinitionScript();

        final var onSubmittedEventBuilder = WorkflowDefinitionOnSubmittedEvent.builder();
        onSubmittedEventBuilder.token(onSubmitEvent.getToken());

        if (workflowDefScriptObject instanceof String workflowAsString) {
            Set<ValidationMessage> validationMessages;
            try {
                validationMessages = applicationManager.getBeanOfType(WorkflowDefinitionValidatorService.class)
                    .validate(workflowAsString);
            } catch (Exception exception) {
                validationMessages = Set.of(ValidationMessage.builder().message(exception.getMessage()).build());
            }
            onSubmittedEventBuilder.validationMessages(validationMessages);
            if (!validationMessages.isEmpty()) {
                applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(onSubmittedEventBuilder.build());
                return;
            }
            onSubmittedEventBuilder.workflowRun(submit(toObject(workflowAsString, WorkflowDefinitionScript.class), onSubmitEvent));
        } else if (workflowDefScriptObject instanceof WorkflowDefinitionScript workflowDefinitionScript) {
            onSubmittedEventBuilder.workflowRun(submit(workflowDefinitionScript, onSubmitEvent));
        }

        applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(onSubmittedEventBuilder.build());
    }

    @Override
    public Class<WorkflowDefinitionOnSubmitEvent> getEventType() {
        return WorkflowDefinitionOnSubmitEvent.class;
    }

    private WorkflowRun submit(WorkflowDefinitionScript workflowDefinitionScript,
                               WorkflowDefinitionOnSubmitEvent onSubmitEvent) {
        final var workflowRun = onSubmitEvent.getWorkflowRun();
        final var future = executorService.submit(() -> runSynchronously(workflowDefinitionScript, workflowRun, onSubmitEvent.getToken()));
        workflowRun.setFuture(future);
        return workflowRun;
    }

    private void runSynchronously(WorkflowDefinitionScript workflowDefinitionScript,
                                  WorkflowRun workflowRun,
                                  UUID token) {
        final var startTime = System.currentTimeMillis();
        applicationManager.getBeanOfType(EventBroadcaster.class)
            .broadcast(WorkflowDefinitionOnProgressEvent.of(token, workflowRun, false));
        try {
            workflowDefinitionScript.getCommands().stream()
                .takeWhile(command -> runCommandAndMarkAsFailedIfNeeded(command, workflowRun))
                .forEach(command -> applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowDefinitionOnProgressEvent.of(token, workflowRun, false)));
        } finally {
            workflowDefinitionScript.getFinallyCommands().stream()
                .takeWhile(command -> runCommandAndMarkAsFailedIfNeeded(command, workflowRun))
                .forEach(command -> applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowDefinitionOnProgressEvent.of(token, workflowRun, false)));
            workflowRun.markAsCompleted(System.currentTimeMillis() - startTime);
            applicationManager.getBeanOfType(EventBroadcaster.class)
                .broadcast(WorkflowDefinitionOnProgressEvent.of(token, workflowRun, true), 10);
        }
    }

    private boolean runCommandAndMarkAsFailedIfNeeded(AbstractCommand abstractCommand, WorkflowRun workflowRun) {
        try {
            resolveCommandPlaceholders(abstractCommand);
            abstractCommand.run(workflowRun);
            return true;
        } catch (Exception exception) {
            if (log.isDebugEnabled()) {
                log.debug("An exception was raised by command '{}' inside workflow run '{}'", abstractCommand.getName(), workflowRun.getId().toString(),
                    exception);
            }
            workflowRun.markAsFailed(new WorkflowRuntimeException("Command '" + abstractCommand.getName() + "' failed with exception", exception));
            return false;
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
            final var resolvedValueAsString = PlaceholderUtil.resolvePlaceholders(valueAsString,
                applicationManager.getBeanOfType(WorkflowPlaceholderService.class).getAllAsMap());
            log.debug("After resolving placeholders, '{}' was converted to '{}'", valueAsString, resolvedValueAsString);
            return resolvedValueAsString;
        } else if (value instanceof List<?> valueAsList) {
            return valueAsList.stream().map(this::resolvePlaceholders).toList();
        } else if (value instanceof Map<?, ?> valueAsMap) {
            return valueAsMap.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, entry -> resolvePlaceholders(entry.getValue())));
        }

        return value;
    }

}
