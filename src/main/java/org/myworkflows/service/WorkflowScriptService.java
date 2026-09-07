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
import org.myworkflows.serializer.SerializerFactory;
import org.myworkflows.util.PlaceholderUtil;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

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
        final var workflowDefScriptObject = onSubmitEvent.workflowDefinitionScript();

        // The workflow run is always attached, so that subscribers can correlate the outcome - including a
        // validation failure - with the run they submitted.
        final var onSubmittedEventBuilder = WorkflowDefinitionOnSubmittedEvent.builder()
            .workflowRun(onSubmitEvent.workflowRun())
            .validationMessages(Set.of());

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
        final var workflowRun = onSubmitEvent.workflowRun();
        final var runnableScript = detach(workflowDefinitionScript);
        final var future = executorService.submit(() -> runSynchronously(runnableScript, workflowRun));
        workflowRun.setFuture(future);
        return workflowRun;
    }

    /**
     * Returns a private copy of the script, because running it resolves placeholders by mutating the
     * expressions in place. A template or definition script comes straight out of the internal cache and
     * is shared by every run, so mutating it would freeze the placeholder values at their first-run state
     * and would race between concurrent runs of the same workflow.
     */
    private WorkflowDefinitionScript detach(WorkflowDefinitionScript workflowDefinitionScript) {
        final var scriptAsString = SerializerFactory.toString(workflowDefinitionScript, null);
        if (scriptAsString == null) {
            log.warn("Workflow definition script could not be copied, so the shared instance is used instead.");
            return workflowDefinitionScript;
        }
        return toObject(scriptAsString, WorkflowDefinitionScript.class);
    }

    private void runSynchronously(WorkflowDefinitionScript workflowDefinitionScript, WorkflowRun workflowRun) {
        final var startTime = System.currentTimeMillis();
        applicationManager.getBeanOfType(EventBroadcaster.class)
            .broadcast(WorkflowDefinitionOnProgressEvent.of(workflowRun, false));

        try {
            final var commands = workflowDefinitionScript.getCommands();
            IntStream.range(0, commands.size())
                .takeWhile(index -> runCommandAndMarkAsFailedIfNeeded(index, commands.get(index), workflowRun, workflowDefinitionScript))
                .forEach(index -> applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowDefinitionOnProgressEvent.of(workflowRun, false)));
        } finally {
            workflowDefinitionScript.getFinallyCommands().stream()
                .takeWhile(command -> runCommandAndMarkAsFailedIfNeeded(Integer.MAX_VALUE, command, workflowRun, workflowDefinitionScript))
                .forEach(command -> applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowDefinitionOnProgressEvent.of(workflowRun, false)));
            workflowRun.markAsCompleted(System.currentTimeMillis() - startTime);
            applicationManager.getBeanOfType(EventBroadcaster.class)
                .broadcast(WorkflowDefinitionOnProgressEvent.of(workflowRun, true), 10);
        }
    }

    private boolean runCommandAndMarkAsFailedIfNeeded(int commandIndex, AbstractCommand abstractCommand,
                                                      WorkflowRun workflowRun, WorkflowDefinitionScript script) {
        if (commandIndex <= workflowRun.getLastSuccessfulIndex()) {
            return true;
        }

        try {
            resolveCommandPlaceholders(abstractCommand);
            abstractCommand.run(workflowRun);
            if (commandIndex < Integer.MAX_VALUE) {
                workflowRun.incrementLastSuccessfulIndex();
            }
            return true;
        } catch (Exception exception) {
            if (log.isDebugEnabled()) {
                log.debug("An exception was raised by command '{}' inside workflow run '{}'", abstractCommand.getName(), workflowRun.getId().toString(),
                    exception);
            }
            workflowRun.markAsFailed(new WorkflowRuntimeException("Command '" + abstractCommand.getName() + "' failed with exception", exception), script);
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
