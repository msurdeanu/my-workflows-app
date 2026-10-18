package org.myworkflows.service;

import com.networknt.schema.Error;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunPause;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.domain.command.AbstractSubCommand;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmittedEvent;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.serializer.SerializerFactory;
import org.myworkflows.util.PlaceholderUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;

import static java.util.Optional.ofNullable;
import static org.myworkflows.serializer.SerializerFactory.toObject;
import static org.myworkflows.serializer.SerializerFactory.toObjectKeepingUnknownCommands;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
public final class WorkflowScriptService implements EventListener<WorkflowDefinitionOnSubmitEvent> {

    /**
     * How long a run stays paused before one of its commands, when nobody tells it how to go on: as long as an approval
     * waits by default.
     */
    private static final Duration PAUSE_TIMEOUT = Duration.ofHours(1);

    private final ExecutorService executorService;

    private final ApplicationManager applicationManager;

    private final Duration pauseTimeout;

    @Autowired
    public WorkflowScriptService(@Qualifier("workflow-pool") ExecutorService executorService,
                                 ApplicationManager applicationManager) {
        this(executorService, applicationManager, PAUSE_TIMEOUT);
    }

    WorkflowScriptService(ExecutorService executorService, ApplicationManager applicationManager, Duration pauseTimeout) {
        this.executorService = executorService;
        this.applicationManager = applicationManager;
        this.pauseTimeout = pauseTimeout;
    }

    @Override
    public void onEventReceived(WorkflowDefinitionOnSubmitEvent onSubmitEvent) {
        final var workflowDefScriptObject = onSubmitEvent.workflowDefinitionScript();

        // The workflow run is always attached, so that subscribers can correlate the outcome - including a
        // validation failure - with the run they submitted.
        final var onSubmittedEventBuilder = WorkflowDefinitionOnSubmittedEvent.builder()
            .workflowRun(onSubmitEvent.workflowRun())
            .validationMessages(List.of());

        if (workflowDefScriptObject instanceof String workflowAsString) {
            final var validationMessages = validate(workflowAsString);
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

    /**
     * @return what keeps the given script, as somebody typed it, from being run. It is empty for a script which can be
     *     read as a {@link WorkflowDefinitionScript}.
     */
    public List<Error> validate(String workflowAsString) {
        try {
            return applicationManager.getBeanOfType(WorkflowDefinitionValidatorService.class).validate(workflowAsString);
        } catch (Exception exception) {
            return List.of(Error.builder().message(exception.getMessage()).build());
        }
    }

    private WorkflowRun submit(WorkflowDefinitionScript workflowDefinitionScript,
                               WorkflowDefinitionOnSubmitEvent onSubmitEvent) {
        final var workflowRun = onSubmitEvent.workflowRun();
        // a command can have something to tell before it is over, like an approval which starts to wait for its answer
        workflowRun.setChangeListener(() -> applicationManager.getBeanOfType(EventBroadcaster.class)
            .broadcast(WorkflowDefinitionOnProgressEvent.of(workflowRun, false)));
        workflowRun.setReportWriter(applicationManager.getBeanOfType(WorkflowReportService.class));
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
        // a command of a type which is not known is copied as well, so that it is the run which fails, with the reason
        return toObjectKeepingUnknownCommands(scriptAsString, WorkflowDefinitionScript.class);
    }

    private void runSynchronously(WorkflowDefinitionScript workflowDefinitionScript, WorkflowRun workflowRun) {
        final var execution = new Execution(workflowDefinitionScript);
        broadcastProgress(workflowRun);

        try {
            runCommands(execution, workflowRun);
        } finally {
            runFinallyCommands(execution.getScript(), workflowRun);
            // what the run ends with is kept for whoever debugs it, like what it held each time it paused
            ofNullable(workflowRun.getDebugger()).ifPresent(debugger -> debugger.snapshot(workflowRun.getCache()));
            workflowRun.markAsCompleted(execution.getDuration());
            applicationManager.getBeanOfType(EventBroadcaster.class)
                .broadcast(WorkflowDefinitionOnProgressEvent.of(workflowRun, true), 10);
        }
    }

    /**
     * Runs the commands one after another, from the one which follows the last command that passed: the first one, or
     * the one a replay starts again from. Where the run stands is read again before every command, as a paused run can
     * be told to go on from another command.
     */
    private void runCommands(Execution execution, WorkflowRun workflowRun) {
        while (pauseIfAsked(execution, workflowRun)) {
            final var commands = execution.getScript().getCommands();
            final var index = workflowRun.getLastSuccessfulIndex() + 1;
            if (index >= commands.size() || !runCommandAndMarkAsFailedIfNeeded(commands.get(index), workflowRun, execution.getScript(), false)) {
                return;
            }
            broadcastProgress(workflowRun);
        }
    }

    private void runFinallyCommands(WorkflowDefinitionScript script, WorkflowRun workflowRun) {
        final var finallyCommands = script.getFinallyCommands();
        if (workflowRun.getFailureMessage() != null && !finallyCommands.isEmpty()) {
            // a failed command is followed by no progress of its own, and the final commands may take a while
            broadcastProgress(workflowRun);
        }
        finallyCommands.stream()
            .takeWhile(command -> runCommandAndMarkAsFailedIfNeeded(command, workflowRun, script, true))
            .forEach(_ -> broadcastProgress(workflowRun));
    }

    /**
     * Keeps the run before its next command when whoever debugs it asked for a pause there, until they tell it how to
     * go on: with another script, from another command or with other breakpoints, if they say so.
     *
     * @return whether the run goes on. It does not when it was stopped or cancelled while it was paused, or when nobody
     *     told it anything in time.
     */
    private boolean pauseIfAsked(Execution execution, WorkflowRun workflowRun) {
        final var debugger = workflowRun.getDebugger();
        final var commands = execution.getScript().getCommands();
        final var index = workflowRun.getLastSuccessfulIndex() + 1;
        if (debugger == null || index >= commands.size() || !debugger.pausesBefore(index)) {
            return true;
        }

        final var commandName = commands.get(index).getName();
        final var pausedAt = System.currentTimeMillis();
        try {
            final var resume = workflowRun.awaitResume(index, commandName, pauseTimeout);
            if (resume.action() == WorkflowRunPause.Action.STOP) {
                workflowRun.markAsFailed(new WorkflowRuntimeException(describeStop(resume, commandName)), execution.getScript());
                return false;
            }
            ofNullable(resume.script()).ifPresent(execution::setScript);
            workflowRun.moveTo(resume.commandIndex());
            debugger.goOn(resume.commandIndex(), resume.action() == WorkflowRunPause.Action.STEP, resume.breakpoints());
            return true;
        } catch (InterruptedException exception) {
            // the interruption ends here, like the one of a command: the final commands run as after any failure
            workflowRun.markAsFailed(new WorkflowRuntimeException("The run was cancelled while it was paused before command '" + commandName + "'."),
                execution.getScript());
            return false;
        } finally {
            execution.addPausedTime(System.currentTimeMillis() - pausedAt);
        }
    }

    private String describeStop(WorkflowRunPause.Resume resume, String commandName) {
        return resume.isTimedOut()
            ? "The run was paused before command '" + commandName + "' and nobody told it how to go on before the pause timed out."
            : "The run was stopped by '" + resume.username() + "' while it was paused before command '" + commandName + "'.";
    }

    private void broadcastProgress(WorkflowRun workflowRun) {
        applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(WorkflowDefinitionOnProgressEvent.of(workflowRun, false));
    }

    private boolean runCommandAndMarkAsFailedIfNeeded(AbstractCommand abstractCommand, WorkflowRun workflowRun, WorkflowDefinitionScript script,
                                                      boolean finalCommand) {
        try {
            resolveCommandPlaceholders(abstractCommand);
            abstractCommand.run(workflowRun);
            if (finalCommand) {
                workflowRun.incrementLastSuccessfulFinallyIndex();
            } else {
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

    /**
     * A run on its way: the script it runs, which whoever debugs it can replace while it is paused, and the time it spent
     * paused, which is no part of how long it took.
     *
     * @author Mihai Surdeanu
     * @since 2.1
     */
    private static final class Execution {

        private final long startTime = System.currentTimeMillis();

        @Getter
        @Setter
        private WorkflowDefinitionScript script;

        private long pausedTime;

        private Execution(WorkflowDefinitionScript script) {
            this.script = script;
        }

        private void addPausedTime(long millis) {
            pausedTime += millis;
        }

        private long getDuration() {
            return System.currentTimeMillis() - startTime - pausedTime;
        }

    }

}
