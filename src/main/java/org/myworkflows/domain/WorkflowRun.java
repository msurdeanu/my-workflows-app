package org.myworkflows.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.PostRemove;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;
import org.myworkflows.converter.SetOfStringToStringConverter;
import org.myworkflows.converter.WorkflowDefinitionScriptToStringConverter;
import org.myworkflows.converter.WorkflowRunCacheToByteArrayConverter;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.file.BinaryFileSource;
import org.myworkflows.util.ExceptionUtil;
import org.myworkflows.util.LangUtil;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import static java.util.Optional.ofNullable;
import static org.myworkflows.holder.file.FileSourceHolder.INSTANCE;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@Entity
@Table(name = "workflow_runs")
@AllArgsConstructor
public class WorkflowRun {

    @Id
    @Getter
    @JdbcTypeCode(SqlTypes.BINARY)
    private UUID id = UUID.randomUUID();

    @Getter
    @Column(name = "workflow_template_id")
    private Integer workflowTemplateId;

    @Getter
    @Column(name = "user_id")
    private Integer userId;

    @Getter
    @Column(name = "cache")
    @Convert(converter = WorkflowRunCacheToByteArrayConverter.class)
    private WorkflowRunCache cache;

    @Getter
    @Column(name = "printed_keys")
    @Convert(converter = SetOfStringToStringConverter.class)
    private Set<String> printedKeys = new LinkedHashSet<>();

    @Getter
    @Column(name = "failure_message")
    private String failureMessage;

    @Getter
    private long duration = -1;

    @Getter
    @Column(name = "last_successful_index")
    private int lastSuccessfulIndex = -1;

    /**
     * How far the final commands got. It is not saved, because a replay runs every final command again.
     */
    @Getter
    @Transient
    private int lastSuccessfulFinallyIndex = -1;

    @Getter
    @Convert(converter = WorkflowDefinitionScriptToStringConverter.class)
    private WorkflowDefinitionScript script;

    @Getter
    private Instant created = Instant.now();

    @Setter
    @Transient
    private Future<?> future;

    /**
     * What the run waits for while it stands at an approval command. It is not saved, as a run in progress lives in
     * the cache only.
     */
    @Getter
    @Transient
    private volatile WorkflowRunApproval approval;

    /**
     * What the run waits for while it is paused before one of its commands. It is not saved either.
     */
    @Getter
    @Transient
    private volatile WorkflowRunPause pause;

    /**
     * Who says where the run pauses, for a run started from the development page. Any other run has none.
     */
    @Getter
    @Setter
    @Transient
    private volatile WorkflowRunDebugger debugger;

    /**
     * Told when the run changes while one of its commands is still running, which no progress of a command announces.
     */
    @Setter
    @Transient
    private Runnable changeListener = () -> {
    };

    public WorkflowRun() {
        this(null, null);
    }

    public WorkflowRun(Integer workflowTemplateId) {
        this(workflowTemplateId, null);
    }

    public WorkflowRun(Map<String, Object> parameters) {
        this(null, parameters);
    }

    public WorkflowRun(Integer workflowTemplateId, Map<String, Object> parameters) {
        this(workflowTemplateId, parameters, null);
    }

    public WorkflowRun(Integer workflowTemplateId, Map<String, Object> parameters, Integer userId) {
        this.workflowTemplateId = workflowTemplateId;
        this.userId = userId;
        this.cache = new WorkflowRunCache(id);
        ofNullable(parameters).orElse(Map.of()).forEach(cache::put);
    }

    public List<WorkflowRunPrint> getAllPrints() {
        final var keysSet = isDebugModeEnabled() ? cache.getAllKeys() : printedKeys;
        return keysSet.stream()
            .flatMap(item -> cache.find(item).map(value -> new WorkflowRunPrint(item, value)).stream())
            .collect(Collectors.toList());
    }

    public boolean markKeyAsPrinted(String key) {
        return cache.find(key)
            .map(value -> printedKeys.add(key))
            .orElse(false);
    }

    public String getHumanReadableDuration() {
        if (duration < 0) {
            return "N/A";
        } else if (duration < 1_000) {
            return duration + " ms";
        }

        return LangUtil.describe(Duration.ofMillis(duration));
    }

    public void markAsFailed(Throwable throwable, WorkflowDefinitionScript script) {
        this.failureMessage = ExceptionUtil.getMessageAndCause(throwable);
        this.script = script;
    }

    public void markAsCompleted(long duration) {
        this.duration = duration;
    }

    /**
     * @return whether the run is over. A run which was just told to start again is not, even before it does.
     */
    public boolean isCompleted() {
        return duration >= 0;
    }

    public boolean isDebugModeEnabled() {
        return cache.find("debug", Boolean.class).orElse(false);
    }

    public boolean isRunning() {
        return ofNullable(future).map(item -> !item.isDone() && !item.isCancelled()).orElse(false);
    }

    public boolean cancelAndInterruptIfRunning() {
        return isRunning() && future.cancel(true);
    }

    public boolean isWaitingForApproval() {
        return approval != null;
    }

    /**
     * Keeps the run where it is until a user approves it. Whoever follows the run is told when it starts to wait and
     * when it stops, as nothing else about the run changes in between.
     *
     * @return the name of the user who approved.
     * @throws WorkflowRuntimeException when the run is rejected, or when nobody decides within the given time.
     * @throws InterruptedException when the run is cancelled while it waits.
     */
    public String awaitApproval(String commandName, String message, Duration timeout) throws InterruptedException {
        final var pendingApproval = new WorkflowRunApproval(commandName, message, Instant.now().plus(timeout));
        approval = pendingApproval;
        changeListener.run();
        try {
            return pendingApproval.await();
        } finally {
            approval = null;
            changeListener.run();
        }
    }

    public boolean isPaused() {
        return pause != null;
    }

    /**
     * Keeps the run before the given command until somebody tells it how to go on. Whoever follows the run is told when
     * it starts to wait and when it stops, as for an approval, and finds what the run holds in its debugger.
     *
     * @return what the run was told, which is to stop when nobody answered in the given time.
     * @throws InterruptedException when the run is cancelled while it is paused.
     */
    public WorkflowRunPause.Resume awaitResume(int commandIndex, String commandName, Duration timeout) throws InterruptedException {
        final var pendingPause = new WorkflowRunPause(commandIndex, commandName, Instant.now().plus(timeout));
        ofNullable(debugger).ifPresent(item -> item.snapshot(cache));
        pause = pendingPause;
        changeListener.run();
        try {
            return pendingPause.await();
        } finally {
            pause = null;
            changeListener.run();
        }
    }

    /**
     * Makes the given command the next one to run: the commands before it are taken as passed, whether they ran or not.
     */
    public void moveTo(int commandIndex) {
        lastSuccessfulIndex = Math.max(commandIndex, 0) - 1;
    }

    public boolean isEligibleForReplay() {
        return failureMessage != null && script != null && cache.isCacheObjectMapComplete();
    }

    /**
     * A run can be replayed only by the user who triggered it, so a run without a user - one scheduled by its
     * template - cannot be replayed at all.
     */
    public boolean isReplayableBy(Integer currentUserId) {
        return userId != null && userId.equals(currentUserId) && isEligibleForReplay();
    }

    public void prepareForReplay() {
        failureMessage = null;
        duration = -1;
        lastSuccessfulFinallyIndex = -1;
    }

    public void incrementLastSuccessfulIndex() {
        lastSuccessfulIndex++;
    }

    public void incrementLastSuccessfulFinallyIndex() {
        lastSuccessfulFinallyIndex++;
    }

    @PostRemove
    public void deleteCacheFile() {
        try {
            INSTANCE.deleteIfExists(BinaryFileSource.of(cache.getId().toString()));
        } catch (IOException exception) {
            log.warn("Error deleting workflow run cache file", exception);
        }
    }
}

