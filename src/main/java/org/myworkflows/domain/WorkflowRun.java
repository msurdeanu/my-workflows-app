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
import org.myworkflows.converter.SetOfStringToStringConverter;
import org.myworkflows.converter.UuidToByteArrayConverter;
import org.myworkflows.converter.WorkflowDefinitionScriptToStringConverter;
import org.myworkflows.converter.WorkflowRunCacheToByteArrayConverter;
import org.myworkflows.holder.file.BinaryFileSource;
import org.myworkflows.util.ExceptionUtil;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import static java.lang.String.join;
import static java.util.Optional.ofNullable;
import static org.myworkflows.holder.file.FileSourceHolder.INSTANCE;
import static org.myworkflows.util.LangUtil.pluralize;

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
    @Convert(converter = UuidToByteArrayConverter.class)
    private UUID id = UUID.randomUUID();

    @Getter
    @Column(name = "workflow_template_id")
    private Integer workflowTemplateId;

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

    @Getter
    @Convert(converter = WorkflowDefinitionScriptToStringConverter.class)
    private WorkflowDefinitionScript script;

    @Getter
    private Instant created = Instant.now();

    @Setter
    @Transient
    private Future<?> future;

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
        this.workflowTemplateId = workflowTemplateId;
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

        final var parts = new ArrayList<String>(3);
        var remainingDuration = Duration.ofMillis(duration);
        pluralize("hr", remainingDuration.toHours()).ifPresent(parts::add);
        remainingDuration = remainingDuration.minusHours(remainingDuration.toHours());
        pluralize("min", remainingDuration.toMinutes()).ifPresent(parts::add);
        remainingDuration = remainingDuration.minusMinutes(remainingDuration.toMinutes());
        pluralize("sec", remainingDuration.toSeconds()).ifPresent(parts::add);

        return join(" and ", parts);
    }

    public void markAsFailed(Throwable throwable, WorkflowDefinitionScript script) {
        this.failureMessage = ExceptionUtil.getMessageAndCause(throwable);
        this.script = script;
    }

    public void markAsCompleted(long duration) {
        this.duration = duration;
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

    public boolean isEligibleForReplay() {
        return failureMessage != null && script != null && cache.isCacheObjectMapComplete();
    }

    public void prepareForReplay() {
        failureMessage = null;
        duration = -1;
    }

    public void incrementLastSuccessfulIndex() {
        lastSuccessfulIndex++;
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

