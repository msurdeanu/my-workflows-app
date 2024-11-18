package org.myworkflows.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.myworkflows.converter.SetOfStringToStringConverter;
import org.myworkflows.converter.UuidToByteArrayConverter;
import org.myworkflows.converter.WorkflowRunCacheToByteArrayConverter;
import org.myworkflows.util.ExceptionUtil;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import static java.lang.String.join;
import static java.util.Optional.ofNullable;
import static org.myworkflows.util.LangUtil.pluralize;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Entity
@Table(name = "workflow_runs")
@AllArgsConstructor
@NoArgsConstructor
public class WorkflowRun implements CacheableEntry {

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
    private WorkflowRunCache cache = new WorkflowRunCache();

    @Getter
    @Column(name = "printed_keys")
    @Convert(converter = SetOfStringToStringConverter.class)
    private Set<String> printedKeys = new LinkedHashSet<>();

    @Getter
    @Column(name = "failure_message")
    private String failureMessage;

    private long duration = -1;

    @Getter
    private Instant created = Instant.now();

    @Setter
    @Transient
    private Future<?> future;

    public WorkflowRun(Integer workflowTemplateId) {
        this.workflowTemplateId = workflowTemplateId;
    }

    @Override
    public Object getCacheableKey() {
        return id;
    }

    public List<WorkflowRunPrint> getAllPrints() {
        return printedKeys.stream()
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
        pluralize("hr", remainingDuration.toHours(), true).ifPresent(parts::add);
        remainingDuration = remainingDuration.minusHours(remainingDuration.toHours());
        pluralize("min", remainingDuration.toMinutes(), true).ifPresent(parts::add);
        remainingDuration = remainingDuration.minusMinutes(remainingDuration.toMinutes());
        pluralize("sec", remainingDuration.toSeconds(), true).ifPresent(parts::add);

        return join(" and ", parts);
    }

    public void markAsFailed(Throwable throwable) {
        this.failureMessage = ExceptionUtil.getMessageAndCause(throwable);
    }

    public void markAsCompleted(long duration) {
        this.duration = duration;
    }

    public boolean isRunning() {
        return ofNullable(future).map(item -> !item.isDone() && !item.isCancelled()).orElse(false);
    }

    public boolean cancelAndInterruptIfRunning() {
        return isRunning() && future.cancel(true);
    }

}

