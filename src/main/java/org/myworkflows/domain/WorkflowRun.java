package org.myworkflows.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.myworkflows.converter.ExecutionCacheToByteArrayConverter;
import org.myworkflows.converter.SetOfStringToStringConverter;
import org.myworkflows.converter.UuidToByteArrayConverter;
import org.myworkflows.util.ExceptionUtil;
import org.springframework.data.domain.Persistable;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static java.lang.String.join;
import static org.myworkflows.util.LangUtil.pluralize;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Entity
@Table(name = "workflow_runs")
@AllArgsConstructor
@NoArgsConstructor
public class WorkflowRun implements CacheableEntry, Persistable<UUID> {

    @Id
    @Convert(converter = UuidToByteArrayConverter.class)
    private UUID id = UUID.randomUUID();

    @Getter
    @Column(name = "workflow_template_id")
    private Integer workflowTemplateId;

    @Getter
    @Column(name = "cache")
    @Convert(converter = ExecutionCacheToByteArrayConverter.class)
    private WorkflowRunCache cache = new WorkflowRunCache();

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
    private Instant created = Instant.now();

    @Override
    public UUID getId() {
        return id;
    }

    @Override
    public boolean isNew() {
        return true;
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

    public boolean isCompleted() {
        return duration > 0;
    }

}

