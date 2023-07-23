package org.myworkflows.domain;

import lombok.Getter;
import org.jooq.lambda.tuple.Tuple2;
import org.myworkflows.util.ExceptionUtil;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static java.util.stream.Collectors.toMap;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ExecutionContext {

    @Getter
    private final ExecutionCache cache = new ExecutionCache();

    private final Set<String> printedKeys = new HashSet<>();

    private String failureMessage;

    private long duration;

    public boolean markKeyAsPrinted(final String key) {
        return cache.find(key)
                .map(value -> printedKeys.add(key))
                .orElse(false);
    }

    public Map<String, Object> getAllPrints() {
        return printedKeys.stream()
                .flatMap(key -> cache.find(key).map(value -> new Tuple2<>(key, value)).stream())
                .collect(toMap(Tuple2::v1, Tuple2::v2));
    }

    public void markCommandAsFailed(final Throwable throwable) {
        this.failureMessage = ExceptionUtil.getMessageAndCause(throwable);
    }

    public void markAsCompleted(final long duration) {
        this.duration = duration;
    }

}
