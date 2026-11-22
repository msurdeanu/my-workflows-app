package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.ai.AiBatchResult;
import org.myworkflows.domain.ai.AiCall;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.AiClientHolder;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.IntStream;

/**
 * Sends many prompts to Claude at once, through the Message Batches API: half the price of as many {@code ai}
 * commands, but the answers come when Anthropic is done with all of them - often within minutes, at most within a day -
 * and the run waits for them. What fits a nightly loop over hundreds of items better than a {@code loop} of single
 * calls. Every prompt is asked with the same system text, model, limit, effort and schema, and the answers come out in
 * the order of the prompts.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@NoArgsConstructor
public final class AiBatchCommand extends AbstractCommand {

    public static final String PREFIX = "aiBatch";

    /**
     * How many failures the message of a failed command names, at most.
     */
    static final int MAX_FAILURES_TOLD = 5;

    public AiBatchCommand(String name,
                          Set<ExpressionNameValue> ifs,
                          Set<ExpressionNameValue> inputs,
                          Set<ExpressionNameValue> asserts,
                          Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    /**
     * @param prompts the prompts, a list of texts; anything else in it is turned into its text.
     * @param timeout how long the whole batch may take, in milliseconds. Past it, the batch is cancelled.
     * @param failOnError whether a prompt which got no answer fails the command; when it does not, its answer is
     *     {@code null}.
     * @return the answers, in the order of the prompts: texts, or what they say when a schema is given.
     */
    @ExecutionMethod(prefix = PREFIX)
    public List<Object> aiBatch(@ExecutionParam List<Object> prompts,
                                @ExecutionParam(required = false) String system,
                                @ExecutionParam(required = false) String model,
                                @ExecutionParam(required = false, defaultValue = AiCommand.DEFAULT_MAX_TOKENS) Number maxTokens,
                                @ExecutionParam(required = false) String effort,
                                @ExecutionParam(required = false, defaultValue = ":of") Map<String, Object> schema,
                                @ExecutionParam(required = false, defaultValue = "86400000") Number timeout,
                                @ExecutionParam(required = false, defaultValue = "true") Boolean failOnError) {
        if (prompts.stream().anyMatch(Objects::isNull)) {
            throw new WorkflowRuntimeException("Every prompt of 'aiBatch.prompts' needs a text: prompt "
                + (prompts.indexOf(null) + 1) + " has none.");
        }
        final List<AiCall> calls = prompts.stream()
            .map(prompt -> AiCommand.toCall(String.valueOf(prompt), system, model, maxTokens, effort, schema).build())
            .toList();
        final var results = AiClientHolder.INSTANCE.sendBatch(calls, Duration.ofMillis(timeout.longValue()));
        final var structured = !calls.isEmpty() && calls.getFirst().isStructured();
        final var answers = new ArrayList<>(results.size());
        final var failures = new ArrayList<String>();
        IntStream.range(0, results.size()).forEach(index -> {
            final var answer = toOutput(results.get(index), structured);
            answers.add(answer.output());
            if (answer.failure() != null) {
                failures.add("prompt " + (index + 1) + ": " + answer.failure());
            }
        });
        if (Boolean.TRUE.equals(failOnError) && !failures.isEmpty()) {
            throw new WorkflowRuntimeException(failures.size() + " of " + results.size() + " prompts got no answer - "
                + String.join("; ", failures.subList(0, Math.min(MAX_FAILURES_TOLD, failures.size())))
                + (failures.size() > MAX_FAILURES_TOLD ? "; ..." : ""));
        }
        return answers;
    }

    private static Answer toOutput(AiBatchResult result, boolean structured) {
        if (!result.isAnswered()) {
            return new Answer(null, result.failure());
        }
        try {
            return new Answer(result.answer().toOutput(structured), null);
        } catch (RuntimeException exception) {
            return new Answer(null, exception.getMessage());
        }
    }

    private record Answer(Object output, String failure) {
    }

}
