package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.ai.AiCall;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.holder.AiClientHolder;

import java.time.Duration;
import java.util.Map;
import java.util.Set;

/**
 * Sends a prompt to Claude and outputs its answer: the text of it, or - with a schema - what the answer says, held to
 * that schema: a {@code Map} for an object, whose fields {@code ifs}, {@code asserts} and {@code outputs} read. An
 * {@code enum} in the schema makes the command a classifier which the commands after it branch on.
 *
 * <p>Nothing is sent unless Claude is turned on, on the features page, and the server has an API key; the prompt and
 * the system text are masked first (see {@link AiClientHolder}).
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@NoArgsConstructor
public final class AiCommand extends AbstractCommand {

    public static final String PREFIX = "ai";

    static final String DEFAULT_MAX_TOKENS = "16000";

    public AiCommand(String name,
                     Set<ExpressionNameValue> ifs,
                     Set<ExpressionNameValue> inputs,
                     Set<ExpressionNameValue> asserts,
                     Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    /**
     * @param model the model to ask; the one of the settings when it is empty.
     * @param maxTokens the most the answer may take, thinking included. An answer cut at this limit fails the command.
     * @param effort {@code low}, {@code medium}, {@code high}, {@code xhigh} or {@code max}; the default of the model
     *     when it is empty.
     * @param schema a JSON schema, as a map, which the answer is held to. An empty one asks for a text.
     * @param timeout how long Claude may take, in milliseconds.
     * @return the text of the answer, or what it says, read from the JSON the schema asked for.
     */
    @ExecutionMethod(prefix = PREFIX)
    public Object ai(@ExecutionParam String prompt,
                     @ExecutionParam(required = false) String system,
                     @ExecutionParam(required = false) String model,
                     @ExecutionParam(required = false, defaultValue = DEFAULT_MAX_TOKENS) Number maxTokens,
                     @ExecutionParam(required = false) String effort,
                     @ExecutionParam(required = false, defaultValue = ":of") Map<String, Object> schema,
                     @ExecutionParam(required = false, defaultValue = "300000") Number timeout) {
        final var call = toCall(prompt, system, model, maxTokens, effort, schema)
            .timeout(Duration.ofMillis(timeout.longValue()))
            .build();
        return AiClientHolder.INSTANCE.send(call).toOutput(call.isStructured());
    }

    /**
     * @return a call with what the inputs of a command give: an empty text stands for a value which is not given.
     */
    static AiCall.AiCallBuilder toCall(String prompt, String system, String model, Number maxTokens, String effort, Map<String, Object> schema) {
        return AiCall.builder()
            .prompt(prompt)
            .system(StringUtils.trimToNull(system))
            .model(StringUtils.trimToNull(model))
            .maxTokens(maxTokens.longValue())
            .effort(StringUtils.trimToNull(effort))
            .schema(schema);
    }

}
