package org.myworkflows.domain;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.toList;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_DEFAULT)
public class ExpressionNameValue {

    @EqualsAndHashCode.Include
    @JsonProperty("name")
    private String name;

    @JsonProperty("value")
    private Object value;

    @JsonProperty("evaluator")
    private RuntimeEvaluator runtimeEvaluator = RuntimeEvaluator.PLAIN;

    public Object evaluate(final Map<String, Object> variables) {
        return recursiveEvaluation(value, variables);
    }

    @SuppressWarnings("unchecked")
    public Object recursiveEvaluation(final Object object, final Map<String, Object> variables) {
        if (object instanceof String) {
            return runtimeEvaluator.evaluate((String) object, variables);
        } else if (object instanceof List) {
            return ((List<?>) object).stream().map(item -> recursiveEvaluation(item, variables)).collect(toList());
        } else if (object instanceof Map) {
            final var copyOfInitialMap = new HashMap<>((Map<?, Object>) object);
            for (Map.Entry<?, Object> entry : copyOfInitialMap.entrySet()) {
                entry.setValue(recursiveEvaluation(entry.getValue(), variables));
            }
            return copyOfInitialMap;
        }

        return object;
    }

}
