package org.myworkflows.domain;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import static java.util.stream.Collectors.toList;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_DEFAULT)
public class ExpressionNameValue {

    public static final Pattern CACHE_ACCESS_PATTERN = Pattern.compile("\\$\\(([a-zA-Z0-9_.]+)(:[a-zA-Z0-9_.]+)?\\)");

    @EqualsAndHashCode.Include
    @JsonProperty("name")
    @Setter
    private String name;

    @Setter
    @JsonProperty("value")
    private Object value;

    @JsonProperty("@type")
    private RuntimeEvaluator runtimeEvaluator = RuntimeEvaluator.PLAIN;

    public Object evaluate(Map<String, Object> variables) {
        return recursiveEvaluation(value, variables);
    }

    @SuppressWarnings("unchecked")
    private Object recursiveEvaluation(Object object, Map<String, Object> variables) {
        if (object instanceof String objectAsStr) {
            return runtimeEvaluator.evaluate(objectAsStr, variables, CACHE_ACCESS_PATTERN);
        } else if (object instanceof List<?> objectAsList) {
            return objectAsList.stream().map(item -> recursiveEvaluation(item, variables)).collect(toList());
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
