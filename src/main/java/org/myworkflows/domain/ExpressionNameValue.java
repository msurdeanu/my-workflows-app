package org.myworkflows.domain;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import static java.util.stream.Collectors.toList;

/**
 * @author Mihai Surdeanu
 * @since 1.0
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

    @JsonProperty("class")
    private RuntimeEvaluator runtimeEvaluator = RuntimeEvaluator.PLAIN;

    public Object evaluate(Map<String, Object> variables) {
        return recursiveEvaluation(value, variables);
    }

    private Object recursiveEvaluation(Object object, Map<String, Object> variables) {
        return switch (object) {
            case String objectAsStr -> runtimeEvaluator.evaluate(objectAsStr, variables, CACHE_ACCESS_PATTERN);
            case List<?> objectAsList -> objectAsList.stream().map(item -> recursiveEvaluation(item, variables)).collect(toList());
            case Map<?, ?> objectAsMap -> evaluateMap(objectAsMap, variables);
            default -> object;
        };
    }

    private Map<Object, Object> evaluateMap(Map<?, ?> objectAsMap, Map<String, Object> variables) {
        // Collectors.toMap rejects null values, so an expression evaluating to null would blow up here.
        // Insertion order is kept as well, which Collectors.toMap does not guarantee.
        final var evaluatedMap = new LinkedHashMap<>();
        objectAsMap.forEach((key, value) -> evaluatedMap.put(key, recursiveEvaluation(value, variables)));
        return evaluatedMap;
    }

}
