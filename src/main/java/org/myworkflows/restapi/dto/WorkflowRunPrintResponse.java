package org.myworkflows.restapi.dto;

import lombok.Builder;
import org.myworkflows.domain.WorkflowRunPrint;
import org.myworkflows.view.transformer.WorkflowPrintValue.Node;
import org.myworkflows.view.transformer.WorkflowRunPrintToValueTransformer;

import java.math.BigDecimal;
import java.util.LinkedHashMap;

import static org.myworkflows.domain.WorkflowRunPrint.NULL_AS_STR;

/**
 * A printed value, read the way the runs page reads it - so what is masked there is masked here - and written as JSON:
 * a map and a list as an object and an array, a result set as an array of rows, each an object keyed by its columns,
 * and anything else as its text. A text which holds JSON stays a text, as it was printed.
 *
 * @param type the simple name of the class of the value, in lower case, as the runs page shows it.
 * @param value as many rows of a result set as the runs page reads, which are the first 1000 at most.
 * @author Mihai Surdeanu
 * @since 2.2
 */
@Builder
public record WorkflowRunPrintResponse(String name, String type, Object value) {

    private static final WorkflowRunPrintToValueTransformer TRANSFORMER = new WorkflowRunPrintToValueTransformer();

    public static WorkflowRunPrintResponse of(WorkflowRunPrint workflowRunPrint) {
        final var printValue = TRANSFORMER.transform(workflowRunPrint);
        return WorkflowRunPrintResponse.builder()
            .name(workflowRunPrint.name())
            .type(workflowRunPrint.type())
            .value(switch (printValue.source()) {
                case STRUCTURE, TABLE -> toValue(printValue.root());
                case JSON, TEXT -> printValue.text();
            })
            .build();
    }

    private static Object toValue(Node node) {
        return switch (node.type()) {
            case MAP -> {
                final var map = new LinkedHashMap<String, Object>();
                node.children().forEach(child -> map.put(child.key(), toValue(child)));
                yield map;
            }
            case LIST -> node.children().stream().map(WorkflowRunPrintResponse::toValue).toList();
            case NUMBER -> toNumber(node.text());
            case LITERAL -> NULL_AS_STR.equals(node.text()) ? null : Boolean.valueOf(node.text());
            case STRING, MASKED, OTHER -> node.text();
        };
    }

    /**
     * A number is read from its text, which is what its class makes of it: a class of somebody else may not write one.
     */
    private static Object toNumber(String text) {
        try {
            return new BigDecimal(text);
        } catch (NumberFormatException notNumber) {
            return text;
        }
    }

}
