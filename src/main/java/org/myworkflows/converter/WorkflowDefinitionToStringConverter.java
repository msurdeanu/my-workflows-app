package org.myworkflows.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.serializer.JsonFactory;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Converter
public final class WorkflowDefinitionToStringConverter implements AttributeConverter<WorkflowDefinition, String> {

    @Override
    public String convertToDatabaseColumn(final WorkflowDefinition attribute) {
        return ofNullable(attribute)
                .map(item -> JsonFactory.toString(attribute, null))
                .orElse(null);
    }

    @Override
    public WorkflowDefinition convertToEntityAttribute(final String data) {
        return ofNullable(data)
                .map(item -> JsonFactory.fromJsonToObject(item, WorkflowDefinition.class))
                .orElse(null);
    }

}
