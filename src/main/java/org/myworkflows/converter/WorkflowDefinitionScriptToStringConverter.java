package org.myworkflows.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.serializer.SerializerFactory;

import static java.util.Optional.ofNullable;
import static org.myworkflows.serializer.SerializerFactory.toObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Converter
public final class WorkflowDefinitionScriptToStringConverter implements AttributeConverter<WorkflowDefinitionScript, String> {

    @Override
    public String convertToDatabaseColumn(WorkflowDefinitionScript attribute) {
        return ofNullable(attribute)
            .map(item -> SerializerFactory.toString(attribute, null))
            .orElse(null);
    }

    @Override
    public WorkflowDefinitionScript convertToEntityAttribute(String data) {
        return ofNullable(data)
            .map(item -> toObject(item, WorkflowDefinitionScript.class))
            .orElse(null);
    }

}
