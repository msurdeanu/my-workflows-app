package org.myworkflows.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import org.myworkflows.domain.WorkflowParameterType;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Converter
public final class ParameterTypeToStringConverter implements AttributeConverter<WorkflowParameterType, String> {

    @Override
    public String convertToDatabaseColumn(WorkflowParameterType attribute) {
        return ofNullable(attribute).map(WorkflowParameterType::getValue).orElse(null);
    }

    @Override
    public WorkflowParameterType convertToEntityAttribute(String data) {
        return ofNullable(data).map(WorkflowParameterType::of).orElse(null);
    }

}
