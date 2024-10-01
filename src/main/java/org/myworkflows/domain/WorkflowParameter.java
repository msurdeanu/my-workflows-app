package org.myworkflows.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;
import org.myworkflows.converter.ParameterTypeToStringConverter;

import java.util.Optional;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Entity
@Getter
@Setter
@Table(name = "workflow_parameters")
public class WorkflowParameter implements CacheableEntry {

    @Id
    private String name;

    @Convert(converter = ParameterTypeToStringConverter.class)
    private WorkflowParameterType type;

    @Column(name = "value")
    private String value;

    @Override
    public Object getCacheableKey() {
        return name;
    }

    public static Optional<String> validateTypeAndValue(WorkflowParameterType type, String value) {
        return type.validate(value);
    }

    public Object getComputedValue() {
        return type.getComputedValue(value);
    }

}
