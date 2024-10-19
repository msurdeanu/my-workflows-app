package org.myworkflows.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.EqualsAndHashCode;
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
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class WorkflowParameter implements CacheableEntry {

    @Id
    @EqualsAndHashCode.Include
    private String name;

    @Convert(converter = ParameterTypeToStringConverter.class)
    private WorkflowParameterType type;

    @Column(name = "value")
    private String value;

    public static WorkflowParameter of(String name, WorkflowParameterType type, String value) {
        final var workflowParameter = new WorkflowParameter();
        workflowParameter.name = name;
        workflowParameter.type = type;
        workflowParameter.value = value;
        return workflowParameter;
    }

    public static Optional<String> validateTypeAndValue(WorkflowParameterType type, String value) {
        return type.validate(value);
    }

    @Override
    public Object getCacheableKey() {
        return name;
    }

    public Object getComputedValue() {
        return type.getComputedValue(value);
    }

}
