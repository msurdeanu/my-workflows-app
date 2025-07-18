package org.myworkflows.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
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
public class WorkflowParameter {

    @Id
    @EqualsAndHashCode.Include
    private String name;

    @Convert(converter = ParameterTypeToStringConverter.class)
    private WorkflowParameterType type;

    @Column(name = "value")
    private String value;

    public static WorkflowParameter of(String name) {
        return of(name, WorkflowParameterType.STR, StringUtils.EMPTY);
    }

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

    public Object getComputedValue() {
        return type.getComputedValue(value);
    }

    public void setTypeAndValue(WorkflowParameter workflowParameter) {
        this.type = workflowParameter.type;
        this.value = workflowParameter.value;
    }
}
