package org.myworkflows.domain;

import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;
import org.myworkflows.converter.WorkflowDefinitionToStringConverter;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@Entity
@Table(name = "workflow_definitions")
public class WorkflowDefinition {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    private String name;

    @Setter
    @Convert(converter = WorkflowDefinitionToStringConverter.class)
    private WorkflowDefinitionScript script;

}
