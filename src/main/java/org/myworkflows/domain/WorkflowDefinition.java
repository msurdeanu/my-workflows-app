package org.myworkflows.domain;

import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.myworkflows.converter.WorkflowDefinitionScriptToStringConverter;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@Entity
@Table(name = "workflow_definitions")
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class WorkflowDefinition {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @EqualsAndHashCode.Include
    private Integer id;

    @Setter
    private String name;

    @Setter
    @Convert(converter = WorkflowDefinitionScriptToStringConverter.class)
    private WorkflowDefinitionScript script;

    @Getter
    @Setter
    @Transient
    private boolean editable = false;

    public static WorkflowDefinition of(String name) {
        return of(name, WorkflowDefinitionScript.empty());
    }

    public static WorkflowDefinition of(String name, WorkflowDefinitionScript script) {
        final var workflowDefinition = new WorkflowDefinition();
        workflowDefinition.name = name;
        workflowDefinition.script = script;
        return workflowDefinition;
    }

    public void toggleOnEditing() {
        editable = !editable;
    }

    public void setNameAndScript(WorkflowDefinition workflowDefinition) {
        this.name = workflowDefinition.getName();
        this.script = workflowDefinition.getScript();
    }

}
