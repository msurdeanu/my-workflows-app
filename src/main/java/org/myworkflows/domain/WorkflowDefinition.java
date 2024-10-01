package org.myworkflows.domain;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.myworkflows.converter.WorkflowDefinitionScriptToStringConverter;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@Entity
@Table(name = "workflow_definitions")
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class WorkflowDefinition implements CacheableEntry {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @EqualsAndHashCode.Include
    private Integer id;

    @Setter
    private String name;

    @Getter
    @Setter
    @ManyToMany(cascade = {
        CascadeType.PERSIST,
        CascadeType.MERGE
    }, fetch = FetchType.EAGER)
    @JoinTable(name = "workflow_definitions_workflow_parameters",
        joinColumns = @JoinColumn(name = "workflow_definition_id"),
        inverseJoinColumns = @JoinColumn(name = "workflow_parameter_name")
    )
    private List<WorkflowParameter> workflowParameters;

    @Setter
    @Convert(converter = WorkflowDefinitionScriptToStringConverter.class)
    private WorkflowDefinitionScript script;

    @Getter
    @Setter
    @Transient
    private boolean editable = false;

    @Override
    public Object getCacheableKey() {
        return id;
    }

    public void toggleOnEditing() {
        editable = !editable;
    }

}
