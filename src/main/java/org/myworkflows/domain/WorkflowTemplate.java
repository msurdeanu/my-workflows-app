package org.myworkflows.domain;

import jakarta.persistence.CascadeType;
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
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Entity
@Table(name = "workflow_templates")
public class WorkflowTemplate {

    @Id
    @Getter
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Getter
    private boolean enabled;

    @Getter
    @Setter
    private String name;

    @Getter
    @Setter
    private String cron;

    @Getter
    @Setter
    @ManyToMany(cascade = {
        CascadeType.PERSIST,
        CascadeType.MERGE
    }, fetch = FetchType.EAGER)
    @JoinTable(name = "workflow_templates_workflow_definitions",
        joinColumns = @JoinColumn(name = "workflow_template_id"),
        inverseJoinColumns = @JoinColumn(name = "workflow_definition_id")
    )
    private List<WorkflowDefinition> workflowDefinitions;

    @Getter
    @Setter
    @Transient
    private boolean editable = false;

    public boolean isEnabledForScheduling() {
        return enabled && !StringUtils.isEmpty(cron);
    }

    public Map<String, Object> getWorkflowDefinitionParameters() {
        return ofNullable(workflowDefinitions)
            .orElse(List.of())
            .stream()
            .flatMap(item -> item.getParameters().stream())
            .collect(Collectors.toMap(Parameter::getName, Parameter::getComputedValue, (it1, it2) -> it2));
    }

    public List<WorkflowDefinitionScript> getWorkflowDefinitionScripts() {
        return ofNullable(workflowDefinitions)
            .orElse(List.of())
            .stream()
            .map(WorkflowDefinition::getScript)
            .collect(Collectors.toList());
    }

    public void toggleOnEnabling() {
        enabled = !enabled;
    }

    public void toggleOnEditing() {
        editable = !editable;
    }

}
