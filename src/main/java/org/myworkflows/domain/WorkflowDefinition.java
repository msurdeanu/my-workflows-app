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
import org.myworkflows.converter.WorkflowDefinitionToStringConverter;

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

    @Setter
    @Convert(converter = WorkflowDefinitionToStringConverter.class)
    private WorkflowDefinitionScript script;

    @Getter
    @Setter
    @Transient
    private boolean editable = false;

    @Override
    public Object key() {
        return id;
    }

    public void toggleOnEditing() {
        editable = !editable;
    }

}
