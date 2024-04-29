package org.myworkflows.domain;

import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.converter.WorkflowDefinitionToStringConverter;

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
    @Convert(converter = WorkflowDefinitionToStringConverter.class)
    private WorkflowDefinition definition;

    @Getter
    @Setter
    @Transient
    private boolean editable = false;

    public boolean isEnabledForScheduling() {
        return enabled && !StringUtils.isEmpty(cron);
    }

    public void toggleOnEnabling() {
        enabled = !enabled;
    }

    public void toggleOnEditing() {
        editable = !editable;
    }

}
