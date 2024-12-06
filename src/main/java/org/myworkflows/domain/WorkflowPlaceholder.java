package org.myworkflows.domain;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import org.apache.commons.lang3.StringUtils;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Entity
@Getter
@Table(name = "workflow_placeholders")
public class WorkflowPlaceholder implements CacheableEntry {

    @Id
    private String name;

    private String value;

    public static WorkflowPlaceholder of(String name) {
        return of(name, StringUtils.EMPTY);
    }

    public static WorkflowPlaceholder of(String name, String value) {
        final var workflowPlaceholder = new WorkflowPlaceholder();
        workflowPlaceholder.name = name;
        workflowPlaceholder.value = value;
        return workflowPlaceholder;
    }

    @Override
    public Object getCacheableKey() {
        return name;
    }

}
