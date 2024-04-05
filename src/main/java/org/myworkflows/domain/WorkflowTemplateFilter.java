package org.myworkflows.domain;

import lombok.Getter;
import org.apache.commons.lang3.StringUtils;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
public final class WorkflowTemplateFilter {

    private String byNameCriteria = StringUtils.EMPTY;

    public WorkflowTemplateFilter setByNameCriteria(final String byNameCriteria) {
        this.byNameCriteria = byNameCriteria;

        return this;
    }

}
