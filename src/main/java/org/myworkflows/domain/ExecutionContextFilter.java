package org.myworkflows.domain;

import lombok.Getter;
import org.apache.commons.lang3.StringUtils;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
public final class ExecutionContextFilter {

    private String byWorkflowIdCriteria = StringUtils.EMPTY;

    public ExecutionContextFilter setByWorkflowIdCriteria(String byWorkflowIdCriteria) {
        this.byWorkflowIdCriteria = byWorkflowIdCriteria;

        return this;
    }

}
