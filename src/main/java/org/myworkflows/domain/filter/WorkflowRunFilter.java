package org.myworkflows.domain.filter;

import lombok.Getter;
import org.myworkflows.domain.WorkflowRun;

import java.util.function.Predicate;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
public final class WorkflowRunFilter implements Filter<WorkflowRun> {

    private Integer byWorkflowIdCriteria = 0;

    @Override
    public Predicate<WorkflowRun> getFilterPredicate() {
        return byWorkflowIdCriteria > 0
            ? item -> byWorkflowIdCriteria.equals(item.getId())
            : item -> true;
    }

    public WorkflowRunFilter setByWorkflowIdCriteria(Integer byWorkflowIdCriteria) {
        this.byWorkflowIdCriteria = byWorkflowIdCriteria;

        return this;
    }

}
