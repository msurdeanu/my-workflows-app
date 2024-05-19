package org.myworkflows.domain.filter;

import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.ExecutionContext;

import java.util.function.Predicate;

import static org.apache.commons.lang3.StringUtils.containsIgnoreCase;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
public final class ExecutionContextFilter implements Filter<ExecutionContext> {

    private String byWorkflowIdCriteria = StringUtils.EMPTY;

    @Override
    public Predicate<ExecutionContext> getFilterPredicate() {
        return isNotEmpty(byWorkflowIdCriteria)
            ? item -> containsIgnoreCase(item.getWorkflowId().toString(), byWorkflowIdCriteria)
            : item -> true;
    }

    public ExecutionContextFilter setByWorkflowIdCriteria(String byWorkflowIdCriteria) {
        this.byWorkflowIdCriteria = byWorkflowIdCriteria;

        return this;
    }

}
