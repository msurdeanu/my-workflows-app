package org.myworkflows.domain.filter;

import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowParameter;

import java.util.function.Predicate;

import static org.apache.commons.lang3.StringUtils.containsIgnoreCase;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
public final class WorkflowParameterFilter implements Filter<WorkflowParameter> {

    private String byNameCriteria = StringUtils.EMPTY;

    @Override
    public Predicate<WorkflowParameter> getFilterPredicate() {
        return isNotEmpty(byNameCriteria)
                ? item -> containsIgnoreCase(item.getName(), byNameCriteria)
                : item -> true;
    }

    public WorkflowParameterFilter setByNameCriteria(String byNameCriteria) {
        this.byNameCriteria = byNameCriteria;

        return this;
    }

}
