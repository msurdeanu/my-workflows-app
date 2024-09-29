package org.myworkflows.domain.filter;

import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowRun;

import java.util.UUID;
import java.util.function.Predicate;

import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
public final class WorkflowRunFilter implements Filter<WorkflowRun> {

    private String byIdCriteria = StringUtils.EMPTY;

    @Override
    public Predicate<WorkflowRun> getFilterPredicate() {
        return isNotEmpty(byIdCriteria)
            ? item -> ofNullable(item.getId()).map(UUID::toString).orElse(StringUtils.EMPTY).contains(byIdCriteria)
            : item -> true;
    }

    public WorkflowRunFilter setByIdCriteria(String byIdCriteria) {
        this.byIdCriteria = byIdCriteria;

        return this;
    }

}
