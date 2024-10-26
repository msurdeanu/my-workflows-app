package org.myworkflows.domain.filter;

import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowRun;

import java.util.UUID;
import java.util.function.Predicate;

import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.containsIgnoreCase;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
public final class WorkflowRunFilter implements Filter<WorkflowRun> {

    private int byTemplateIdCriteria;
    private String byRunIdCriteria = StringUtils.EMPTY;

    @Override
    public Predicate<WorkflowRun> getFilterPredicate() {
        final Predicate<WorkflowRun> predicate = byTemplateIdCriteria > 0
            ? item -> byTemplateIdCriteria == ofNullable(item.getWorkflowTemplateId()).orElse(0)
            : item -> true;
        return predicate.and(isNotEmpty(byRunIdCriteria)
            ? item -> containsIgnoreCase(ofNullable(item.getId()).map(UUID::toString).orElse(StringUtils.EMPTY), byRunIdCriteria)
            : item -> true);
    }

    public WorkflowRunFilter setByTemplateIdCriteria(int byTemplateIdCriteria) {
        this.byTemplateIdCriteria = byTemplateIdCriteria;

        return this;
    }

    public WorkflowRunFilter setByRunIdCriteria(String byRunIdCriteria) {
        this.byRunIdCriteria = byRunIdCriteria;

        return this;
    }

}
