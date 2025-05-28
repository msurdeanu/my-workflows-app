package org.myworkflows.domain.filter;

import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowTemplate;

import java.util.function.Predicate;

import static org.apache.commons.lang3.StringUtils.containsIgnoreCase;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
public final class WorkflowTemplateFilter implements Filter<WorkflowTemplate> {

    private int byIdCriteria;
    private String byNameCriteria = StringUtils.EMPTY;

    @Override
    public Predicate<WorkflowTemplate> getFilterPredicate() {
        final Predicate<WorkflowTemplate> predicate = byIdCriteria > 0
            ? item -> item.getId() == byIdCriteria
            : item -> true;
        return predicate.and(isNotEmpty(byNameCriteria)
            ? item -> containsIgnoreCase(item.getName(), byNameCriteria)
            : item -> true);
    }

    public WorkflowTemplateFilter setByIdCriteria(int byIdCriteria) {
        this.byIdCriteria = byIdCriteria;

        return this;
    }

    public WorkflowTemplateFilter setByNameCriteria(String byNameCriteria) {
        this.byNameCriteria = byNameCriteria;

        return this;
    }

}
