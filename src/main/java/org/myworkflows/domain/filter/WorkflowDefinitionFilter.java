package org.myworkflows.domain.filter;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowDefinition;

import java.util.function.Predicate;

import static org.apache.commons.lang3.StringUtils.containsIgnoreCase;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@Setter
@Accessors(fluent = true)
public final class WorkflowDefinitionFilter implements Filter<WorkflowDefinition> {

    private int idCriteria;

    private String nameCriteria = StringUtils.EMPTY;

    @Override
    public Predicate<WorkflowDefinition> getFilterPredicate() {
        final Predicate<WorkflowDefinition> predicate = idCriteria > 0
            ? item -> item.getId() == idCriteria
            : item -> true;
        return predicate.and(isNotEmpty(nameCriteria)
            ? item -> containsIgnoreCase(item.getName(), nameCriteria)
            : item -> true);
    }

}
