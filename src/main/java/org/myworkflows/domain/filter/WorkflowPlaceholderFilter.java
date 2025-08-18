package org.myworkflows.domain.filter;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowPlaceholder;

import java.util.function.Predicate;

import static org.apache.commons.lang3.StringUtils.containsIgnoreCase;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Getter
@Setter
@Accessors(fluent = true)
public final class WorkflowPlaceholderFilter implements Filter<WorkflowPlaceholder> {

    private String nameCriteria = StringUtils.EMPTY;

    @Override
    public Predicate<WorkflowPlaceholder> getFilterPredicate() {
        return isNotEmpty(nameCriteria)
            ? item -> containsIgnoreCase(item.getName(), nameCriteria)
            : item -> true;
    }

}
