package org.myworkflows.domain.filter;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowRun;

import java.util.UUID;
import java.util.function.Predicate;

import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.containsIgnoreCase;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Getter
@Setter
@Accessors(fluent = true)
public final class WorkflowRunFilter implements Filter<WorkflowRun> {

    private int templateIdCriteria;

    private String runIdCriteria = StringUtils.EMPTY;

    private Integer userIdCriteria;

    @Override
    public Predicate<WorkflowRun> getFilterPredicate() {
        final Predicate<WorkflowRun> predicate = templateIdCriteria > 0
            ? item -> templateIdCriteria == ofNullable(item.getWorkflowTemplateId()).orElse(0)
            : _ -> true;
        return predicate.and(isNotEmpty(runIdCriteria)
            ? item -> containsIgnoreCase(ofNullable(item.getId()).map(UUID::toString).orElse(StringUtils.EMPTY), runIdCriteria)
            : _ -> true)
            .and(userIdCriteria != null
                ? item -> userIdCriteria.equals(item.getUserId())
                : _ -> true);
    }

}
