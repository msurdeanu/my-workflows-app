package org.myworkflows.domain.filter;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowReportFormat;

import java.util.UUID;
import java.util.function.Predicate;

import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.containsIgnoreCase;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 2.3
 */
@Getter
@Setter
@Accessors(fluent = true)
public final class WorkflowReportFilter implements Filter<WorkflowReport> {

    private String titleCriteria = StringUtils.EMPTY;

    private WorkflowReportFormat formatCriteria;

    private int templateIdCriteria;

    /**
     * A part of the id of the run, like the eight characters the runs page shows.
     */
    private String runIdCriteria = StringUtils.EMPTY;

    @Override
    public Predicate<WorkflowReport> getFilterPredicate() {
        final Predicate<WorkflowReport> predicate = isNotEmpty(titleCriteria)
            ? item -> containsIgnoreCase(item.title(), titleCriteria)
            : _ -> true;
        return predicate
            .and(formatCriteria != null
                ? item -> formatCriteria == item.format()
                : _ -> true)
            .and(templateIdCriteria > 0
                ? item -> templateIdCriteria == ofNullable(item.workflowTemplateId()).orElse(0)
                : _ -> true)
            .and(isNotEmpty(runIdCriteria)
                ? item -> containsIgnoreCase(ofNullable(item.workflowRunId()).map(UUID::toString).orElse(StringUtils.EMPTY), runIdCriteria)
                : _ -> true);
    }

}
