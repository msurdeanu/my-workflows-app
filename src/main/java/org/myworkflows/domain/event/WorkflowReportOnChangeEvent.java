package org.myworkflows.domain.event;

import org.myworkflows.domain.WorkflowReport;

import java.util.List;

/**
 * Published once reports were written or deleted, and the cache says so: whoever lists them from there, like the
 * reports page, finds them as they are now.
 *
 * @param workflowReports the reports which were written, or the ones which were deleted, never empty.
 * @author Mihai Surdeanu
 * @since 2.3
 */
public record WorkflowReportOnChangeEvent(List<WorkflowReport> workflowReports, boolean deleted) implements Event {

}
