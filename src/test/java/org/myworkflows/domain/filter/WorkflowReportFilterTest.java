package org.myworkflows.domain.filter;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowReportFormat;

import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Mihai Surdeanu
 * @since 2.3
 */
public final class WorkflowReportFilterTest {

    private final UUID firstRunId = UUID.randomUUID();
    private final UUID secondRunId = UUID.randomUUID();

    private final WorkflowReport weeklySales = new WorkflowReport(firstRunId, "Weekly sales.md", WorkflowReportFormat.MARKDOWN, 1, null, 1, null);
    private final WorkflowReport healthCheck = new WorkflowReport(firstRunId, "Health check.html", WorkflowReportFormat.HTML, 1, null, 1, null);
    private final WorkflowReport audit = new WorkflowReport(secondRunId, "Audit of sales.html", WorkflowReportFormat.HTML, null, null, 1, null);

    @Test
    public void whenNoCriteriaIsSetThenEveryReportMatches() {
        assertEquals(List.of(weeklySales, healthCheck, audit), filter(new WorkflowReportFilter()));
    }

    @Test
    public void whenCriteriaAreSetThenOnlyTheReportsMatchingAllOfThemAreLeft() {
        assertEquals(List.of(weeklySales, audit), filter(new WorkflowReportFilter().titleCriteria("SALES")));
        assertEquals(List.of(healthCheck, audit), filter(new WorkflowReportFilter().formatCriteria(WorkflowReportFormat.HTML)));
        assertEquals(List.of(weeklySales, healthCheck), filter(new WorkflowReportFilter().templateIdCriteria(1)));
        assertEquals(List.of(audit), filter(new WorkflowReportFilter().runIdCriteria(secondRunId.toString().substring(0, 8))));
        assertEquals(List.of(audit), filter(new WorkflowReportFilter().titleCriteria("sales").formatCriteria(WorkflowReportFormat.HTML)));
    }

    @Test
    public void whenTitleIsSearchedThenTheExtensionIsNotPartOfIt() {
        assertEquals(List.of(), filter(new WorkflowReportFilter().titleCriteria("html")));
    }

    private List<WorkflowReport> filter(WorkflowReportFilter workflowReportFilter) {
        return Stream.of(weeklySales, healthCheck, audit).filter(workflowReportFilter.getFilterPredicate()).toList();
    }

}
