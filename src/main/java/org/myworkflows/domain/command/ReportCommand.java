package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowReportFormat;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;

import java.util.Set;

/**
 * Writes a Markdown or HTML text as a report of the run, which the reports page lists and shows.
 *
 * @author Mihai Surdeanu
 * @since 2.3
 */
@NoArgsConstructor
public final class ReportCommand extends AbstractCommand {

    public static final String PREFIX = "report";

    public ReportCommand(String name,
                         Set<ExpressionNameValue> ifs,
                         Set<ExpressionNameValue> inputs,
                         Set<ExpressionNameValue> asserts,
                         Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    /**
     * @return the id of the report, {@code <run id>/<file name>}, by which it can be found on the reports page:
     *     {@code workflow/reports/<id>}.
     */
    @ExecutionMethod(prefix = PREFIX)
    public String report(@ExecutionParam(bypassed = true) WorkflowRun workflowRun,
                         @ExecutionParam String title,
                         @ExecutionParam String content,
                         @ExecutionParam(required = false, defaultValue = "markdown") String format) {
        final WorkflowReport workflowReport = workflowRun.writeReport(title, WorkflowReportFormat.of(format), content);
        return workflowReport.id();
    }

}
