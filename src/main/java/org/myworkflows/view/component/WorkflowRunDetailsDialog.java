package org.myworkflows.view.component;

import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.router.QueryParameters;
import com.vaadin.flow.router.RouterLink;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.view.WorkflowReportView;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public final class WorkflowRunDetailsDialog extends ResponsiveDialog {

    public WorkflowRunDetailsDialog(WorkflowRun workflowRun, long reportCount) {
        super("workflow-run-details");

        if (reportCount > 0) {
            add(createReportsLink(workflowRun, reportCount));
        }
        add(new WorkflowPrintGrid(workflowRun.getAllPrints()));
    }

    /**
     * The reports the run wrote are on a page of their own, which lists them filtered by the run.
     */
    private Div createReportsLink(WorkflowRun workflowRun, long reportCount) {
        final var routerLink = new RouterLink(getTranslation("workflow-runs.details.reports.link", reportCount), WorkflowReportView.class);
        routerLink.setQueryParameters(QueryParameters.of("run", workflowRun.getId().toString()));
        // the dialog belongs to the page it was opened from, which the link leaves
        routerLink.getElement().addEventListener("click", _ -> close());
        final var layout = new Div(VaadinIcon.NEWSPAPER.create(), routerLink);
        layout.addClassName("workflow-run-reports");
        return layout;
    }

}
