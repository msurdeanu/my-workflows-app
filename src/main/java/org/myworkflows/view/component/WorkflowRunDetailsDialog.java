package org.myworkflows.view.component;

import com.flowingcode.vaadin.addons.granitealert.GraniteAlert;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import org.myworkflows.domain.WorkflowRun;

import java.util.Optional;

import static java.lang.String.valueOf;
import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowRunDetailsDialog extends ResponsiveDialog {

    public WorkflowRunDetailsDialog(WorkflowRun workflowRun) {
        super("workflow-run-details");

        add(createBody(workflowRun));
    }

    private Component createBody(WorkflowRun workflowRun) {
        final var layout = new VerticalLayout();
        createFailureAlert(workflowRun).ifPresent(layout::add);
        layout.add(createPrintGrid(workflowRun));
        return layout;
    }

    private Optional<Component> createFailureAlert(WorkflowRun workflowRun) {
        return ofNullable(workflowRun.getFailureMessage())
            .map(failureMessage -> {
                final var alert = new GraniteAlert();
                alert.setLevel(GraniteAlert.GraniteAlertLevel.ERROR);
                alert.add(new Span(failureMessage));
                alert.add(new Span(getTranslation("workflow-development.error.message",
                    valueOf(workflowRun.getId()), workflowRun.getHumanReadableDuration(),
                    workflowRun.getFailureMessage())));
                return alert;
            });
    }

    private Component createPrintGrid(WorkflowRun workflowRun) {
        return new WorkflowPrintGrid(workflowRun.getAllPrints());
    }

}
