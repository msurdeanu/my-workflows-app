package org.myworkflows.view.component;

import com.flowingcode.vaadin.addons.granitealert.GraniteAlert;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import org.myworkflows.domain.ExecutionContext;

import java.util.Optional;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowRunDetailsDialog extends ResponsiveDialog {

    public WorkflowRunDetailsDialog(ExecutionContext executionContext) {
        super("workflow-run-details");

        add(createBody(executionContext));
    }

    private Component createBody(ExecutionContext executionContext) {
        final var layout = new VerticalLayout();
        createFailureAlert(executionContext).ifPresent(layout::add);
        layout.add(createPrintGrid(executionContext));
        return layout;
    }

    private Optional<Component> createFailureAlert(ExecutionContext executionContext) {
        return ofNullable(executionContext.getFailureMessage())
            .map(failureMessage -> {
                final var alert = new GraniteAlert();
                alert.setLevel(GraniteAlert.GraniteAlertLevel.ERROR);
                alert.add(new Span(failureMessage));
                alert.add(new Span(getTranslation("workflow-development.error.message",
                    executionContext.getWorkflowId().toString(), executionContext.getHumanReadableDuration(),
                    executionContext.getFailureMessage())));
                return alert;
            });
    }

    private Component createPrintGrid(ExecutionContext executionContext) {
        return new WorkflowPrintGrid(executionContext.getAllPrints());
    }

}
