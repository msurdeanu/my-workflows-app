package org.myworkflows.view;

import com.flowingcode.vaadin.addons.granitealert.GraniteAlert;
import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.DetachEvent;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.details.Details;
import com.vaadin.flow.component.html.Hr;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.splitlayout.SplitLayout;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.shared.Registration;
import de.f0rce.ace.AceEditor;
import de.f0rce.ace.enums.AceMode;
import jakarta.annotation.security.PermitAll;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.ExecutionContext;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.layout.BaseLayout;
import org.myworkflows.layout.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowPrintGrid;

import java.util.UUID;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@PermitAll
@Route(value = WorkflowDefinitionDevView.ROUTE, layout = BaseLayout.class)
public class WorkflowDefinitionDevView extends ResponsiveLayout implements HasDynamicTitle {

    public static final String ROUTE = "workflow/definition/dev";

    private final AceEditor editor = new AceEditor();
    private final GraniteAlert currentWorkflowStatus = new GraniteAlert();
    private final WorkflowPrintGrid workflowPrintGrid = new WorkflowPrintGrid();

    private final ApplicationManager applicationManager;

    private Registration workflowRegistration;
    private UUID lastSubmittedUuid;

    public WorkflowDefinitionDevView(final ApplicationManager applicationManager) {
        this.applicationManager = applicationManager;

        editor.setMode(AceMode.json);
        editor.setSofttabs(true);
        currentWorkflowStatus.setCompact(true);
        currentWorkflowStatus.setVisible(false);

        add(createHeader(getTranslation("workflow-development.page.title")),
                createContent(createBody()),
                createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-development"));
    }

    @Override
    protected void onAttach(final AttachEvent attachEvent) {
        final var ui = attachEvent.getUI();
        workflowRegistration = applicationManager.getBeanOfType(EventBroadcaster.class).register(event -> {
            final var workflowResultEvent = (WorkflowDefinitionOnProgressEvent) event;
            if (workflowResultEvent.getToken().equals(lastSubmittedUuid)) {
                ui.access(() -> {
                    updateWorkflowProgress(workflowResultEvent.getExecutionContext());
                    workflowPrintGrid.setItems(workflowResultEvent.getExecutionContext().getAllPrints());
                });
            }
        }, WorkflowDefinitionOnProgressEvent.class);
    }

    @Override
    protected void onDetach(final DetachEvent detachEvent) {
        workflowRegistration.remove();
        workflowRegistration = null;
    }

    private Component createBody() {
        final var layout = new VerticalLayout(currentWorkflowStatus);
        final var splitLayout = new SplitLayout(createLeft(), createRight());
        splitLayout.setSplitterPosition(50);
        splitLayout.setWidthFull();
        layout.add(splitLayout);
        return layout;
    }

    private Component createLeft() {
        final var layout = new VerticalLayout();
        layout.setDefaultHorizontalComponentAlignment(Alignment.START);

        final var inputDetails = new Details(getTranslation("workflow-development.input.label"), editor);
        inputDetails.setOpened(true);
        inputDetails.setWidthFull();

        final var runWorkflowButton = new Button(getTranslation("workflow-development.run.button"),
                new Icon(VaadinIcon.PLAY));
        runWorkflowButton.setIconAfterText(true);
        runWorkflowButton.addClickListener(event -> {
            lastSubmittedUuid = UUID.randomUUID();
            applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(WorkflowDefinitionOnSubmitEvent.builder()
                    .isManual(true)
                    .token(lastSubmittedUuid)
                    .workflow(editor.getValue())
                    .build());
        });
        runWorkflowButton.setWidthFull();

        layout.add(inputDetails, new Hr(), runWorkflowButton);
        return layout;
    }

    private Component createRight() {
        final var layout = new VerticalLayout();
        layout.setDefaultHorizontalComponentAlignment(Alignment.START);

        final var outputDetails = new Details(getTranslation("workflow-development.output.label"), workflowPrintGrid);
        outputDetails.setOpened(true);
        outputDetails.setWidthFull();

        layout.add(outputDetails);
        return layout;
    }

    private void updateWorkflowProgress(final ExecutionContext executionContext) {
        currentWorkflowStatus.removeAll();

        if (executionContext.isRunCompleted()) {
            ofNullable(executionContext.getFailureMessage()).ifPresentOrElse(error -> {
                currentWorkflowStatus.setLevel(GraniteAlert.GraniteAlertLevel.ERROR);
                currentWorkflowStatus.add(new Span(getTranslation("workflow-development.error.message",
                        executionContext.getWorkflowId().toString(), executionContext.getHumanReadableDuration(),
                        executionContext.getFailureMessage())));
            }, () -> {
                currentWorkflowStatus.setLevel(GraniteAlert.GraniteAlertLevel.SUCCESS);
                currentWorkflowStatus.add(new Span(getTranslation("workflow-development.success.message",
                        executionContext.getWorkflowId().toString(), executionContext.getHumanReadableDuration())));
            });
        } else {
            currentWorkflowStatus.setLevel(GraniteAlert.GraniteAlertLevel.INFO);
            currentWorkflowStatus.add(new Span(getTranslation("workflow-development.in-progress.message",
                    executionContext.getWorkflowId().toString())));
        }

        currentWorkflowStatus.setVisible(true);
    }

}
