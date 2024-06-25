package org.myworkflows.view;

import com.flowingcode.vaadin.addons.granitealert.GraniteAlert;
import com.networknt.schema.ValidationMessage;
import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.DetachEvent;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.details.Details;
import com.vaadin.flow.component.html.Hr;
import com.vaadin.flow.component.html.ListItem;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.html.UnorderedList;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.select.Select;
import com.vaadin.flow.component.splitlayout.SplitLayout;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.shared.Registration;
import de.f0rce.ace.AceEditor;
import de.f0rce.ace.enums.AceMode;
import jakarta.annotation.security.RolesAllowed;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmittedEvent;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.HasResizeableWidth;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowPrintGrid;

import java.util.Set;
import java.util.UUID;

import static java.lang.String.valueOf;
import static java.util.Optional.ofNullable;
import static org.myworkflows.serializer.JsonFactory.toPrettyString;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@RolesAllowed("ROLE_ADMIN")
@Route(value = WorkflowDevelopmentView.ROUTE, layout = BaseLayout.class)
public class WorkflowDevelopmentView extends ResponsiveLayout implements HasResizeableWidth, HasDynamicTitle, HasUrlParameter<Integer> {

    public static final String ROUTE = "workflow/dev";

    private final AceEditor editor = new AceEditor();
    private final GraniteAlert currentWorkflowStatus = new GraniteAlert();
    private final WorkflowPrintGrid workflowPrintGrid = new WorkflowPrintGrid();

    private final ApplicationManager applicationManager;
    private final SplitLayout splitLayout;
    private final Select<WorkflowDefinition> filterByTemplate;

    private Registration onSubmittedRegistration;
    private Registration onProgressRegistration;
    private UUID lastSubmittedUuid;
    private final Button updateWorkflowButton = new Button(getTranslation("workflow-development.update.button"),
        new Icon(VaadinIcon.UPLOAD));

    public WorkflowDevelopmentView(ApplicationManager applicationManager) {
        this.applicationManager = applicationManager;

        editor.setMode(AceMode.json);
        editor.setSofttabs(true);
        currentWorkflowStatus.setCompact(true);
        currentWorkflowStatus.setVisible(false);

        splitLayout = createBody();
        filterByTemplate = createFilterByTemplate();
        add(createHeader(getTranslation("workflow-development.page.title"), filterByTemplate),
            createContent(splitLayout),
            createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-development"));
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter Integer workflowDefinitionId) {
        ofNullable(workflowDefinitionId)
            .flatMap(item -> applicationManager.getBeanOfType(WorkflowDefinitionService.class)
                .getAll(new WorkflowDefinitionFilter().setByIdCriteria(item), 0, 1)
                .findFirst())
            .ifPresent(this::onFilteringByDefinition);
    }

    @Override
    public void onSmallWidth() {
        splitLayout.setOrientation(SplitLayout.Orientation.VERTICAL);
    }

    @Override
    public void onBigWidth() {
        splitLayout.setOrientation(SplitLayout.Orientation.HORIZONTAL);
    }

    @Override
    protected void onAttach(AttachEvent attachEvent) {
        super.onAttach(attachEvent);
        final var ui = attachEvent.getUI();
        onSubmittedRegistration = applicationManager.getBeanOfType(EventBroadcaster.class).register(event -> {
            final var workflowResultEvent = (WorkflowDefinitionOnSubmittedEvent) event;
            if (workflowResultEvent.getToken().equals(lastSubmittedUuid)
                && !workflowResultEvent.getValidationMessages().isEmpty()) {
                ui.access(() -> updateWorkflowProgress(workflowResultEvent.getValidationMessages()));
            }
        }, WorkflowDefinitionOnSubmittedEvent.class);
        onProgressRegistration = applicationManager.getBeanOfType(EventBroadcaster.class).register(event -> {
            final var workflowResultEvent = (WorkflowDefinitionOnProgressEvent) event;
            if (workflowResultEvent.getToken().equals(lastSubmittedUuid)) {
                ui.access(() -> {
                    updateWorkflowProgress(workflowResultEvent.getWorkflowRun());
                    workflowPrintGrid.setItems(workflowResultEvent.getWorkflowRun().getAllPrints());
                });
            }
        }, WorkflowDefinitionOnProgressEvent.class);
    }

    @Override
    protected void onDetach(DetachEvent detachEvent) {
        onProgressRegistration.remove();
        onSubmittedRegistration.remove();
        super.onDetach(detachEvent);
    }

    private Select<WorkflowDefinition> createFilterByTemplate() {
        final var filterByTemplateSelect = new Select<WorkflowDefinition>();
        filterByTemplateSelect.setItems(applicationManager.getBeanOfType(WorkflowDefinitionService.class)
            .getAll().toList());
        filterByTemplateSelect.setPlaceholder(getTranslation("workflow-development.filter.by-template.placeholder"));
        filterByTemplateSelect.setHelperText(getTranslation("workflow-development.filter.by-template.helper"));
        filterByTemplateSelect.setItemLabelGenerator(WorkflowDefinition::getName);
        filterByTemplateSelect.addValueChangeListener(event -> onFilteringByDefinition(event.getValue()));
        return filterByTemplateSelect;
    }

    private void onFilteringByDefinition(WorkflowDefinition workflowDefinition) {
        filterByTemplate.setValue(workflowDefinition);
        editor.setValue(toPrettyString(workflowDefinition.getScript(), ""));
        updateWorkflowButton.setEnabled(true);
    }

    private SplitLayout createBody() {
        final var layout = new SplitLayout(createLeft(), createRight());
        layout.setSplitterPosition(50);
        layout.setWidthFull();
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
                .workflowDefinitionScript(editor.getValue())
                .build());
        });
        runWorkflowButton.setWidthFull();

        final var horizontalLayout = new HorizontalLayout();
        horizontalLayout.setSpacing(true);
        updateWorkflowButton.setEnabled(false);
        updateWorkflowButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        updateWorkflowButton.getStyle().set("flex", "1 1 50%");
        // TODO : call updateDefinition from WorkflowDefinitionService
        final var createWorkflowButton = new Button(getTranslation("workflow-development.create.button"),
            new Icon(VaadinIcon.PLUS_CIRCLE));
        createWorkflowButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        createWorkflowButton.getStyle().set("flex", "1 1 50%");
        // TODO : call create from WorkflowDefinitionService
        horizontalLayout.add(updateWorkflowButton, createWorkflowButton);
        horizontalLayout.setWidth("100%");

        layout.add(inputDetails, new Hr(), runWorkflowButton, horizontalLayout);
        return layout;
    }

    private Component createRight() {
        final var layout = new VerticalLayout();
        layout.setDefaultHorizontalComponentAlignment(Alignment.START);

        final var outputDetails = new Details(getTranslation("workflow-development.output.label"), workflowPrintGrid);
        outputDetails.setOpened(true);
        outputDetails.setWidthFull();

        layout.add(currentWorkflowStatus, outputDetails);
        return layout;
    }

    private void updateWorkflowProgress(Set<ValidationMessage> validationMessages) {
        currentWorkflowStatus.removeAll();

        currentWorkflowStatus.setLevel(GraniteAlert.GraniteAlertLevel.ERROR);
        currentWorkflowStatus.add(new Span(getTranslation("workflow-development.validation.message")));
        final var listItems = validationMessages.stream()
            .map(ValidationMessage::getMessage)
            .map(ListItem::new)
            .toList();
        currentWorkflowStatus.add(new UnorderedList(listItems.toArray(listItems.toArray(new ListItem[0]))));

        currentWorkflowStatus.setVisible(true);
    }

    private void updateWorkflowProgress(WorkflowRun workflowRun) {
        currentWorkflowStatus.removeAll();

        if (workflowRun.isCompleted()) {
            ofNullable(workflowRun.getFailureMessage()).ifPresentOrElse(error -> {
                currentWorkflowStatus.setLevel(GraniteAlert.GraniteAlertLevel.ERROR);
                currentWorkflowStatus.add(new Span(getTranslation("workflow-development.error.message",
                    valueOf(workflowRun.getId()), workflowRun.getHumanReadableDuration(),
                    workflowRun.getFailureMessage())));
            }, () -> {
                currentWorkflowStatus.setLevel(GraniteAlert.GraniteAlertLevel.SUCCESS);
                currentWorkflowStatus.add(new Span(getTranslation("workflow-development.success.message",
                    valueOf(workflowRun.getId()), workflowRun.getHumanReadableDuration())));
            });
        } else {
            currentWorkflowStatus.setLevel(GraniteAlert.GraniteAlertLevel.INFO);
            currentWorkflowStatus.add(new Span(getTranslation("workflow-development.in-progress.message",
                valueOf(workflowRun.getId()))));
        }

        currentWorkflowStatus.setVisible(true);
    }

}
