package org.myworkflows.view;

import com.flowingcode.vaadin.addons.granitealert.GraniteAlert;
import com.networknt.schema.ValidationMessage;
import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.DetachEvent;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.KeyModifier;
import com.vaadin.flow.component.ShortcutEventListener;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.details.Details;
import com.vaadin.flow.component.html.Hr;
import com.vaadin.flow.component.html.ListItem;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.html.UnorderedList;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.select.Select;
import com.vaadin.flow.component.splitlayout.SplitLayout;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.QueryParameters;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.router.RouteConfiguration;
import com.vaadin.flow.shared.Registration;
import de.f0rce.ace.AceEditor;
import de.f0rce.ace.enums.AceMode;
import jakarta.annotation.security.RolesAllowed;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmittedEvent;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.HasResizeableWidth;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowDevParamGrid;
import org.myworkflows.view.component.WorkflowPrintGrid;
import org.myworkflows.view.util.ClipboardUtil;
import org.myworkflows.view.util.EditorAutoCompleteUtil;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.IntStream;
import java.util.stream.Stream;

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

    private static final String READ_ONLY = "ro";

    private final AceEditor editor = new AceEditor();
    private final GraniteAlert currentWorkflowStatus = new GraniteAlert();
    private final WorkflowDevParamGrid workflowDevParamGrid = new WorkflowDevParamGrid();
    private final WorkflowPrintGrid workflowPrintGrid = new WorkflowPrintGrid();

    private final ApplicationManager applicationManager;
    private final SplitLayout splitLayout;
    private final Button shareButton;
    private final Select<WorkflowDefinition> filterByDefinition;

    private Registration onSubmittedRegistration;
    private Registration onProgressRegistration;
    private UUID lastSubmittedUuid;
    private final Button updateWorkflowButton = new Button(getTranslation("workflow-development.update.button"),
        new Icon(VaadinIcon.UPLOAD));

    public WorkflowDevelopmentView(ApplicationManager applicationManager) {
        this.applicationManager = applicationManager;

        UI.getCurrent().addShortcutListener((ShortcutEventListener) shortcutEvent -> editor.setWrap(!editor.isWrap()),
            Key.KEY_W, KeyModifier.CONTROL, KeyModifier.ALT);

        editor.setMode(AceMode.json);
        editor.setSofttabs(true);
        editor.addFocusShortcut(Key.KEY_E, KeyModifier.ALT);
        editor.setAutoComplete(true);
        editor.setLiveAutocompletion(true);
        EditorAutoCompleteUtil.apply(editor);
        currentWorkflowStatus.setCompact(true);
        currentWorkflowStatus.setVisible(false);

        filterByDefinition = createFilterByTemplate();
        splitLayout = createBody();
        shareButton = createShareButton();
        add(createHeader(getTranslation("workflow-development.page.title"), shareButton, filterByDefinition),
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
            .ifPresent(workflowDefinition -> {
                onFilteringByDefinition(workflowDefinition);
                processReadOnlyParamIfPresent(beforeEvent.getLocation().getQueryParameters());
            });
        processRemainingQueryParams(beforeEvent.getLocation().getQueryParameters());
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

    private Button createShareButton() {
        final var button = new Button(VaadinIcon.LINK.create());
        button.setTooltipText(getTranslation("workflow-development.share.button.tooltip"));
        button.setEnabled(false);
        button.addThemeVariants(ButtonVariant.LUMO_ICON);
        button.addClickListener(event -> {
            var url = ofNullable(filterByDefinition.getValue())
                .map(item -> RouteConfiguration.forSessionScope().getUrl(WorkflowDevelopmentView.class, item.getId()))
                .orElseGet(() -> RouteConfiguration.forSessionScope().getUrl(WorkflowDevelopmentView.class));
            final var queryString = new QueryParameters(workflowDevParamGrid.getParametersForQuery()).getQueryString();
            if (!queryString.isEmpty()) {
                url = url + "?" + queryString + "&" + READ_ONLY;
            } else {
                url = url + "?" + READ_ONLY;
            }
            ClipboardUtil.copyTo(getElement(), url);
        });
        return button;
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
        filterByDefinition.setValue(workflowDefinition);
        editor.setValue(toPrettyString(workflowDefinition.getScript(), StringUtils.EMPTY));
        updateWorkflowButton.setEnabled(true);
        shareButton.setEnabled(true);
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

        final var defDetails = new Details(getTranslation("workflow-development.def.label"), editor);
        defDetails.setOpened(true);
        defDetails.setWidthFull();

        final var runWorkflowButton = new Button(getTranslation("workflow-development.run.button"),
            new Icon(VaadinIcon.PLAY));
        runWorkflowButton.setIconAfterText(true);
        runWorkflowButton.addClickListener(event -> {
            lastSubmittedUuid = UUID.randomUUID();
            applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(WorkflowDefinitionOnSubmitEvent.builder()
                .token(lastSubmittedUuid)
                .workflowParameters(workflowDevParamGrid.getParametersAsMap())
                .workflowDefinitionScript(editor.getValue())
                .build());
        });
        runWorkflowButton.setWidthFull();

        updateWorkflowButton.setEnabled(false);
        updateWorkflowButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        updateWorkflowButton.setWidthFull();
        updateWorkflowButton.addClickListener(event -> ofNullable(filterByDefinition.getValue())
            .ifPresent(workflowDefinition -> applicationManager.getBeanOfType(WorkflowDefinitionService.class)
                .updateDefinition(workflowDefinition, editor.getValue())));

        layout.add(currentWorkflowStatus, defDetails, new Hr(), runWorkflowButton, updateWorkflowButton);
        return layout;
    }

    private Component createRight() {
        final var layout = new VerticalLayout();
        layout.setDefaultHorizontalComponentAlignment(Alignment.START);

        final var inputDetails = new Details(getTranslation("workflow-development.input.label"), workflowDevParamGrid);
        inputDetails.setOpened(true);
        inputDetails.setWidthFull();

        final var printDetails = new Details(getTranslation("workflow-development.print.label"), workflowPrintGrid);
        printDetails.setOpened(true);
        printDetails.setWidthFull();

        layout.add(inputDetails, printDetails);
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

        if (workflowRun.getDuration() > 0) {
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

    private void processReadOnlyParamIfPresent(QueryParameters queryParameters) {
        final var parameters = queryParameters.getParameters();
        if (parameters.isEmpty()) {
            return;
        }
        boolean readOnly = ofNullable(parameters.get(READ_ONLY)).isPresent();
        editor.setReadOnly(readOnly);
        updateWorkflowButton.setVisible(!readOnly);
        workflowDevParamGrid.setReadOnly(readOnly);
    }

    private void processRemainingQueryParams(QueryParameters queryParameters) {
        final var parameters = queryParameters.getParameters();
        if (parameters.isEmpty()) {
            return;
        }
        final var names = parameters.getOrDefault("name", List.of());
        workflowDevParamGrid.addParameters(IntStream.range(0, names.size()).boxed().flatMap(index -> {
            final var type = searchValueAtIndex(parameters.getOrDefault("type", List.of()), index, WorkflowParameterType.STR.getValue());
            final var value = searchValueAtIndex(parameters.getOrDefault("value", List.of()), index, StringUtils.EMPTY);
            final var workflowParameterType = ofNullable(WorkflowParameterType.of(type)).orElse(WorkflowParameterType.STR);
            return workflowParameterType.validate(value)
                .<Stream<WorkflowParameter>>map(error -> Stream.empty())
                .orElseGet(() -> Stream.of(WorkflowParameter.of(names.get(index), workflowParameterType, value)));
        }).toList());
    }

    private String searchValueAtIndex(List<String> values, int index, String defaultValue) {
        if (index >= 0 && index < values.size()) {
            return values.get(index);
        } else {
            return defaultValue;
        }
    }

}
