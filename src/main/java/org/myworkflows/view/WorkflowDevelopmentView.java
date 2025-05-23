package org.myworkflows.view;

import com.networknt.schema.ValidationMessage;
import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.DetachEvent;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.KeyModifier;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.details.Details;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Hr;
import com.vaadin.flow.component.html.ListItem;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.html.UnorderedList;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
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
import com.vaadin.flow.theme.lumo.LumoUtility;
import de.f0rce.ace.AceEditor;
import de.f0rce.ace.enums.AceMode;
import jakarta.annotation.security.PermitAll;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.config.BaseConfig;
import org.myworkflows.domain.UserRole;
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
import org.myworkflows.view.util.RequestUtil;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.vaadin.flow.component.Shortcuts.addShortcutListener;
import static java.lang.String.valueOf;
import static java.util.Optional.ofNullable;
import static org.myworkflows.serializer.JsonFactory.toPrettyString;
import static org.myworkflows.util.ListUtil.getValueAtIndex;
import static org.myworkflows.util.Base64Util.base64Decode;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@PermitAll
@Route(value = WorkflowDevelopmentView.ROUTE, layout = BaseLayout.class)
public class WorkflowDevelopmentView extends ResponsiveLayout implements HasResizeableWidth, HasDynamicTitle, HasUrlParameter<Integer> {

    public static final String ROUTE = "workflow/dev";
    private static final String READ_ONLY = "ro";

    private final AceEditor editor = new AceEditor();
    private final Div currentWorkflowStatus = new Div();
    private final WorkflowDevParamGrid workflowDevParamGrid = new WorkflowDevParamGrid();
    private final WorkflowPrintGrid workflowPrintGrid = new WorkflowPrintGrid();
    private final boolean isLoggedAsAdmin = UserRole.ADMIN.validate();
    private final Button runWorkflowButton = new Button(getTranslation("workflow-development.run.button"),
        new Icon(VaadinIcon.PLAY));

    private final ApplicationManager applicationManager;

    private final SplitLayout splitLayout;
    private final Button updateWorkflowButton;
    private final Button shareWorkflowButton;
    private final Select<WorkflowDefinition> filterByDefinition;

    private Registration onSubmittedRegistration;
    private Registration onProgressRegistration;
    private UUID lastSubmittedUuid;

    public WorkflowDevelopmentView(ApplicationManager applicationManager) {
        this.applicationManager = applicationManager;

        editor.setMode(AceMode.json);
        editor.setSofttabs(true);
        editor.setTabSize(2);
        editor.addFocusShortcut(Key.KEY_E, KeyModifier.ALT);
        editor.setAutoComplete(true);
        editor.setLiveAutocompletion(true);
        EditorAutoCompleteUtil.apply(editor);
        attachShortcutsToEditor();

        currentWorkflowStatus.addClassNames("workflow-dev-status", LumoUtility.BorderRadius.LARGE, LumoUtility.Padding.SMALL, LumoUtility.FontSize.SMALL);
        currentWorkflowStatus.setVisible(false);

        filterByDefinition = createFilterByDefinition();
        shareWorkflowButton = createShareWorkflowButton();
        updateWorkflowButton = createUpdateWorkflowButton();
        splitLayout = createSplitLayout();
        add(createHeader(getTranslation("workflow-development.page.title"), shareWorkflowButton, updateWorkflowButton, filterByDefinition),
            createContent(splitLayout),
            createFooter(applicationManager.getBeanOfType(BaseConfig.class)));
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
                onFilterByDefinition(workflowDefinition);
                processReadOnlyParamIfPresent(beforeEvent.getLocation().getQueryParameters());
            });
        processRemainingQueryParams(beforeEvent.getLocation().getQueryParameters().getParameters());
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

    private void attachShortcutsToEditor() {
        addShortcutListener(this, () -> editor.setWrap(!editor.isWrap()),
            Key.KEY_W, KeyModifier.CONTROL, KeyModifier.ALT).listenOn(editor);
        addShortcutListener(this, () -> {
            final var currentValue = editor.getValue();
            editor.setValue(toPrettyString(currentValue, currentValue));
        }, Key.KEY_F, KeyModifier.CONTROL, KeyModifier.ALT).listenOn(editor).resetFocusOnActiveElement();
    }

    private Select<WorkflowDefinition> createFilterByDefinition() {
        final var filterByDefinitionSelect = new Select<WorkflowDefinition>();
        filterByDefinitionSelect.setItems(applicationManager.getBeanOfType(WorkflowDefinitionService.class)
            .getAll().toList());
        filterByDefinitionSelect.setPlaceholder(getTranslation("workflow-development.filter.by-template.placeholder"));
        filterByDefinitionSelect.setItemLabelGenerator(WorkflowDefinition::getName);
        filterByDefinitionSelect.addValueChangeListener(event -> onFilterByDefinition(event.getValue()));
        return filterByDefinitionSelect;
    }

    private void onFilterByDefinition(WorkflowDefinition workflowDefinition) {
        filterByDefinition.setValue(workflowDefinition);
        editor.setValue(toPrettyString(workflowDefinition.getScript(), StringUtils.EMPTY));
        if (isLoggedAsAdmin) {
            shareWorkflowButton.setEnabled(true);
            updateWorkflowButton.setEnabled(true);
        }
    }

    private Button createShareWorkflowButton() {
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
            RequestUtil.getFullUrl(url).ifPresent(fullUrl -> {
                ClipboardUtil.copyTo(getElement(), fullUrl);
                Notification.show(getTranslation("workflow-development.clipboard.message"));
            });
        });
        button.addClickShortcut(Key.KEY_S, KeyModifier.CONTROL, KeyModifier.ALT).resetFocusOnActiveElement();
        return button;
    }

    private Button createUpdateWorkflowButton() {
        final var button = new Button(VaadinIcon.EDIT.create());
        button.setTooltipText(getTranslation("workflow-development.update.button.tooltip"));
        button.setEnabled(false);
        button.addThemeVariants(ButtonVariant.LUMO_ICON);
        button.addClickListener(event -> ofNullable(filterByDefinition.getValue())
            .ifPresent(workflowDefinition -> applicationManager.getBeanOfType(WorkflowDefinitionService.class)
                .updateDefinition(workflowDefinition, editor.getValue())));
        button.addClickShortcut(Key.KEY_U, KeyModifier.CONTROL, KeyModifier.ALT).resetFocusOnActiveElement();
        return button;
    }

    private SplitLayout createSplitLayout() {
        final var layout = new SplitLayout(createLeft(), createRight());
        layout.setSplitterPosition(50);
        layout.setSizeFull();
        return layout;
    }

    private Component createLeft() {
        final var layout = new VerticalLayout();
        layout.setSizeFull();
        editor.setSizeFull();

        runWorkflowButton.setIconAfterText(true);
        runWorkflowButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        runWorkflowButton.addClickListener(event -> {
            lastSubmittedUuid = UUID.randomUUID();
            applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(WorkflowDefinitionOnSubmitEvent.builder()
                .token(lastSubmittedUuid)
                .workflowRun(new WorkflowRun(workflowDevParamGrid.getParametersAsMap()))
                .workflowDefinitionScript(editor.getValue())
                .build());
        });
        runWorkflowButton.addClickShortcut(Key.KEY_R, KeyModifier.CONTROL, KeyModifier.ALT).resetFocusOnActiveElement();
        if (!isLoggedAsAdmin) {
            runWorkflowButton.setEnabled(false);
        }
        runWorkflowButton.setWidthFull();

        layout.add(currentWorkflowStatus, editor, new Hr(), runWorkflowButton);
        layout.setFlexGrow(1, editor);
        return layout;
    }

    private Component createRight() {
        final var layout = new VerticalLayout();
        layout.setSizeFull();

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
        currentWorkflowStatus.removeClassNames(LumoUtility.Background.SUCCESS_10, LumoUtility.Background.WARNING_10);
        currentWorkflowStatus.addClassName(LumoUtility.Background.ERROR_10);
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
        if (workflowRun.isRunning()) {
            currentWorkflowStatus.removeClassNames(LumoUtility.Background.ERROR_10, LumoUtility.Background.SUCCESS_10);
            currentWorkflowStatus.addClassName(LumoUtility.Background.WARNING_10);
            currentWorkflowStatus.add(new Span(getTranslation("workflow-development.in-progress.message",
                valueOf(workflowRun.getId()))));
        } else {
            ofNullable(workflowRun.getFailureMessage()).ifPresentOrElse(error -> {
                currentWorkflowStatus.removeClassNames(LumoUtility.Background.SUCCESS_10, LumoUtility.Background.WARNING_10);
                currentWorkflowStatus.addClassName(LumoUtility.Background.ERROR_10);
                currentWorkflowStatus.add(new Span(getTranslation("workflow-development.error.message",
                    valueOf(workflowRun.getId()), workflowRun.getHumanReadableDuration(),
                    workflowRun.getFailureMessage())));
            }, () -> {
                currentWorkflowStatus.removeClassNames(LumoUtility.Background.ERROR_10, LumoUtility.Background.WARNING_10);
                currentWorkflowStatus.addClassName(LumoUtility.Background.SUCCESS_10);
                currentWorkflowStatus.add(new Span(getTranslation("workflow-development.success.message",
                    valueOf(workflowRun.getId()), workflowRun.getHumanReadableDuration())));
            });
        }

        currentWorkflowStatus.setVisible(true);
    }

    private void processReadOnlyParamIfPresent(QueryParameters queryParameters) {
        final var parameters = queryParameters.getParameters();
        if (parameters.isEmpty()) {
            return;
        }
        final var readOnly = ofNullable(parameters.get(READ_ONLY)).isPresent();
        editor.setReadOnly(readOnly);
        updateWorkflowButton.setVisible(!readOnly);
        workflowDevParamGrid.setReadOnly(readOnly);
        if (readOnly) {
            runWorkflowButton.setEnabled(true);
            splitLayout.setSplitterPosition(40);
        }
    }

    private void processRemainingQueryParams(Map<String, List<String>> queryParameters) {
        if (queryParameters.isEmpty()) {
            return;
        }
        final var names = queryParameters.getOrDefault("n", List.of());
        workflowDevParamGrid.addParameters(IntStream.range(0, names.size()).boxed().flatMap(index -> {
            final var type = getValueAtIndex(queryParameters.getOrDefault("t", List.of()), index, WorkflowParameterType.STR.getValue());
            final var value = base64Decode(getValueAtIndex(queryParameters.getOrDefault("v", List.of()), index, StringUtils.EMPTY));
            final var workflowParameterType = ofNullable(WorkflowParameterType.of(type)).orElse(WorkflowParameterType.STR);
            return workflowParameterType.validate(value)
                .<Stream<WorkflowParameter>>map(error -> Stream.empty())
                .orElseGet(() -> Stream.of(WorkflowParameter.of(names.get(index), workflowParameterType, value)));
        }).toList());
    }

}
