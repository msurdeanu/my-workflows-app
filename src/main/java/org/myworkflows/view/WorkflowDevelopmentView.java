package org.myworkflows.view;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.networknt.schema.Error;
import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.DetachEvent;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.KeyModifier;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
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
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
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
import jakarta.annotation.security.PermitAll;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.config.ScriptEditorConfig;
import org.myworkflows.config.UnsavedChangesConfig;
import org.myworkflows.domain.UserRole;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.event.EditorTipOnSubmitEvent;
import org.myworkflows.domain.event.EventType;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmittedEvent;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.util.LangUtil;
import org.myworkflows.util.SecurityUtil;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.Breakpoint;
import org.myworkflows.view.component.PageGuide;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.ScriptEditorDialog;
import org.myworkflows.view.component.WorkflowDevParamGrid;
import org.myworkflows.view.component.WorkflowPipelinePanel;
import org.myworkflows.view.component.WorkflowPrintGrid;
import org.myworkflows.view.component.WorkflowShareDialog;
import org.myworkflows.view.transformer.WorkflowDefinitionScriptToPipelineTransformer;
import org.myworkflows.view.util.ClipboardUtil;
import org.myworkflows.view.util.EditorAutoCompleteUtil;
import org.myworkflows.view.util.RequestUtil;
import org.myworkflows.view.util.ScriptBlockUtil;

import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.vaadin.flow.component.Shortcuts.addShortcutListener;
import static java.lang.String.valueOf;
import static java.util.Optional.ofNullable;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.joining;
import static org.myworkflows.config.SnippetConfig.JS_CODE;
import static org.myworkflows.serializer.SerializerFactory.toPrettyString;
import static org.myworkflows.util.Base64Util.base64Decode;
import static org.myworkflows.util.ListUtil.getValueAtIndex;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@PermitAll
@Route(value = WorkflowDevelopmentView.ROUTE, layout = BaseLayout.class)
public class WorkflowDevelopmentView extends ResponsiveLayout implements HasDynamicTitle, HasUrlParameter<Integer> {

    public static final String ROUTE = "workflow/dev";
    private static final String READ_ONLY = "ro";
    private static final int ERROR_NOTIFICATION_DURATION = 10_000;

    private final AceEditor editor = new AceEditor();
    private final Span editorHelper = new Span();
    private final Div currentWorkflowStatus = new Div();
    private final Badge unsavedChangesBadge = new Badge(getTranslation("workflow-development.unsaved.badge"));
    private final Badge readOnlyBadge = new Badge(getTranslation("workflow-development.read-only.badge"), VaadinIcon.LOCK.create());
    private final WorkflowPipelinePanel workflowPipelinePanel = new WorkflowPipelinePanel();
    private final WorkflowDevParamGrid workflowDevParamGrid = new WorkflowDevParamGrid();
    private final WorkflowPrintGrid workflowPrintGrid = new WorkflowPrintGrid();
    private final boolean isLoggedAsAdmin = UserRole.ADMIN.validate();
    private final Button runWorkflowButton = new Button(getTranslation("workflow-development.run.button"),
        new Icon(VaadinIcon.PLAY));

    private final ApplicationManager applicationManager;

    private final SplitLayout splitLayout;
    private final Button saveWorkflowButton;
    private final Button shareWorkflowButton;
    private final Select<WorkflowDefinition> filterByDefinition;

    private final Map<EventType, Registration> registrations = new EnumMap<EventType, Registration>(EventType.class);
    private UUID lastSubmittedUuid;
    private boolean unsavedChanges;
    private boolean readOnly;

    public WorkflowDevelopmentView(ApplicationManager applicationManager) {
        this.applicationManager = applicationManager;

        editor.setMode(AceMode.yaml);
        editor.setSofttabs(true);
        editor.setShowInvisibles(true);
        editor.setTabSize(2);
        editor.addFocusShortcut(Key.KEY_E, KeyModifier.ALT);
        editor.setAutoComplete(true);
        editor.setEnableSnippets(true);
        editor.setUseWorker(true);
        editor.setLiveAutocompletion(true);
        editor.addAceReadyListener(_ -> {
            editor.getElement().executeJs(JS_CODE);
            editor.getElement().executeJs(ScriptEditorConfig.JS_CODE, getTranslation("workflow-development.script-editor.lens.java"),
                getTranslation("workflow-development.script-editor.lens.groovy"));
            editor.getElement().executeJs(UnsavedChangesConfig.JS_CODE);
        });
        EditorAutoCompleteUtil.apply(editor);
        attachShortcutsToEditor();
        attachScriptEditorToEditor();
        attachUnsavedChangesToEditor();

        currentWorkflowStatus.addClassName("workflow-status");
        currentWorkflowStatus.setVisible(false);
        unsavedChangesBadge.addThemeVariants(BadgeVariant.WARNING);
        unsavedChangesBadge.setVisible(false);
        readOnlyBadge.setVisible(false);
        // a read-only link shows the pipeline of the workflow instead of the editor
        workflowPipelinePanel.setVisible(false);

        filterByDefinition = createFilterByDefinition();
        shareWorkflowButton = createShareWorkflowButton();
        saveWorkflowButton = createSaveWorkflowButton();
        splitLayout = createSplitLayout();
        add(createHeader(getTranslation("workflow-development.page.title"), unsavedChangesBadge, readOnlyBadge, filterByDefinition),
            createContent(splitLayout), createGuide());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-development"));
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter Integer workflowDefinitionId) {
        ofNullable(workflowDefinitionId)
            .flatMap(item -> applicationManager.getBeanOfType(WorkflowDefinitionService.class)
                .getAll(new WorkflowDefinitionFilter().idCriteria(item), 0, 1)
                .findFirst())
            .ifPresent(workflowDefinition -> {
                onFilterByDefinition(workflowDefinition);
                processReadOnlyParamIfPresent(beforeEvent.getLocation().getQueryParameters());
            });
        processRemainingQueryParams(beforeEvent.getLocation().getQueryParameters().getParameters());
    }

    @Override
    public void onWidthChange(Breakpoint breakpoint) {
        // editor and results side by side need a desktop, anything narrower stacks them
        splitLayout.setOrientation(breakpoint == Breakpoint.DESKTOP ? SplitLayout.Orientation.HORIZONTAL : SplitLayout.Orientation.VERTICAL);
    }

    @Override
    protected void onAttach(AttachEvent attachEvent) {
        super.onAttach(attachEvent);
        final var ui = attachEvent.getUI();
        registrations.put(EventType.ON_SUBMITTED_WORKFLOW_DEFINITION, applicationManager.getBeanOfType(EventBroadcaster.class).register(event -> {
            final var workflowResultEvent = (WorkflowDefinitionOnSubmittedEvent) event;
            if (workflowResultEvent.workflowRun().getId().equals(lastSubmittedUuid)
                && !workflowResultEvent.validationMessages().isEmpty()) {
                ui.access(() -> updateWorkflowProgress(workflowResultEvent.validationMessages()));
            }
        }, WorkflowDefinitionOnSubmittedEvent.class));
        registrations.put(EventType.ON_PROGRESS_WORKFLOW_DEFINITION, applicationManager.getBeanOfType(EventBroadcaster.class).register(event -> {
            final var workflowResultEvent = (WorkflowDefinitionOnProgressEvent) event;
            if (workflowResultEvent.workflowRun().getId().equals(lastSubmittedUuid)) {
                ui.access(() -> {
                    updateWorkflowProgress(workflowResultEvent.workflowRun());
                    workflowPrintGrid.setItems(workflowResultEvent.workflowRun().getAllPrints());
                });
            }
        }, WorkflowDefinitionOnProgressEvent.class));
        registrations.put(EventType.ON_SUBMIT_EDITOR_TIP, applicationManager.getBeanOfType(EventBroadcaster.class).register(event -> {
            final var editorTipOnSubmitEvent = (EditorTipOnSubmitEvent) event;
            ui.access(() -> editorHelper.getElement().setProperty("innerHTML", getTranslation("editor.tip." + editorTipOnSubmitEvent.tipId())));
        }, EditorTipOnSubmitEvent.class));
    }

    @Override
    protected void onDetach(DetachEvent detachEvent) {
        registrations.values().forEach(Registration::remove);
        super.onDetach(detachEvent);
    }

    private PageGuide createGuide() {
        final var status = new Div();
        status.addClassNames("workflow-status", "success");
        final var lens = new Span(getTranslation("workflow-development.script-editor.lens.java"));
        lens.addClassName("page-guide-code");
        return createFoldedGuide("workflow-development")
            .legendTitle("workflow-development.guide.legend.title")
            .legend("unsaved", PageGuide.badge(getTranslation("workflow-development.unsaved.badge"), BadgeVariant.WARNING))
            .legend("status", status)
            .legend("lens", lens)
            .legend("run", createGuideButton("workflow-development.run.button", VaadinIcon.PLAY, ButtonVariant.PRIMARY), UserRole.ADMIN)
            .legend("save", createGuideButton("workflow-development.save.button", VaadinIcon.CHECK), UserRole.ADMIN)
            .legend("share", createGuideButton("workflow-development.share.button", VaadinIcon.LINK), UserRole.ADMIN)
            .steps(5)
            .note("shortcuts", PageGuide.fields(
                "Alt+E", getTranslation("workflow-development.guide.notes.shortcuts.focus"),
                "Ctrl+Alt+R", getTranslation("workflow-development.guide.notes.shortcuts.run"),
                "Ctrl+Alt+U", getTranslation("workflow-development.guide.notes.shortcuts.save"),
                "Ctrl+Alt+S", getTranslation("workflow-development.guide.notes.shortcuts.share"),
                "Ctrl+Alt+F", getTranslation("workflow-development.guide.notes.shortcuts.reformat"),
                "Ctrl+Alt+J", getTranslation("workflow-development.guide.notes.shortcuts.script")))
            .note("read-only")
            .note("runs");
    }

    private Button createGuideButton(String textKey, VaadinIcon icon, ButtonVariant... variants) {
        final var button = PageGuide.button(icon, variants);
        button.setText(getTranslation(textKey));
        return button;
    }

    private void attachShortcutsToEditor() {
        addShortcutListener(this, () -> editor.setWrap(!editor.isWrap()),
            Key.KEY_W, KeyModifier.CONTROL, KeyModifier.ALT).listenOn(editor);
        addShortcutListener(this, () -> {
            final var currentValue = editor.getValue();
            editor.setValue(toPrettyString(currentValue, currentValue));
        }, Key.KEY_F, KeyModifier.CONTROL, KeyModifier.ALT).listenOn(editor).resetFocusOnActiveElement();
    }

    private void attachScriptEditorToEditor() {
        editor.getElement().addEventListener("script-edit", event -> openScriptEditor(
                event.getEventData().get("event.detail.value").asString(),
                event.getEventData().get("event.detail.row").asInt()))
            .addEventData("event.detail.value")
            .addEventData("event.detail.row");
    }

    private void attachUnsavedChangesToEditor() {
        editor.getElement().addEventListener("unsaved-change", event -> {
            unsavedChanges = event.getEventData().get("event.detail.unsaved").asBoolean();
            onUnsavedChanges();
        }).addEventData("event.detail.unsaved");
    }

    private void onUnsavedChanges() {
        // only an admin with a definition at hand has something to save
        final var somethingToSave = unsavedChanges && isLoggedAsAdmin && filterByDefinition.getValue() != null;
        saveWorkflowButton.setEnabled(somethingToSave);
        unsavedChangesBadge.setVisible(somethingToSave);
    }

    private void markEditorAsSaved(String script) {
        // the editor answers with an unsaved-change event, as its content may have changed again in the meantime
        editor.getElement().executeJs(UnsavedChangesConfig.MARK_SAVED_JS_CODE, script);
    }

    private void openScriptEditor(String definition, int row) {
        ScriptBlockUtil.locate(definition, row).ifPresentOrElse(scriptBlock -> {
            final var dialog = new ScriptEditorDialog(scriptBlock, editor.isReadOnly());
            dialog.addSaveListener(event -> ScriptBlockUtil.replace(definition, row, event.getScript())
                .ifPresentOrElse(replacement -> writeScript(replacement, dialog), () -> onScriptNotSaved(dialog)));
            dialog.open();
        }, () -> Notification.show(getTranslation("workflow-development.script-editor.not-found.message")));
    }

    private void writeScript(ScriptBlockUtil.Replacement replacement, ScriptEditorDialog dialog) {
        editor.getElement().executeJs("return this.replaceScriptBlock($0, $1, $2, $3, $4, $5)", replacement.startRow(),
                replacement.startColumn(), replacement.endRow(), replacement.endColumn(), replacement.source(), replacement.text())
            .then(Boolean.class, replaced -> {
                if (!Boolean.TRUE.equals(replaced)) {
                    onScriptNotSaved(dialog);
                }
            }, _ -> onScriptNotSaved(dialog));
    }

    private void onScriptNotSaved(ScriptEditorDialog dialog) {
        // reopen the dialog, so that the changes made to the script are not lost
        Notification.show(getTranslation("workflow-development.script-editor.not-saved.message"));
        dialog.open();
    }

    private Select<WorkflowDefinition> createFilterByDefinition() {
        final var filterByDefinitionSelect = new Select<WorkflowDefinition>();
        filterByDefinitionSelect.setItems(applicationManager.getBeanOfType(WorkflowDefinitionService.class)
            .getAll().toList());
        filterByDefinitionSelect.setPlaceholder(getTranslation("workflow-development.filter.by-template.placeholder"));
        filterByDefinitionSelect.setItemLabelGenerator(WorkflowDefinition::getName);
        filterByDefinitionSelect.addClassName("header-filter");
        filterByDefinitionSelect.addValueChangeListener(event -> onFilterByDefinition(event.getValue()));
        return filterByDefinitionSelect;
    }

    private void onFilterByDefinition(WorkflowDefinition workflowDefinition) {
        filterByDefinition.setValue(workflowDefinition);
        final var script = toPrettyString(workflowDefinition.getScript(), StringUtils.EMPTY);
        editor.setValue(script);
        markEditorAsSaved(script);
        workflowPipelinePanel.setPipeline(workflowDefinition.getName(),
            new WorkflowDefinitionScriptToPipelineTransformer().transform(workflowDefinition.getScript()));
        if (isLoggedAsAdmin) {
            shareWorkflowButton.setEnabled(true);
        }
    }

    private Button createShareWorkflowButton() {
        final var button = new Button(getTranslation("workflow-development.share.button"), VaadinIcon.LINK.create());
        button.setTooltipText(getTranslation("workflow-development.share.button.tooltip"));
        button.setEnabled(false);
        button.addClickListener(_ -> ofNullable(filterByDefinition.getValue()).ifPresent(this::openShareDialog));
        button.addClickShortcut(Key.KEY_S, KeyModifier.CONTROL, KeyModifier.ALT).resetFocusOnActiveElement();
        return button;
    }

    private void openShareDialog(WorkflowDefinition workflowDefinition) {
        final var parametersQuery = new QueryParameters(workflowDevParamGrid.getParametersForQuery()).getQueryString();
        final var dialog = new WorkflowShareDialog(workflowDefinition.getName(),
            (readOnly, withParameters) -> createShareLink(workflowDefinition, withParameters ? parametersQuery : StringUtils.EMPTY, readOnly),
            !parametersQuery.isEmpty(), unsavedChanges);
        dialog.addCopyListener(event -> {
            ClipboardUtil.copyTo(getElement(), event.getLink());
            Notification.show(getTranslation("workflow-development.clipboard.message"));
        });
        dialog.open();
    }

    private String createShareLink(WorkflowDefinition workflowDefinition, String parametersQuery, boolean readOnly) {
        final var url = RouteConfiguration.forSessionScope().getUrl(WorkflowDevelopmentView.class, workflowDefinition.getId());
        final var query = Stream.of(parametersQuery, readOnly ? READ_ONLY : StringUtils.EMPTY)
            .filter(not(String::isEmpty))
            .collect(joining("&"));
        final var urlWithQuery = query.isEmpty() ? url : url + "?" + query;
        return RequestUtil.getFullUrl(urlWithQuery).orElse(urlWithQuery);
    }

    private Button createSaveWorkflowButton() {
        final var button = new Button(getTranslation("workflow-development.save.button"), VaadinIcon.CHECK.create());
        button.setTooltipText(getTranslation("workflow-development.save.button.tooltip"));
        button.setEnabled(false);
        button.addClickListener(_ -> ofNullable(filterByDefinition.getValue()).ifPresent(this::saveWorkflowDefinition));
        button.addClickShortcut(Key.KEY_U, KeyModifier.CONTROL, KeyModifier.ALT).resetFocusOnActiveElement();
        return button;
    }

    private void saveWorkflowDefinition(WorkflowDefinition workflowDefinition) {
        final var script = editor.getValue();
        try {
            applicationManager.getBeanOfType(WorkflowDefinitionService.class).updateDefinition(workflowDefinition, script);
        } catch (WorkflowRuntimeException exception) {
            Notification.show(getTranslation("workflow-development.save.error.message", describe(exception)), ERROR_NOTIFICATION_DURATION,
                Notification.Position.BOTTOM_START).addThemeVariants(NotificationVariant.ERROR);
            return;
        }
        markEditorAsSaved(script);
        Notification.show(getTranslation("workflow-development.save.success.message", workflowDefinition.getName()));
    }

    private String describe(WorkflowRuntimeException exception) {
        // a script which cannot be read is the usual reason, and the parser knows where it stopped
        if (exception.getCause() instanceof JsonProcessingException cause) {
            final var message = cause.getOriginalMessage();
            // a syntax error names its line by itself, a value of the wrong type does not
            return ofNullable(cause.getLocation())
                .filter(location -> !message.contains("line " + location.getLineNr()))
                .map(location -> getTranslation("workflow-development.save.error.location", message, location.getLineNr()))
                .orElse(message);
        }
        return exception.getMessage();
    }

    private SplitLayout createSplitLayout() {
        final var layout = new SplitLayout(createLeft(), createRight());
        layout.setSplitterPosition(50);
        layout.setSizeFull();
        return layout;
    }

    private Component createLeft() {
        final var layout = new VerticalLayout();
        layout.setSpacing(false);
        layout.setSizeFull();
        editor.setSizeFull();

        runWorkflowButton.setIconAfterText(true);
        runWorkflowButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        runWorkflowButton.addClickListener(event -> {
            final var workflowRun = new WorkflowRun(null, workflowDevParamGrid.getParametersAsMap(), SecurityUtil.findCurrentUserId().orElse(null));
            lastSubmittedUuid = workflowRun.getId();
            workflowPipelinePanel.start(lastSubmittedUuid);
            applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(WorkflowDefinitionOnSubmitEvent.builder()
                .workflowRun(workflowRun)
                .workflowDefinitionScript(editor.getValue())
                .build());
        });
        runWorkflowButton.addClickShortcut(Key.KEY_R, KeyModifier.CONTROL, KeyModifier.ALT).resetFocusOnActiveElement();
        runWorkflowButton.setTooltipText(getTranslation("workflow-development.run.button.tooltip"));
        if (!isLoggedAsAdmin) {
            runWorkflowButton.setEnabled(false);
        }
        runWorkflowButton.addClassName("workflow-actions-run");

        final var actions = new HorizontalLayout(runWorkflowButton, saveWorkflowButton, shareWorkflowButton);
        actions.addClassName("workflow-actions");
        actions.setWidthFull();

        editorHelper.addClassName("editor-helper");
        layout.add(currentWorkflowStatus, workflowPipelinePanel, editor, editorHelper, new Hr(), actions);
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

    private void updateWorkflowProgress(List<Error> validationMessages) {
        currentWorkflowStatus.removeAll();
        setWorkflowStatus("error");
        final var message = new Div(new Span(getTranslation("workflow-development.validation.message")));
        message.addClassName("workflow-status-message");
        final var listItems = validationMessages.stream()
            .map(error -> error.getInstanceLocation() == null ? error.getMessage() : error.toString())
            .map(ListItem::new)
            .toList();
        message.add(new UnorderedList(listItems.toArray(new ListItem[0])));
        currentWorkflowStatus.add(message);
        currentWorkflowStatus.setVisible(true);
    }

    private void updateWorkflowProgress(WorkflowRun workflowRun) {
        currentWorkflowStatus.removeAll();
        // the editor may hold something else than the definition the pipeline was drawn from, unless it is read-only
        final var position = readOnly ? workflowPipelinePanel.setProgress(workflowRun) : Optional.<String>empty();
        if (workflowRun.isRunning()) {
            setWorkflowStatus("running");
            currentWorkflowStatus.add(createStatusMessage(getTranslation("workflow-development.in-progress.message",
                position.orElseGet(() -> LangUtil.pluralize(getTranslation("workflow-development.command"), workflowRun.getLastSuccessfulIndex() + 1)
                    .map(item -> getTranslation("workflow-development.in-progress.command-message", item))
                    .orElse(StringUtils.EMPTY)))));
        } else {
            ofNullable(workflowRun.getFailureMessage()).ifPresentOrElse(error -> {
                setWorkflowStatus("error");
                currentWorkflowStatus.add(createStatusMessage(getTranslation("workflow-development.error.message",
                    workflowRun.getHumanReadableDuration(), workflowRun.getFailureMessage())));
            }, () -> {
                setWorkflowStatus("success");
                currentWorkflowStatus.add(createStatusMessage(getTranslation("workflow-development.success.message",
                    workflowRun.getHumanReadableDuration())));
            });
        }

        currentWorkflowStatus.add(createCopyWorkflowIdButton(workflowRun));
        currentWorkflowStatus.setVisible(true);
    }

    private void setWorkflowStatus(String status) {
        currentWorkflowStatus.removeClassNames("error", "running", "success");
        currentWorkflowStatus.addClassName(status);
    }

    private Div createStatusMessage(String text) {
        final var message = new Div(new Span(text));
        message.addClassName("workflow-status-message");
        return message;
    }

    private Button createCopyWorkflowIdButton(WorkflowRun workflowRun) {
        final var workflowId = valueOf(workflowRun.getId());
        final var button = new Button(VaadinIcon.COPY_O.create());
        button.addThemeVariants(ButtonVariant.TERTIARY, ButtonVariant.SMALL);
        button.addClassName("workflow-status-copy");
        button.setTooltipText(getTranslation("workflow-development.copy-id.button.tooltip"));
        button.setAriaLabel(getTranslation("workflow-development.copy-id.button.tooltip"));
        button.addClickListener(_ -> {
            ClipboardUtil.copyTo(getElement(), workflowId);
            Notification.show(getTranslation("workflow-development.copy-id.message"));
        });
        return button;
    }

    private void processReadOnlyParamIfPresent(QueryParameters queryParameters) {
        final var parameters = queryParameters.getParameters();
        if (parameters.isEmpty()) {
            return;
        }
        readOnly = ofNullable(parameters.get(READ_ONLY)).isPresent();
        editor.setReadOnly(readOnly);
        saveWorkflowButton.setVisible(!readOnly);
        workflowDevParamGrid.setReadOnly(readOnly);
        readOnlyBadge.setVisible(readOnly);
        workflowPipelinePanel.setVisible(readOnly);
        editor.setVisible(!readOnly);
        editorHelper.setVisible(!readOnly);
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
                .<Stream<WorkflowParameter>>map(_ -> Stream.empty())
                .orElseGet(() -> Stream.of(WorkflowParameter.of(names.get(index), workflowParameterType, value)));
        }).toList());
    }

}
