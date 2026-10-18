package org.myworkflows.view;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Hr;
import com.vaadin.flow.component.html.ListItem;
import com.vaadin.flow.component.html.Paragraph;
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
import com.vaadin.flow.component.tabs.Tab;
import com.vaadin.flow.component.tabs.Tabs;
import com.vaadin.flow.component.tabs.TabsVariant;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.QueryParameters;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.router.RouteConfiguration;
import com.vaadin.flow.router.RouterLink;
import com.vaadin.flow.shared.Registration;
import de.f0rce.ace.AceEditor;
import de.f0rce.ace.enums.AceMode;
import jakarta.annotation.security.PermitAll;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.config.DebugGutterConfig;
import org.myworkflows.config.ScriptEditorConfig;
import org.myworkflows.config.UnsavedChangesConfig;
import org.myworkflows.domain.UserAccountDetails;
import org.myworkflows.domain.UserRole;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunApproval;
import org.myworkflows.domain.WorkflowRunDebugger;
import org.myworkflows.domain.WorkflowRunPause;
import org.myworkflows.domain.WorkflowRunPrint;
import org.myworkflows.domain.WorkflowRunVariable;
import org.myworkflows.domain.event.EditorTipOnSubmitEvent;
import org.myworkflows.domain.event.EventType;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmittedEvent;
import org.myworkflows.domain.event.WorkflowReportOnChangeEvent;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.service.WorkflowRunService;
import org.myworkflows.service.WorkflowScriptService;
import org.myworkflows.util.LangUtil;
import org.myworkflows.util.SecurityUtil;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.Breakpoint;
import org.myworkflows.view.component.PageGuide;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.ScriptEditorDialog;
import org.myworkflows.view.component.WorkflowApprovalDialog;
import org.myworkflows.view.component.WorkflowDevParamGrid;
import org.myworkflows.view.component.WorkflowDuplicateDialog;
import org.myworkflows.view.component.WorkflowPipelinePanel;
import org.myworkflows.view.component.WorkflowPrintGrid;
import org.myworkflows.view.component.WorkflowShareDialog;
import org.myworkflows.view.component.WorkflowVariableGrid;
import org.myworkflows.view.transformer.WorkflowDefinitionScriptToPipelineTransformer;
import org.myworkflows.view.util.ClipboardUtil;
import org.myworkflows.view.util.EditorAutoCompleteUtil;
import org.myworkflows.view.util.EditorSnippetUtil;
import org.myworkflows.view.util.RequestUtil;
import org.myworkflows.view.util.ScriptBlockUtil;
import org.myworkflows.view.util.ScriptCommand;
import org.myworkflows.view.util.ScriptCommandUtil;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.vaadin.flow.component.Shortcuts.addShortcutListener;
import static java.lang.String.valueOf;
import static java.util.Optional.ofNullable;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.joining;
import static org.myworkflows.serializer.SerializerFactory.toObject;
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

    private static final int REPORT_NOTIFICATION_DURATION = 10_000;

    private static final ObjectMapper JSON_MAPPER = new ObjectMapper();
    private static final String CLEAR_COMMAND_MARK_JS = "this.setDebugCommand && this.setDebugCommand(-1, -1, '')";
    private static final String PAUSED = "paused";
    private static final String FAILED = "failed";
    private static final int NO_ROW = -1;

    private final AceEditor editor = new AceEditor();
    private final Span editorHelper = new Span();
    private final Div currentWorkflowStatus = new Div();
    private final Badge unsavedChangesBadge = new Badge(getTranslation("workflow-development.unsaved.badge"));
    private final Badge readOnlyBadge = new Badge(getTranslation("workflow-development.read-only.badge"), VaadinIcon.LOCK.create());
    private final WorkflowPipelinePanel workflowPipelinePanel = new WorkflowPipelinePanel();
    private final WorkflowDevParamGrid workflowDevParamGrid = new WorkflowDevParamGrid();
    private final WorkflowVariableGrid workflowVariableGrid = new WorkflowVariableGrid();
    private final WorkflowPrintGrid workflowPrintGrid = new WorkflowPrintGrid();
    private final Badge parameterTitleCount = createCountBadge();
    private final Badge parameterTabCount = createCountBadge();
    private final Badge variableTabCount = createCountBadge();
    private final Badge printCount = createCountBadge();
    private final Div parameterTitle = createPaneTitle(getTranslation("workflow-development.input.label"), parameterTitleCount);
    private final Tab variableTab = new Tab(new Span(getTranslation("workflow-development.variables.label")), variableTabCount);
    private final Tab parameterTab = new Tab(new Span(getTranslation("workflow-development.input.label")), parameterTabCount);
    private final Tabs debugTabs = new Tabs(variableTab, parameterTab);
    private final Paragraph nextRunNote = new Paragraph(getTranslation("workflow-development.parameters.next-run"));
    private final SplitLayout resultsLayout = new SplitLayout();
    private final boolean isLoggedAsAdmin = UserRole.ADMIN.validate();
    private final String currentUsername = SecurityUtil.findCurrentAccount().map(UserAccountDetails::getUsername).orElse(null);
    private final Button runWorkflowButton = new Button(getTranslation("workflow-development.run.button"),
        new Icon(VaadinIcon.PLAY));
    private final Button debugWorkflowButton = new Button(getTranslation("workflow-development.debug.button"),
        new Icon(VaadinIcon.BUG));
    /**
     * The buttons the status offers for the run it is about. A run is told one thing at a time, so they all go off
     * once one of them is clicked.
     */
    private final List<Button> statusActions = new ArrayList<>();

    private final ApplicationManager applicationManager;

    private final SplitLayout splitLayout;
    private final Button saveWorkflowButton;
    private final Button saveCopyWorkflowButton;
    private final Button shareWorkflowButton;
    private final Select<WorkflowDefinition> filterByDefinition;

    private final Map<EventType, Registration> registrations = new EnumMap<EventType, Registration>(EventType.class);
    private UUID lastSubmittedUuid;
    /**
     * The last run started from this page. It is kept as it is in memory, with everything it holds, to be told how to
     * go on while it is paused and to be started again from one of its commands once it is over.
     */
    private WorkflowRun followedRun;
    /**
     * Whether that run was started with Debug, and pauses at the breakpoints of the editor.
     */
    private boolean debugging;
    /**
     * The command the editor marks: the one the run is paused before, or the one it failed at.
     */
    private CommandMark commandMark;
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
            EditorSnippetUtil.apply(editor);
            editor.getElement().executeJs(ScriptEditorConfig.JS_CODE, getTranslation("workflow-development.script-editor.lens.java"),
                getTranslation("workflow-development.script-editor.lens.groovy"));
            editor.getElement().executeJs(UnsavedChangesConfig.JS_CODE);
            editor.getElement().executeJs(DebugGutterConfig.JS_CODE);
        });
        EditorAutoCompleteUtil.apply(editor);
        attachShortcutsToEditor();
        attachScriptEditorToEditor();
        attachUnsavedChangesToEditor();
        attachBreakpointsToEditor();

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
        saveCopyWorkflowButton = createSaveCopyWorkflowButton();
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
                    showPrints(workflowResultEvent.workflowRun().getAllPrints());
                });
            }
        }, WorkflowDefinitionOnProgressEvent.class));
        registrations.put(EventType.ON_CHANGE_WORKFLOW_REPORT, applicationManager.getBeanOfType(EventBroadcaster.class).register(event -> {
            final var workflowReportChangeEvent = (WorkflowReportOnChangeEvent) event;
            if (!workflowReportChangeEvent.deleted()) {
                workflowReportChangeEvent.workflowReports().stream()
                    .filter(workflowReport -> workflowReport.workflowRunId().equals(lastSubmittedUuid))
                    .forEach(workflowReport -> ui.access(() -> showReportWritten(workflowReport)));
            }
        }, WorkflowReportOnChangeEvent.class));
        registrations.put(EventType.ON_SUBMIT_EDITOR_TIP, applicationManager.getBeanOfType(EventBroadcaster.class).register(event -> {
            final var editorTipOnSubmitEvent = (EditorTipOnSubmitEvent) event;
            ui.access(() -> editorHelper.getElement().setProperty("innerHTML", getTranslation("editor.tip." + editorTipOnSubmitEvent.tipId())));
        }, EditorTipOnSubmitEvent.class));
    }

    @Override
    protected void onDetach(DetachEvent detachEvent) {
        registrations.values().forEach(Registration::remove);
        // nobody is left to tell a paused run how to go on
        stopPausedRun();
        super.onDetach(detachEvent);
    }

    /**
     * A report of the run of this page is read on the reports page, which the notification links to.
     */
    private void showReportWritten(WorkflowReport workflowReport) {
        final var routerLink = new RouterLink(getTranslation("workflow-development.report.open.link"), WorkflowReportView.class, workflowReport.id());
        final var content = new Div(new Span(getTranslation("workflow-development.report.written.message", workflowReport.title())), routerLink);
        content.addClassName("workflow-report-notification");
        final var notification = new Notification(content);
        notification.setDuration(REPORT_NOTIFICATION_DURATION);
        notification.setPosition(Notification.Position.BOTTOM_START);
        routerLink.getElement().addEventListener("click", _ -> notification.close());
        notification.open();
    }

    private PageGuide createGuide() {
        final var status = new Div();
        status.addClassNames("workflow-status", "success");
        final var lens = new Span(getTranslation("workflow-development.script-editor.lens.java"));
        lens.addClassName("page-guide-code");
        final var breakpoint = new Span();
        breakpoint.addClassName("workflow-breakpoint");
        return createFoldedGuide("workflow-development")
            .legendTitle("workflow-development.guide.legend.title")
            .legend("unsaved", PageGuide.badge(getTranslation("workflow-development.unsaved.badge"), BadgeVariant.WARNING))
            .legend("status", status)
            .legend("lens", lens)
            .legend("run", createGuideButton("workflow-development.run.button", VaadinIcon.PLAY, ButtonVariant.PRIMARY), UserRole.ADMIN)
            .legend("debug", createGuideButton("workflow-development.debug.button", VaadinIcon.BUG), UserRole.ADMIN)
            .legend("breakpoint", breakpoint, UserRole.ADMIN)
            .legend("save", createGuideButton("workflow-development.save.button", VaadinIcon.CHECK), UserRole.ADMIN)
            .legend("save-copy", createGuideButton("workflow-development.save-copy.button", VaadinIcon.COPY_O), UserRole.ADMIN)
            .legend("share", createGuideButton("workflow-development.share.button", VaadinIcon.LINK), UserRole.ADMIN)
            .steps(5)
            .note("shortcuts", PageGuide.fields(
                "Alt+E", getTranslation("workflow-development.guide.notes.shortcuts.focus"),
                "Ctrl+Alt+R", getTranslation("workflow-development.guide.notes.shortcuts.run"),
                "Ctrl+Alt+D", getTranslation("workflow-development.guide.notes.shortcuts.debug"),
                "Ctrl+Alt+U", getTranslation("workflow-development.guide.notes.shortcuts.save"),
                "Ctrl+Alt+S", getTranslation("workflow-development.guide.notes.shortcuts.share"),
                "Ctrl+Alt+F", getTranslation("workflow-development.guide.notes.shortcuts.reformat"),
                "Ctrl+Alt+J", getTranslation("workflow-development.guide.notes.shortcuts.script")))
            .note("debugging", PageGuide.fields(
                "Ctrl+Alt+N", getTranslation("workflow-development.guide.notes.debugging.step"),
                "Ctrl+Alt+G", getTranslation("workflow-development.guide.notes.debugging.continue"),
                "Ctrl+Alt+H", getTranslation("workflow-development.guide.notes.debugging.from-cursor")))
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
        addShortcutListener(this, this::reformat, Key.KEY_F, KeyModifier.CONTROL, KeyModifier.ALT).listenOn(editor).resetFocusOnActiveElement();
    }

    private void reformat() {
        withEditorState(state -> {
            final var breakpoints = ScriptCommandUtil.findIndexesByRows(ScriptCommandUtil.locate(state.value()), state.breakpoints());
            final var script = toPrettyString(state.value(), state.value());
            editor.setValue(script);
            // the editor replaces its whole content, which leaves nothing where it was: every mark is drawn again
            showBreakpoints(ScriptCommandUtil.locate(script), breakpoints);
            showCommandMark(script);
        });
    }

    private void attachBreakpointsToEditor() {
        editor.getElement().addEventListener("breakpoint-toggle", event -> toggleBreakpoint(
                event.getEventData().get("event.detail.row").asInt(),
                readEditorState(event.getEventData().get("event.detail.state").asString())))
            .addEventData("event.detail.row")
            .addEventData("event.detail.state");
    }

    /**
     * Sets or removes the breakpoint of the command the given row belongs to. A breakpoint is drawn on the first line of
     * its command, whichever of its lines was clicked, as a run can pause only between two commands.
     */
    private void toggleBreakpoint(int row, EditorState state) {
        // debugging is for whoever can read the definition and run it: an admin, in the editor
        if (!isLoggedAsAdmin || readOnly) {
            return;
        }
        final var commands = ScriptCommandUtil.locate(state.value());
        ScriptCommandUtil.findByRow(commands, row).ifPresent(command -> {
            if (command.finalCommand()) {
                Notification.show(getTranslation("workflow-development.debug.final-command.message"));
                return;
            }
            final var breakpoints = new HashSet<>(ScriptCommandUtil.findIndexesByRows(commands, state.breakpoints()));
            if (!breakpoints.remove(command.index())) {
                breakpoints.add(command.index());
            }
            showBreakpoints(commands, breakpoints);
            // a run which is on its way learns about it at once, instead of the next time it is told how to go on
            if (debugging) {
                ofNullable(followedRun).map(WorkflowRun::getDebugger).ifPresent(debugger -> debugger.setBreakpoints(breakpoints));
            }
        });
    }

    private void showBreakpoints(List<ScriptCommand> commands, Set<Integer> breakpoints) {
        final var rows = breakpoints.stream()
            .flatMap(index -> ScriptCommandUtil.findByIndex(commands, index).stream())
            .map(ScriptCommand::firstRow)
            .filter(row -> row >= 0)
            .map(String::valueOf)
            .collect(joining(","));
        editor.getElement().executeJs("this.setBreakpointRows && this.setBreakpointRows($0)", rows);
    }

    /**
     * Reads the editor as it is in the browser right now - its content, the row of its cursor and the rows of its
     * breakpoints - and hands it over. The server knows neither the cursor nor the breakpoints, and it learns about the
     * content only when the editor loses the focus.
     */
    private void withEditorState(SerializableConsumer<EditorState> action) {
        editor.getElement().executeJs("return this.debugState ? this.debugState() : ''")
            .then(String.class, stateAsJson -> action.accept(readEditorState(stateAsJson)), error -> {
                log.warn("The state of the workflow editor could not be read: {}", error);
                action.accept(new EditorState(editor.getValue(), NO_ROW, List.of()));
            });
    }

    private EditorState readEditorState(String stateAsJson) {
        try {
            final var state = JSON_MAPPER.readTree(StringUtils.defaultString(stateAsJson));
            if (state != null && state.hasNonNull("value")) {
                final var breakpoints = new ArrayList<Integer>();
                state.path("breakpoints").forEach(row -> breakpoints.add(row.asInt()));
                return new EditorState(state.path("value").asText(), state.path("cursor").asInt(NO_ROW), breakpoints);
            }
        } catch (JsonProcessingException exception) {
            log.warn("The state of the workflow editor could not be read.", exception);
        }
        // the editor is not ready yet: what the server knows about it has to do
        return new EditorState(editor.getValue(), NO_ROW, List.of());
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
        // a definition chosen by the page itself keeps what the editor holds: see saveWorkflowDefinitionAsCopy
        filterByDefinitionSelect.addValueChangeListener(event -> {
            if (event.isFromClient()) {
                onFilterByDefinition(event.getValue());
            }
        });
        return filterByDefinitionSelect;
    }

    private void onFilterByDefinition(WorkflowDefinition workflowDefinition) {
        filterByDefinition.setValue(workflowDefinition);
        // the run of another definition can neither go on nor start again with the script of this one
        forgetRun();
        final var script = toPrettyString(workflowDefinition.getScript(), StringUtils.EMPTY);
        editor.setValue(script);
        markEditorAsSaved(script);
        workflowPipelinePanel.setPipeline(workflowDefinition.getName(),
            new WorkflowDefinitionScriptToPipelineTransformer().transform(workflowDefinition.getScript()));
        if (isLoggedAsAdmin) {
            shareWorkflowButton.setEnabled(true);
            saveCopyWorkflowButton.setEnabled(true);
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

    private Button createSaveCopyWorkflowButton() {
        final var button = new Button(getTranslation("workflow-development.save-copy.button"), VaadinIcon.COPY_O.create());
        button.setTooltipText(getTranslation("workflow-development.save-copy.button.tooltip"));
        button.setEnabled(false);
        button.addClickListener(_ -> ofNullable(filterByDefinition.getValue()).ifPresent(this::openSaveCopyDialog));
        return button;
    }

    private void openSaveCopyDialog(WorkflowDefinition workflowDefinition) {
        final var workflowDefinitionService = applicationManager.getBeanOfType(WorkflowDefinitionService.class);
        final var dialog = new WorkflowDuplicateDialog(workflowDefinition.getName(), workflowDefinitionService.suggestCopyName(workflowDefinition),
            workflowDefinitionService::isNameTaken, this::saveWorkflowDefinitionAsCopy);
        dialog.addDetails(new Paragraph(getTranslation("workflow-development.save-copy.help", workflowDefinition.getName())));
        dialog.open();
    }

    /**
     * Saves what the editor holds as a new definition and goes on with it. The editor keeps its text - comments and
     * layout included, which the saved script does not know of - and the run it follows, so the page is switched to
     * the copy without being loaded again.
     */
    private void saveWorkflowDefinitionAsCopy(String name) {
        final var script = editor.getValue();
        final var workflowDefinitionService = applicationManager.getBeanOfType(WorkflowDefinitionService.class);
        final WorkflowDefinition copy;
        try {
            copy = workflowDefinitionService.duplicate(name, script);
        } catch (WorkflowRuntimeException exception) {
            Notification.show(getTranslation("workflow-development.save.error.message", describe(exception)), ERROR_NOTIFICATION_DURATION,
                Notification.Position.BOTTOM_START).addThemeVariants(NotificationVariant.ERROR);
            return;
        }
        filterByDefinition.setItems(workflowDefinitionService.getAll().toList());
        filterByDefinition.setValue(copy);
        workflowPipelinePanel.setPipeline(copy.getName(), new WorkflowDefinitionScriptToPipelineTransformer().transform(copy.getScript()));
        markEditorAsSaved(script);
        getUI().ifPresent(ui -> ui.getPage().getHistory().replaceState(null,
            RouteConfiguration.forSessionScope().getUrl(WorkflowDevelopmentView.class, copy.getId())));
        Notification.show(getTranslation("workflow-development.save-copy.success.message", copy.getName()));
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

    private String describe(Error validationMessage) {
        return validationMessage.getInstanceLocation() == null ? validationMessage.getMessage() : validationMessage.toString();
    }

    private String describe(List<Error> validationMessages) {
        final var first = describe(validationMessages.getFirst());
        return validationMessages.size() == 1 ? first : getTranslation("workflow-development.debug.invalid.more", first, validationMessages.size() - 1);
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
        // whoever only has a read-only link is shown neither the definition nor what its run holds
        runWorkflowButton.addClickListener(_ -> start(editor.getValue(), readOnly ? null : WorkflowRunDebugger.watching(), false));
        runWorkflowButton.addClickShortcut(Key.KEY_R, KeyModifier.CONTROL, KeyModifier.ALT).resetFocusOnActiveElement();
        runWorkflowButton.setTooltipText(getTranslation("workflow-development.run.button.tooltip"));
        runWorkflowButton.addClassName("workflow-actions-run");

        debugWorkflowButton.addClickListener(_ -> withEditorState(state -> start(state.value(), WorkflowRunDebugger.pausingBefore(
            ScriptCommandUtil.findIndexesByRows(ScriptCommandUtil.locate(state.value()), state.breakpoints())), true)));
        debugWorkflowButton.addClickShortcut(Key.KEY_D, KeyModifier.CONTROL, KeyModifier.ALT).setBrowserDefaultAllowed(false);
        debugWorkflowButton.setTooltipText(getTranslation("workflow-development.debug.button.tooltip"));
        if (!isLoggedAsAdmin) {
            runWorkflowButton.setEnabled(false);
            debugWorkflowButton.setEnabled(false);
        }

        final var actions = new HorizontalLayout(runWorkflowButton, debugWorkflowButton, saveWorkflowButton, saveCopyWorkflowButton,
            shareWorkflowButton);
        actions.addClassName("workflow-actions");
        actions.setWidthFull();

        editorHelper.addClassName("editor-helper");
        layout.add(currentWorkflowStatus, workflowPipelinePanel, editor, editorHelper, new Hr(), actions);
        layout.setFlexGrow(1, editor);
        return layout;
    }

    /**
     * The parameters above the prints, each in an area which scrolls by itself. In a debug session what the run holds
     * takes the place of the parameters, which can no longer change that run and are among what it holds, and a
     * splitter shares the height between it and the prints.
     */
    private Component createRight() {
        final var layout = new VerticalLayout();
        layout.setSizeFull();

        debugTabs.addThemeVariants(TabsVariant.SMALL);
        debugTabs.addSelectedChangeListener(_ -> showDebugTab());
        nextRunNote.addClassName("workflow-results-note");
        workflowDevParamGrid.setSizeListener(size -> {
            setCount(parameterTitleCount, size);
            setCount(parameterTabCount, size);
        });

        resultsLayout.setOrientation(SplitLayout.Orientation.VERTICAL);
        resultsLayout.addClassName("workflow-results");
        resultsLayout.setWidthFull();
        resultsLayout.setSplitterPosition(50);
        resultsLayout.addToPrimary(createPane(new Div(parameterTitle, debugTabs), nextRunNote, workflowDevParamGrid, workflowVariableGrid));
        resultsLayout.addToSecondary(createPane(new Div(createPaneTitle(getTranslation("workflow-development.print.label"), printCount)),
            workflowPrintGrid));
        showDebugSession(false);

        layout.add(resultsLayout);
        layout.setFlexGrow(1, resultsLayout);
        return layout;
    }

    private static Div createPane(Div header, Component... content) {
        header.addClassName("workflow-results-header");
        final var body = new Div(content);
        body.addClassName("workflow-results-body");
        final var pane = new Div(header, body);
        pane.addClassName("workflow-results-pane");
        return pane;
    }

    private static Div createPaneTitle(String text, Badge count) {
        final var title = new Div(new Span(text), count);
        title.addClassName("workflow-results-title");
        return title;
    }

    private static Badge createCountBadge() {
        final var badge = new Badge();
        badge.addThemeVariants(BadgeVariant.NUMBER_ONLY, BadgeVariant.SMALL);
        badge.setVisible(false);
        return badge;
    }

    private static void setCount(Badge badge, int count) {
        badge.setNumber(count);
        // a list with nothing in it says so itself
        badge.setVisible(count > 0);
    }

    /**
     * Shows the parameters above the prints, or - in a debug session, which lasts from Debug to the next Run - what the
     * run holds. The parameters stay one tab away there, for the next run.
     */
    private void showDebugSession(boolean debugSession) {
        resultsLayout.setClassName("debugging", debugSession);
        parameterTitle.setVisible(!debugSession);
        debugTabs.setVisible(debugSession);
        if (debugSession) {
            debugTabs.setSelectedTab(variableTab);
        }
        showDebugTab();
    }

    private void showDebugTab() {
        final var variables = debugTabs.isVisible() && debugTabs.getSelectedTab() == variableTab;
        workflowVariableGrid.setVisible(variables);
        workflowDevParamGrid.setVisible(!variables);
        nextRunNote.setVisible(debugTabs.isVisible() && !variables);
    }

    private void showVariables(List<WorkflowRunVariable> variables) {
        workflowVariableGrid.setItems(variables);
        setCount(variableTabCount, variables.size());
    }

    private void showPrints(List<WorkflowRunPrint> prints) {
        workflowPrintGrid.setItems(prints);
        setCount(printCount, prints.size());
    }

    /**
     * Starts a run of the given script from its first command.
     *
     * @param debugger who follows the run: nobody, somebody who looks at what it held once it is over, or somebody who
     *     makes it pause on its way.
     * @param debug whether the run pauses at the breakpoints of the editor.
     */
    private void start(String script, WorkflowRunDebugger debugger, boolean debug) {
        stopPausedRun();
        final var workflowRun = new WorkflowRun(null, workflowDevParamGrid.getParametersAsMap(), SecurityUtil.findCurrentUserId().orElse(null));
        workflowRun.setDebugger(debugger);
        followedRun = workflowRun;
        debugging = debug;
        lastSubmittedUuid = workflowRun.getId();
        workflowPipelinePanel.start(lastSubmittedUuid);
        showVariables(List.of());
        showDebugSession(debug);
        applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(WorkflowDefinitionOnSubmitEvent.builder()
            .workflowRun(workflowRun)
            .workflowDefinitionScript(script)
            .build());
    }

    private void stopPausedRun() {
        ofNullable(followedRun).map(WorkflowRun::getPause)
            .ifPresent(pause -> pause.resume(WorkflowRunPause.Resume.stop(currentUsername)));
    }

    private void forgetRun() {
        stopPausedRun();
        followedRun = null;
        debugging = false;
        lastSubmittedUuid = null;
        statusActions.clear();
        currentWorkflowStatus.setVisible(false);
        showVariables(List.of());
        showPrints(List.of());
        showDebugSession(false);
        markCommand(null);
        showBreakpoints(List.of(), Set.of());
    }

    /**
     * Tells the run which is paused how to go on, with the script and the breakpoints the editor has by now.
     *
     * @param fromCursor whether the run goes on with the command the cursor is in, instead of the one it is paused before.
     */
    private void resume(WorkflowRun workflowRun, WorkflowRunPause pause, WorkflowRunPause.Action action, boolean fromCursor) {
        withEditorState(state -> {
            final var commands = ScriptCommandUtil.locate(state.value());
            final var commandIndex = fromCursor ? findCommandIndexAtCursor(commands, state) : Optional.of(pause.getCommandIndex());
            final var resume = commandIndex.flatMap(index -> readScript(state.value()).map(script -> new WorkflowRunPause.Resume(action, index, script,
                ScriptCommandUtil.findIndexesByRows(commands, state.breakpoints()), currentUsername)));
            if (resume.isEmpty()) {
                // the run was told nothing, so the buttons are needed again
                updateWorkflowProgress(workflowRun);
            } else if (!applicationManager.getBeanOfType(WorkflowRunService.class).resume(workflowRun, pause, resume.get())) {
                onRunNotPausedAnymore(workflowRun);
            }
        });
    }

    private void stop(WorkflowRun workflowRun, WorkflowRunPause pause) {
        if (!applicationManager.getBeanOfType(WorkflowRunService.class).resume(workflowRun, pause, WorkflowRunPause.Resume.stop(currentUsername))) {
            onRunNotPausedAnymore(workflowRun);
        }
    }

    private void onRunNotPausedAnymore(WorkflowRun workflowRun) {
        // the pause timed out, or the run was cancelled from the runs page
        Notification.show(getTranslation("workflow-development.debug.not-paused.message"));
        updateWorkflowProgress(workflowRun);
    }

    /**
     * Starts the run which is over again, with the script and the breakpoints the editor has by now and with everything
     * the run held when it ended.
     *
     * @param commandIndex the command to start from, or nothing for the command the cursor is in.
     */
    private void runFrom(WorkflowRun workflowRun, Integer commandIndex) {
        withEditorState(state -> {
            final var commands = ScriptCommandUtil.locate(state.value());
            final var index = commandIndex != null ? Optional.of(commandIndex) : findCommandIndexAtCursor(commands, state);
            final var script = index.flatMap(_ -> readScript(state.value()));
            final var breakpoints = debugging ? ScriptCommandUtil.findIndexesByRows(commands, state.breakpoints()) : Set.<Integer>of();
            if (script.isEmpty()) {
                updateWorkflowProgress(workflowRun);
            } else if (applicationManager.getBeanOfType(WorkflowRunService.class).runFrom(workflowRun, index.get(), script.get(), breakpoints)) {
                // the run says how far it is once it starts: until then, nothing else can be asked of it
                currentWorkflowStatus.removeAll();
                statusActions.clear();
                setWorkflowStatus("running");
                currentWorkflowStatus.add(createStatusMessage(getTranslation("workflow-development.in-progress.message", StringUtils.EMPTY)));
                markCommand(null);
            } else {
                Notification.show(getTranslation("workflow-development.debug.not-started.message"));
                updateWorkflowProgress(workflowRun);
            }
        });
    }

    private Optional<Integer> findCommandIndexAtCursor(List<ScriptCommand> commands, EditorState state) {
        final var command = ScriptCommandUtil.findByRow(commands, state.cursor());
        if (command.isEmpty()) {
            Notification.show(getTranslation("workflow-development.debug.no-command.message"));
        } else if (command.get().finalCommand()) {
            Notification.show(getTranslation("workflow-development.debug.final-command.message"));
        }
        return command.filter(item -> !item.finalCommand()).map(ScriptCommand::index);
    }

    /**
     * @return the script as a run can take it over, or nothing - after saying why - when it cannot be run as it is.
     */
    private Optional<WorkflowDefinitionScript> readScript(String scriptAsString) {
        final var validationMessages = applicationManager.getBeanOfType(WorkflowScriptService.class).validate(scriptAsString);
        try {
            if (validationMessages.isEmpty()) {
                return Optional.of(toObject(scriptAsString, WorkflowDefinitionScript.class));
            }
            showError(getTranslation("workflow-development.debug.invalid.message", describe(validationMessages)));
        } catch (WorkflowRuntimeException exception) {
            showError(getTranslation("workflow-development.debug.invalid.message", describe(exception)));
        }
        return Optional.empty();
    }

    private void showError(String text) {
        Notification.show(text, ERROR_NOTIFICATION_DURATION, Notification.Position.BOTTOM_START).addThemeVariants(NotificationVariant.ERROR);
    }

    /**
     * Marks, in the editor, the command the run is paused before or failed at - or no command at all.
     */
    private void markCommand(CommandMark mark) {
        if (Objects.equals(mark, commandMark)) {
            return;
        }
        commandMark = mark;
        if (mark == null) {
            editor.getElement().executeJs(CLEAR_COMMAND_MARK_JS);
        } else {
            withEditorState(state -> showCommandMark(state.value()));
        }
    }

    /**
     * Draws the mark on the lines its command is written on in the given script, which the editor holds.
     */
    private void showCommandMark(String script) {
        final var command = ofNullable(commandMark)
            .flatMap(mark -> ScriptCommandUtil.findByIndex(ScriptCommandUtil.locate(script), mark.commandIndex()))
            .filter(item -> item.firstRow() >= 0);
        if (command.isEmpty()) {
            editor.getElement().executeJs(CLEAR_COMMAND_MARK_JS);
            return;
        }
        // the whole command the run stands before is highlighted, but only the first line of the one it failed at
        final var firstRow = command.get().firstRow();
        final var lastRow = PAUSED.equals(commandMark.kind()) ? command.get().lastRow() : firstRow;
        editor.getElement().executeJs("this.setDebugCommand && this.setDebugCommand($0, $1, $2)", firstRow, lastRow, commandMark.kind());
    }

    private void updateWorkflowProgress(List<Error> validationMessages) {
        currentWorkflowStatus.removeAll();
        statusActions.clear();
        setWorkflowStatus("error");
        final var message = new Div(new Span(getTranslation("workflow-development.validation.message")));
        message.addClassName("workflow-status-message");
        final var listItems = validationMessages.stream()
            .map(this::describe)
            .map(ListItem::new)
            .toList();
        message.add(new UnorderedList(listItems.toArray(new ListItem[0])));
        currentWorkflowStatus.add(message);
        currentWorkflowStatus.setVisible(true);
    }

    private void updateWorkflowProgress(WorkflowRun workflowRun) {
        currentWorkflowStatus.removeAll();
        statusActions.clear();
        // the editor may hold something else than the definition the pipeline was drawn from, unless it is read-only
        final var position = readOnly ? workflowPipelinePanel.setProgress(workflowRun) : Optional.<String>empty();
        final var approval = workflowRun.getApproval();
        final var pause = workflowRun.getPause();
        if (workflowRun.isRunning() && pause != null) {
            setWorkflowStatus(PAUSED);
            currentWorkflowStatus.add(createStatusMessage(getTranslation("workflow-development.paused.message", pause.getCommandIndex() + 1,
                pause.getCommandName())));
            addPausedActions(workflowRun, pause);
        } else if (workflowRun.isRunning() && approval != null) {
            setWorkflowStatus("running");
            currentWorkflowStatus.add(createStatusMessage(getTranslation("workflow-development.approval.message", approval.getCommandName())),
                createApprovalButton(workflowRun, approval));
        } else if (workflowRun.isRunning()) {
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
            addEndedActions(workflowRun);
        }

        currentWorkflowStatus.add(createCopyWorkflowIdButton(workflowRun));
        currentWorkflowStatus.setVisible(true);
        showDebugger(workflowRun);
    }

    /**
     * Shows what only a run with a debugger has: marked in the editor, the command it is paused before or failed at,
     * and - for a run started with Debug - what it held the last time it stood still. A plain run keeps what it held
     * for From cursor, without showing it.
     */
    private void showDebugger(WorkflowRun workflowRun) {
        final var debugger = workflowRun.getDebugger();
        if (debugger == null) {
            return;
        }
        final var pause = workflowRun.getPause();
        if (!workflowRun.isRunning()) {
            if (debugging) {
                showVariables(debugger.getVariables());
            }
            markCommand(findFailedCommandIndex(workflowRun)
                .map(index -> new CommandMark(workflowRun.getFailureMessage(), index, FAILED))
                .orElse(null));
        } else if (pause != null) {
            showVariables(debugger.getVariables());
            markCommand(new CommandMark(pause, pause.getCommandIndex(), PAUSED));
        } else {
            markCommand(null);
        }
    }

    private void addPausedActions(WorkflowRun workflowRun, WorkflowRunPause pause) {
        currentWorkflowStatus.add(
            createStatusAction("step", VaadinIcon.STEP_FORWARD, Key.KEY_N, () -> resume(workflowRun, pause, WorkflowRunPause.Action.STEP, false),
                ButtonVariant.PRIMARY),
            createStatusAction("continue", VaadinIcon.FAST_FORWARD, Key.KEY_G,
                () -> resume(workflowRun, pause, WorkflowRunPause.Action.CONTINUE, false)),
            createStatusAction("from-cursor", VaadinIcon.CURSOR, Key.KEY_H,
                () -> resume(workflowRun, pause, WorkflowRunPause.Action.CONTINUE, true)),
            createStatusAction("stop", VaadinIcon.STOP, null, () -> stop(workflowRun, pause), ButtonVariant.ERROR));
    }

    /**
     * A run which is over can be started again from one of its commands, as long as it has a debugger: it is then a run
     * of this page, kept with everything it held.
     */
    private void addEndedActions(WorkflowRun workflowRun) {
        if (workflowRun.getDebugger() == null) {
            return;
        }
        findFailedCommandIndex(workflowRun).ifPresent(index -> currentWorkflowStatus.add(createStatusAction("from-command",
            getTranslation("workflow-development.from-command.button", index + 1), VaadinIcon.ROTATE_RIGHT, null, () -> runFrom(workflowRun, index),
            ButtonVariant.PRIMARY)));
        currentWorkflowStatus.add(createStatusAction("from-cursor", VaadinIcon.CURSOR, Key.KEY_H, () -> runFrom(workflowRun, null)));
    }

    /**
     * @return the command the run did not get past: the one which failed, or the one it was paused before when it was
     *     stopped. It is nothing for a run which passed, or which failed at one of its final commands.
     */
    private Optional<Integer> findFailedCommandIndex(WorkflowRun workflowRun) {
        final var index = workflowRun.getLastSuccessfulIndex() + 1;
        return ofNullable(workflowRun.getFailureMessage())
            .flatMap(_ -> ofNullable(workflowRun.getScript()))
            .filter(script -> index < script.getCommands().size())
            .map(_ -> index);
    }

    private Button createStatusAction(String key, VaadinIcon icon, Key shortcut, Runnable action, ButtonVariant... variants) {
        return createStatusAction(key, getTranslation("workflow-development." + key + ".button"), icon, shortcut, action, variants);
    }

    /**
     * @param key what the button does, which names its translations.
     * @param shortcut the key which clicks the button together with Ctrl and Alt, or nothing.
     */
    private Button createStatusAction(String key, String text, VaadinIcon icon, Key shortcut, Runnable action, ButtonVariant... variants) {
        final var button = new Button(text, icon.create());
        button.addThemeVariants(ButtonVariant.SMALL);
        button.addThemeVariants(variants);
        button.addClassName("workflow-status-action");
        button.setTooltipText(getTranslation("workflow-development." + key + ".button.tooltip"));
        button.addClickListener(_ -> {
            // the buttons come back with the status the run answers with
            statusActions.forEach(statusAction -> statusAction.setEnabled(false));
            action.run();
        });
        if (shortcut != null) {
            // Ctrl + Alt is AltGr on Windows: without this, the editor would also type what the keyboard has behind the key
            button.addClickShortcut(shortcut, KeyModifier.CONTROL, KeyModifier.ALT).setBrowserDefaultAllowed(false);
        }
        statusActions.add(button);
        return button;
    }

    private void setWorkflowStatus(String status) {
        currentWorkflowStatus.removeClassNames("error", "running", "success", PAUSED);
        currentWorkflowStatus.addClassName(status);
    }

    private Div createStatusMessage(String text) {
        final var message = new Div(new Span(text));
        message.addClassName("workflow-status-message");
        return message;
    }

    private Button createApprovalButton(WorkflowRun workflowRun, WorkflowRunApproval approval) {
        final var button = new Button(getTranslation("workflow-development.approval.button"), VaadinIcon.USER_CHECK.create());
        button.addThemeVariants(ButtonVariant.PRIMARY, ButtonVariant.SMALL);
        button.addClassName("workflow-status-action");
        button.addClickListener(_ -> new WorkflowApprovalDialog(workflowRun, approval,
            applicationManager.getBeanOfType(WorkflowRunService.class)).open());
        return button;
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
        saveCopyWorkflowButton.setVisible(!readOnly);
        // a run is debugged in the editor, by whoever can read what it holds: a read-only link hides both
        debugWorkflowButton.setVisible(!readOnly);
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

    /**
     * The editor as it is in the browser. Rows are 0-based.
     *
     * @param cursor the row of the cursor, or a row no editor has when it is not known.
     * @param breakpoints the rows of the breakpoints, each of them on a line of the command it stands for.
     */
    private record EditorState(String value, int cursor, List<Integer> breakpoints) {

    }

    /**
     * @param owner what the mark is drawn for: the pause of a run, or the failure of one. Another pause before the same
     *     command is another mark, as the definition may have been edited in between.
     * @param kind how the command is marked: as the one the run is paused before, or as the one it failed at.
     */
    private record CommandMark(Object owner, int commandIndex, String kind) {

    }

}
