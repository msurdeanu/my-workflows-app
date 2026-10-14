package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H4;
import com.vaadin.flow.component.html.ListItem;
import com.vaadin.flow.component.html.NativeTable;
import com.vaadin.flow.component.html.NativeTableBody;
import com.vaadin.flow.component.html.NativeTableRow;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.html.UnorderedList;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.radiobutton.RadioButtonGroup;
import com.vaadin.flow.component.radiobutton.RadioGroupVariant;
import com.vaadin.flow.component.select.Select;
import com.vaadin.flow.component.select.SelectVariant;
import com.vaadin.flow.component.textfield.Autocomplete;
import com.vaadin.flow.component.textfield.PasswordField;
import com.vaadin.flow.component.textfield.TextFieldVariant;
import com.vaadin.flow.component.upload.Upload;
import com.vaadin.flow.router.QueryParameters;
import com.vaadin.flow.router.RouterLink;
import com.vaadin.flow.server.streams.UploadHandler;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.transfer.WorkflowBundle;
import org.myworkflows.domain.transfer.WorkflowImportItem;
import org.myworkflows.domain.transfer.WorkflowImportItem.Action;
import org.myworkflows.domain.transfer.WorkflowImportItem.State;
import org.myworkflows.domain.transfer.WorkflowImportNote;
import org.myworkflows.domain.transfer.WorkflowImportNote.Severity;
import org.myworkflows.domain.transfer.WorkflowImportProblem;
import org.myworkflows.domain.transfer.WorkflowImportRequest;
import org.myworkflows.domain.transfer.WorkflowImportResult;
import org.myworkflows.domain.transfer.WorkflowImportResult.Change;
import org.myworkflows.domain.transfer.WorkflowImportResult.Outcome;
import org.myworkflows.domain.transfer.WorkflowImportResult.Todo;
import org.myworkflows.domain.transfer.WorkflowImportReview;
import org.myworkflows.domain.transfer.WorkflowTransferKind;
import org.myworkflows.exception.WorkflowTransferException;
import org.myworkflows.exception.WorkflowTransferException.Reason;
import org.myworkflows.service.WorkflowTransferService;
import org.myworkflows.util.LangUtil;
import org.myworkflows.view.LibraryView;
import org.myworkflows.view.WorkflowDevelopmentView;
import org.myworkflows.view.WorkflowParameterView;
import org.myworkflows.view.WorkflowPlaceholderView;
import org.myworkflows.view.WorkflowTemplateView;

import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;
import static org.springframework.web.util.HtmlUtils.htmlEscape;

/**
 * The tab which imports, in three steps: a file is chosen, then compared with what is here - an item of the file is
 * new, different or the same, and each one which is not the same is either imported or left alone - and at last what
 * was done is listed, with what it leaves to do. Nothing is saved before the second step is confirmed.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Slf4j
public final class WorkflowImportPanel extends Div {

    private static final int MAX_FILE_SIZE = 16 * 1024 * 1024;

    private static final int ERROR_NOTIFICATION_DURATION = 10_000;

    // a value is shown to tell it from another one, which its first characters do
    private static final int MAX_VALUE_LENGTH = 120;

    private static final int COLUMNS = 4;

    private static final String SEPARATOR = " · ";

    // the date a file was written on, where it is read
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").withZone(ZoneId.systemDefault());

    // what was chosen and typed for the items of the file, by the key of the item
    private final Map<String, Select<Action>> actionSelects = new LinkedHashMap<>();
    private final Map<String, PasswordField> valueFields = new HashMap<>();
    private final Map<NativeTableRow, String> rowNames = new HashMap<>();

    private final RadioButtonGroup<Action> existingGroup = new RadioButtonGroup<>(getTranslation("transfer.import.existing.label"));
    private final Checkbox pauseCheckbox = new Checkbox(getTranslation("transfer.import.pause.label"), true);
    private final Div problems = new Div();
    private final Div summary = new Div();
    private final Button importButton = new Button(VaadinIcon.CHECK.create(), _ -> onImport());

    private final WorkflowTransferService workflowTransferService;

    private String fileName;

    private WorkflowImportReview review;

    private String nameFilter = StringUtils.EMPTY;

    // set while the choices are changed by the page itself, which is no choice of the user
    private boolean refreshing;

    public WorkflowImportPanel(WorkflowTransferService workflowTransferService) {
        this.workflowTransferService = workflowTransferService;
        addClassName("transfer-panel");

        existingGroup.setItems(Action.values());
        existingGroup.setItemLabelGenerator(action -> getTranslation("transfer.import.existing." + action.name().toLowerCase()));
        existingGroup.addThemeVariants(RadioGroupVariant.AURA_HORIZONTAL);
        existingGroup.addValueChangeListener(event -> onExistingChanged(event.getValue()));
        pauseCheckbox.addValueChangeListener(_ -> refreshOutcome());
        problems.addClassNames("workflow-status", "error");
        importButton.addThemeVariants(ButtonVariant.PRIMARY);
        showUpload(null);
    }

    /**
     * @param filter what the name of an item of the file has to contain to be shown, whatever its case. An item which
     *     is not shown is imported like the other ones.
     */
    public void setNameFilter(String filter) {
        nameFilter = StringUtils.defaultString(filter).strip();
        rowNames.forEach((row, name) -> row.setVisible(StringUtils.containsIgnoreCase(name, nameFilter)));
    }

    private void showUpload(String errorMessage) {
        review = null;
        removeAll();
        final var intro = new Div(getTranslation("transfer.import.intro"));
        intro.addClassName("profile-text");
        add(intro, createUpload());
        if (errorMessage != null) {
            final var error = new Div(errorMessage);
            error.addClassNames("workflow-status", "error");
            add(error);
        }
    }

    private Component createUpload() {
        // the file is read once it is here as a whole, by the page: nothing of it is kept on the server
        final var upload = new Upload(UploadHandler.inMemory((metadata, data) -> onUploaded(metadata.fileName(), data)));
        upload.setAcceptedFileTypes(".yaml", ".yml", ".zip");
        upload.setMaxFiles(1);
        upload.setMaxFileSize(MAX_FILE_SIZE);
        upload.setUploadButton(new Button(getTranslation("transfer.import.upload.button"), VaadinIcon.UPLOAD.create()));
        upload.setDropLabel(new Span(getTranslation("transfer.import.upload.drop")));
        upload.addFileRejectedListener(event -> showUpload(event.getErrorMessage()));
        return upload;
    }

    private void onUploaded(String uploadedFileName, byte[] data) {
        try {
            showReview(uploadedFileName, workflowTransferService.read(data));
        } catch (WorkflowTransferException exception) {
            log.info("File '{}' cannot be imported: {}", uploadedFileName, exception.getReason());
            showUpload(translate(exception));
        }
    }

    private void showReview(String reviewedFileName, WorkflowImportReview reviewedFile) {
        fileName = reviewedFileName;
        review = reviewedFile;
        actionSelects.clear();
        valueFields.clear();
        rowNames.clear();
        removeAll();

        add(createFile());
        if (!review.problems().isEmpty()) {
            problems.removeAll();
            problems.add(createProblems("transfer.import.problems.file", review.problems()));
            problems.setVisible(true);
            add(problems);
        } else if (review.items().isEmpty()) {
            final var empty = new Div(getTranslation("transfer.import.empty"));
            empty.addClassName("profile-text");
            add(empty);
        } else {
            refreshing = true;
            existingGroup.setValue(Action.IMPORT);
            refreshing = false;
            add(createOptions(), createCounts(), createTable(), problems, createFooter());
            setNameFilter(nameFilter);
            onDecisionChanged();
        }
    }

    /**
     * @return the file which is looked at: its name, what it holds and where it comes from, as far as it says.
     */
    private Component createFile() {
        final var bundle = review.bundle();
        final var facts = new ArrayList<String>();
        facts.add(count("item", bundle.size()));
        ofNullable(bundle.exported()).ifPresent(origin -> addFacts(facts, origin));
        final var name = new Div(fileName);
        name.addClassName("transfer-file-name");
        final var text = new Div(String.join(SEPARATOR, facts));
        text.addClassName("profile-text");
        final var description = new Div(name, text);
        description.addClassName("transfer-file-facts");

        final var icon = new Span(VaadinIcon.FILE_TEXT_O.create());
        icon.addClassName("page-guide-icon");
        icon.getElement().setAttribute("aria-hidden", "true");
        final var anotherButton = new Button(getTranslation("transfer.import.another.button"), VaadinIcon.UPLOAD.create(), _ -> showUpload(null));
        final var file = new Div(icon, description, anotherButton);
        file.addClassName("transfer-file");
        return file;
    }

    private void addFacts(List<String> facts, WorkflowBundle.Origin origin) {
        if (origin.at() != null && StringUtils.isNotBlank(origin.by())) {
            facts.add(getTranslation("transfer.import.file.exported-by", DATE_FORMATTER.format(origin.at()), origin.by()));
        } else if (origin.at() != null) {
            facts.add(getTranslation("transfer.import.file.exported", DATE_FORMATTER.format(origin.at())));
        }
        if (StringUtils.isNotBlank(origin.version())) {
            facts.add(getTranslation("transfer.import.file.version", origin.version()));
        }
    }

    private Component createOptions() {
        final var existing = new Div(existingGroup, createText("transfer.import.existing.text"));
        existing.addClassName("transfer-option");
        final var pause = new Div(pauseCheckbox, createText("transfer.import.pause.text"));
        pause.addClassName("transfer-option");
        final var options = new Div(existing, pause);
        options.addClassName("transfer-options");
        return options;
    }

    private Component createText(String key) {
        final var text = new Div(getTranslation(key));
        text.addClassName("profile-text");
        return text;
    }

    private Component createCounts() {
        final var counts = new Div(new H4(getTranslation("transfer.import.counts.title")),
            createStateBadge(State.NEW, getTranslation("transfer.import.counts.new", review.count(State.NEW))),
            createStateBadge(State.DIFFERENT, getTranslation("transfer.import.counts.different", review.count(State.DIFFERENT))),
            createStateBadge(State.SAME, getTranslation("transfer.import.counts.same", review.count(State.SAME))));
        counts.addClassName("transfer-counts");
        final var needingLook = review.countNeedingLook();
        if (needingLook > 0) {
            final var look = new Span(VaadinIcon.WARNING.create(),
                new Span(getTranslation(needingLook == 1 ? "transfer.import.counts.look.one" : "transfer.import.counts.look.many", needingLook)));
            look.addClassName("transfer-look");
            counts.add(look);
        }
        return counts;
    }

    private Badge createStateBadge(State state, String text) {
        final var badge = new Badge(text);
        badge.addThemeVariants(BadgeVariant.SMALL);
        if (state == State.DIFFERENT) {
            badge.addThemeVariants(BadgeVariant.WARNING);
        } else if (state == State.SAME) {
            badge.addClassName("transfer-neutral");
        }
        return badge;
    }

    /**
     * @return the items of the file, kind by kind. A table which does not fit the screen is scrolled sideways.
     */
    private Component createTable() {
        final var table = new NativeTable();
        final var header = table.getHead().addRow();
        Stream.of("name", "state", "action", "notes").forEach(column -> header.addHeaderCell().setText(getTranslation("transfer.import.table." + column)));
        for (var kind : WorkflowTransferKind.values()) {
            final var items = review.items().stream().filter(item -> item.kind() == kind).toList();
            if (!items.isEmpty()) {
                final var groupRow = table.getBody().addRow();
                groupRow.addClassName("transfer-group");
                final var groupCell = groupRow.addHeaderCell();
                // a header cell has no method for what a data cell has one
                groupCell.getElement().setAttribute("colspan", String.valueOf(COLUMNS));
                groupCell.setText(getTranslation("transfer.kind." + kind.name().toLowerCase() + ".title"));
                items.forEach(item -> addRow(table.getBody(), item));
            }
        }
        final var box = new Div(table);
        box.addClassName("transfer-table");
        return box;
    }

    private void addRow(NativeTableBody body, WorkflowImportItem item) {
        final var row = body.addRow();
        final var name = new Div(new Badge(item.name()));
        name.addClassName("transfer-name");
        if (item.template() != null) {
            final var owner = new Span(getTranslation("transfer.export.only-for", item.template()));
            owner.addClassName("cell-secondary");
            name.add(owner);
        }
        row.addDataCell().add(name);
        row.addDataCell().add(createStateBadge(item.state(), getTranslation("transfer.import.state." + item.state().name().toLowerCase())));
        row.addDataCell().add(createAction(item));
        final var notes = new Div();
        notes.addClassName("transfer-notes");
        item.notes().forEach(note -> {
            notes.add(createNote(note));
            // the value goes under what says it is missing, where it is read before it is typed
            if (note.type() == WorkflowImportNote.Type.VALUE_MISSING) {
                notes.add(createValueField(item));
            }
        });
        row.addDataCell().add(notes);
        rowNames.put(row, item.name());
    }

    private Component createAction(WorkflowImportItem item) {
        if (item.state() == State.SAME) {
            return createText("transfer.import.action.same");
        }

        final var select = new Select<Action>();
        select.setItems(Action.values());
        select.setItemLabelGenerator(action -> getTranslation("transfer.import.action." + item.state().name().toLowerCase() + "."
            + action.name().toLowerCase()));
        select.setValue(Action.IMPORT);
        select.addThemeVariants(SelectVariant.SMALL);
        select.setAriaLabel(getTranslation("transfer.import.action.label", item.name()));
        select.addClassName("transfer-action");
        select.addValueChangeListener(_ -> onDecisionChanged());
        actionSelects.put(item.key(), select);
        return select;
    }

    private Component createNote(WorkflowImportNote note) {
        final var text = new Span();
        // what comes from the file - a name, a value - is escaped on its way into the markup
        text.getElement().setProperty("innerHTML", describe(note));
        final var line = new Div();
        line.addClassName("transfer-note");
        final var severity = note.type().getSeverity();
        if (severity != Severity.INFO) {
            line.addClassName(severity == Severity.ERROR ? "error" : "warning");
            line.add(VaadinIcon.WARNING.create());
        }
        line.add(text);
        return line;
    }

    private String describe(WorkflowImportNote note) {
        final var key = "transfer.import.note." + note.type().name().toLowerCase().replace('_', '-');
        final var arguments = note.arguments();
        return switch (note.type()) {
            case CONTENT -> getTranslation(key, countOrNone(WorkflowTransferKind.DEFINITION, Integer.parseInt(arguments.get(0))),
                countOrNone(WorkflowTransferKind.PARAMETER, Integer.parseInt(arguments.get(1))));
            case TYPE, TYPE_CHANGE -> getTranslation(key, arguments.stream().map(this::describeType).toArray());
            default -> getTranslation(key, arguments.stream().map(this::escape).toArray());
        };
    }

    private String describeType(String code) {
        return StringUtils.isEmpty(code) ? getTranslation("transfer.import.none") : htmlEscape(getTranslation("workflow-parameter.type." + code));
    }

    private String escape(String value) {
        return StringUtils.isBlank(value) ? getTranslation("transfer.import.none") : htmlEscape(StringUtils.abbreviate(value, MAX_VALUE_LENGTH));
    }

    /**
     * @return where the value of a new item is typed when the file left it out, which it does for a secret.
     */
    private Component createValueField(WorkflowImportItem item) {
        final var field = new PasswordField();
        field.setPlaceholder(getTranslation("transfer.import.value.placeholder"));
        field.setAriaLabel(getTranslation("transfer.import.value.label", item.name()));
        field.setAutocomplete(Autocomplete.NEW_PASSWORD);
        field.addThemeVariants(TextFieldVariant.SMALL);
        field.addClassName("transfer-value");
        field.addValueChangeListener(_ -> refreshOutcome());
        valueFields.put(item.key(), field);
        return field;
    }

    private Component createFooter() {
        final var text = new Div(summary, createText("transfer.import.footer.text"));
        text.addClassName("transfer-footer-text");
        final var cancelButton = new Button(getTranslation("transfer.import.cancel.button"), _ -> showUpload(null));
        cancelButton.addThemeVariants(ButtonVariant.TERTIARY);
        final var footer = new Div(text, cancelButton, importButton);
        footer.addClassName("transfer-footer");
        return footer;
    }

    /**
     * What is chosen for the items which are already here applies to each of them, until one is given a choice of
     * its own.
     */
    private void onExistingChanged(Action action) {
        if (refreshing || review == null || action == null) {
            return;
        }
        refreshing = true;
        try {
            review.items().stream()
                .filter(item -> item.state() == State.DIFFERENT)
                .forEach(item -> ofNullable(actionSelects.get(item.key())).ifPresent(select -> select.setValue(action)));
        } finally {
            refreshing = false;
        }
        onDecisionChanged();
    }

    private void onDecisionChanged() {
        if (refreshing) {
            return;
        }
        refreshing = true;
        try {
            // a new parameter of a single template has nowhere to be without its template, so it follows it
            review.items().stream().filter(item -> item.template() != null && item.state() == State.NEW).forEach(item -> {
                final var templateSelect = actionSelects.get(WorkflowImportItem.keyOf(WorkflowTransferKind.TEMPLATE, item.template(), null));
                final var followed = templateSelect != null && templateSelect.getValue() == Action.IMPORT;
                final var select = actionSelects.get(item.key());
                if (select.isEnabled() != followed) {
                    select.setEnabled(followed);
                    select.setValue(followed ? Action.IMPORT : Action.SKIP);
                }
            });
        } finally {
            refreshing = false;
        }
        refreshOutcome();
    }

    /**
     * Says what the choices come down to, and whether they can be applied.
     */
    private void refreshOutcome() {
        if (review == null || review.items().isEmpty()) {
            return;
        }
        final var request = createRequest();
        final var imported = review.items().stream().filter(item -> review.actionOf(item, request) == Action.IMPORT).toList();
        final var created = imported.stream().filter(item -> item.state() == State.NEW).count();
        summary.setText(getTranslation("transfer.import.summary", created, imported.size() - created, review.items().size() - imported.size()));

        final var currentProblems = workflowTransferService.check(review, request);
        problems.removeAll();
        problems.setVisible(!currentProblems.isEmpty());
        if (!currentProblems.isEmpty()) {
            problems.add(createProblems("transfer.import.problems.choices", currentProblems));
        }
        importButton.setText(imported.isEmpty() ? getTranslation("transfer.import.button.empty")
            : getTranslation("transfer.import.button", count("item", imported.size())));
        importButton.setEnabled(!imported.isEmpty() && currentProblems.isEmpty());
    }

    private WorkflowImportRequest createRequest() {
        final var actions = new HashMap<String, Action>();
        actionSelects.forEach((key, select) -> actions.put(key, ofNullable(select.getValue()).orElse(Action.SKIP)));
        final var values = new HashMap<String, String>();
        valueFields.forEach((key, field) -> values.put(key, field.getValue()));
        return new WorkflowImportRequest(actions, values, pauseCheckbox.getValue());
    }

    private Component createProblems(String titleKey, List<WorkflowImportProblem> importProblems) {
        final var list = new UnorderedList();
        importProblems.forEach(importProblem -> list.add(new ListItem(getTranslation(
            "transfer.import.problem." + importProblem.type().name().toLowerCase().replace('_', '-'),
            getTranslation("transfer.kind." + importProblem.kind().name().toLowerCase()), importProblem.name(),
            StringUtils.defaultString(importProblem.detail())))));
        return new Div(new Span(getTranslation(titleKey)), list);
    }

    private void onImport() {
        try {
            showResult(workflowTransferService.apply(review, createRequest()));
        } catch (WorkflowTransferException exception) {
            showError(translate(exception));
            if (exception.getReason() == Reason.INSTANCE_CHANGED) {
                // the choices were made on what is no longer true, so they are made again
                showReview(fileName, workflowTransferService.review(review.bundle()));
            }
        } catch (RuntimeException exception) {
            log.warn("File '{}' could not be imported.", fileName, exception);
            showError(getTranslation("transfer.import.failed.message", StringUtils.defaultString(exception.getMessage())));
        }
    }

    private void showResult(WorkflowImportResult result) {
        review = null;
        removeAll();
        final var done = new Div(getTranslation("transfer.import.done.message", count("item", result.countImported()), fileName));
        done.addClassNames("workflow-status", "success");
        add(done);

        if (!result.todos().isEmpty()) {
            final var todos = new Div();
            todos.addClassName("transfer-list");
            result.todos().forEach(todo -> todos.add(createTodo(todo)));
            add(new H4(getTranslation("transfer.import.todos.title")), todos);
        }

        final var outcomes = new Div();
        outcomes.addClassName("transfer-list");
        Stream.of(Change.values()).forEach(change -> {
            final var changed = result.findOutcomes(change);
            if (!changed.isEmpty()) {
                final var title = new Span(getTranslation("transfer.import.change." + change.name().toLowerCase(), changed.size()));
                title.addClassName("transfer-outcome-title");
                final var row = new Div(title);
                row.addClassName("transfer-outcome");
                changed.forEach(outcome -> row.add(createLink(outcome, nameOf(outcome))));
                outcomes.add(row);
            }
        });
        final var anotherButton = new Button(getTranslation("transfer.import.again.button"), VaadinIcon.UPLOAD.create(), _ -> showUpload(null));
        add(new H4(getTranslation("transfer.import.outcomes.title")), outcomes, new Div(anotherButton));
    }

    private Component createTodo(Todo todo) {
        final var key = "transfer.import.todo." + todo.type().name().toLowerCase().replace('_', '-');
        final var title = new Div(getTranslation(key + ".title", nameOf(todo.outcome())));
        title.addClassName("page-guide-item-title");
        final var text = new Div(getTranslation(key + ".text", StringUtils.defaultString(todo.detail())));
        text.addClassName("profile-text");
        final var description = new Div(title, text);
        description.addClassName("transfer-file-facts");

        final Component link;
        if (todo.type() == Todo.Type.UNKNOWN_COMMANDS) {
            link = new RouterLink(getTranslation("menu.main.libs"), LibraryView.class);
        } else {
            link = createLink(todo.outcome(), getTranslation("transfer.kind." + todo.outcome().kind().name().toLowerCase() + ".title"));
        }
        final var row = new Div(VaadinIcon.WARNING.create(), description, link);
        row.addClassName("transfer-todo");
        return row;
    }

    /**
     * @return the name of an item as it is told from the other ones: a parameter of a single template with its template.
     */
    private String nameOf(Outcome outcome) {
        return outcome.template() == null ? outcome.name() : getTranslation("transfer.import.own-name", outcome.name(), outcome.template());
    }

    /**
     * @return a link to where the item is by now, or its name alone when it is nowhere: it was not imported.
     */
    private Component createLink(Outcome outcome, String text) {
        if (outcome.target() == null) {
            final var badge = new Badge(text);
            badge.addClassName("transfer-neutral");
            return badge;
        }
        final var link = switch (outcome.kind()) {
            case TEMPLATE -> new RouterLink(text, WorkflowTemplateView.class, Integer.valueOf(outcome.target()));
            case DEFINITION -> new RouterLink(text, WorkflowDevelopmentView.class, Integer.valueOf(outcome.target()));
            case PARAMETER -> createFilteredLink(new RouterLink(text, WorkflowParameterView.class), outcome.target());
            case PLACEHOLDER -> createFilteredLink(new RouterLink(text, WorkflowPlaceholderView.class), outcome.target());
        };
        link.getElement().getThemeList().add("badge");
        return link;
    }

    /**
     * @return the link to a page which lists its items by name, filtered down to the given one.
     */
    private RouterLink createFilteredLink(RouterLink link, String name) {
        link.setQueryParameters(QueryParameters.simple(Map.of("name", name)));
        return link;
    }

    private String count(String word, long count) {
        return countOrNone(getTranslation("transfer.kind." + word), count);
    }

    private String countOrNone(WorkflowTransferKind kind, long count) {
        return countOrNone(getTranslation("transfer.kind." + kind.name().toLowerCase()), count);
    }

    private String countOrNone(String word, long count) {
        return LangUtil.pluralize(word, count).orElseGet(() -> getTranslation("transfer.count.none", word));
    }

    private String translate(WorkflowTransferException exception) {
        return getTranslation(exception.getReason().getTranslationKey(), exception.getArguments());
    }

    private void showError(String message) {
        Notification.show(message, ERROR_NOTIFICATION_DURATION, Notification.Position.BOTTOM_START)
            .addThemeVariants(NotificationVariant.ERROR);
    }

}
