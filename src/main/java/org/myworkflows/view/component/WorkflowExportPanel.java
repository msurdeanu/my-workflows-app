package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H4;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.radiobutton.RadioButtonGroup;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.server.streams.DownloadEvent;
import com.vaadin.flow.server.streams.DownloadHandler;
import com.vaadin.flow.server.streams.DownloadResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.transfer.WorkflowBundleFormat;
import org.myworkflows.domain.transfer.WorkflowExportPlan;
import org.myworkflows.domain.transfer.WorkflowExportSelection;
import org.myworkflows.domain.transfer.WorkflowTransferContent;
import org.myworkflows.domain.transfer.WorkflowTransferKind;
import org.myworkflows.service.WorkflowTransferService;
import org.myworkflows.util.LangUtil;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toSet;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.definitionsOf;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.parametersOf;

/**
 * The tab which exports: what the instance holds, kind by kind, each item with a checkbox, then how the file is
 * written and the button which downloads it. What a picked item needs is ticked for it and cannot be unticked, and says
 * what needs it.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Slf4j
public final class WorkflowExportPanel extends Div {

    private static final int HTTP_CONFLICT = 409;

    private static final String SEPARATOR = " · ";

    private final Set<Integer> templateIds = new HashSet<>();
    private final Set<Integer> definitionIds = new HashSet<>();
    private final Set<String> parameterNames = new HashSet<>();
    private final Set<String> placeholderNames = new HashSet<>();

    private final Checkbox dependenciesCheckbox = new Checkbox(getTranslation("transfer.export.dependencies.label"), true);
    private final Checkbox secretsCheckbox = new Checkbox(getTranslation("transfer.export.secrets.label"), false);
    private final RadioButtonGroup<WorkflowBundleFormat> formatGroup = new RadioButtonGroup<>(getTranslation("transfer.export.format.label"));
    private final Div columns = new Div();
    private final Div summary = new Div();
    private final Div fileName = new Div();
    private final Div problem = new Div();
    private final Button exportButton = new Button(VaadinIcon.DOWNLOAD.create());
    private final Anchor exportAnchor = new Anchor(DownloadHandler.fromInputStream(this::download), StringUtils.EMPTY);
    private final List<Section> sections = new ArrayList<>();

    // what the file is made of once it is asked for, by a request which is not one of the page: it reads nothing else
    private final AtomicReference<Export> export = new AtomicReference<>();

    private final WorkflowTransferService workflowTransferService;

    private WorkflowTransferContent content;

    private String nameFilter = StringUtils.EMPTY;

    public WorkflowExportPanel(WorkflowTransferService workflowTransferService) {
        this.workflowTransferService = workflowTransferService;
        addClassName("transfer-panel");

        dependenciesCheckbox.addValueChangeListener(_ -> refreshPlan());
        secretsCheckbox.addValueChangeListener(_ -> refreshPlan());
        formatGroup.setItems(WorkflowBundleFormat.values());
        formatGroup.setRenderer(new ComponentRenderer<>(this::renderFormat));
        formatGroup.setValue(WorkflowBundleFormat.YAML);
        formatGroup.addValueChangeListener(_ -> refreshPlan());
        columns.addClassName("transfer-columns");

        add(createOption(dependenciesCheckbox, "transfer.export.dependencies.text"), columns, createOptions());
        refresh();
    }

    /**
     * Reads what the instance holds once more: what an import brought since is listed, and what is gone is no longer
     * picked.
     */
    public void refresh() {
        content = workflowTransferService.findContent();
        templateIds.retainAll(content.getTemplates().stream().map(WorkflowTemplate::getId).collect(toSet()));
        definitionIds.retainAll(content.getDefinitions().stream().map(definition -> definition.getId()).collect(toSet()));
        parameterNames.retainAll(content.getParameters().stream().map(parameter -> parameter.getName()).collect(toSet()));
        placeholderNames.retainAll(content.getPlaceholders().stream().map(placeholder -> placeholder.getName()).collect(toSet()));

        sections.clear();
        sections.addAll(List.of(createTemplateSection(), createDefinitionSection(), createParameterSection(), createPlaceholderSection()));
        columns.removeAll();
        columns.add(createColumn(sections.get(0), sections.get(1)), createColumn(sections.get(2), sections.get(3)));
        setNameFilter(nameFilter);
        refreshPlan();
    }

    /**
     * @param filter what the name of an item has to contain to be shown, whatever its case. An item which is not shown
     *     stays picked.
     */
    public void setNameFilter(String filter) {
        nameFilter = StringUtils.defaultString(filter).strip();
        sections.forEach(section -> section.rows.forEach(row -> row.root.setVisible(StringUtils.containsIgnoreCase(row.name, nameFilter))));
    }

    private Section createTemplateSection() {
        final var section = new Section(WorkflowTransferKind.TEMPLATE);
        content.getTemplates().forEach(template -> section.add(new Row(template.getName(), describe(template),
            exportPlan -> exportPlan.entryOf(template), _ -> false, picked -> pick(templateIds, template.getId(), picked))));
        return section;
    }

    private Section createDefinitionSection() {
        final var section = new Section(WorkflowTransferKind.DEFINITION);
        content.getDefinitions().forEach(definition -> section.add(new Row(definition.getName(), StringUtils.EMPTY,
            exportPlan -> exportPlan.entryOf(definition), _ -> false, picked -> pick(definitionIds, definition.getId(), picked))));
        return section;
    }

    private Section createParameterSection() {
        final var section = new Section(WorkflowTransferKind.PARAMETER);
        content.getParameters().forEach(parameter -> section.add(new Row(parameter.getName(),
            getTranslation("workflow-parameter.type." + parameter.getType().getValue()), exportPlan -> exportPlan.entryOf(parameter),
            exportPlan -> exportPlan.leavesOutValueOf(parameter), picked -> pick(parameterNames, parameter.getName(), picked))));
        return section;
    }

    private Section createPlaceholderSection() {
        final var section = new Section(WorkflowTransferKind.PLACEHOLDER);
        content.getPlaceholders().forEach(placeholder -> section.add(new Row(placeholder.getName(), StringUtils.EMPTY,
            exportPlan -> exportPlan.entryOf(placeholder), exportPlan -> exportPlan.leavesOutValueOf(placeholder),
            picked -> pick(placeholderNames, placeholder.getName(), picked))));
        return section;
    }

    private <T> void pick(Set<T> picks, T pick, boolean picked) {
        if (picked) {
            picks.add(pick);
        } else {
            picks.remove(pick);
        }
    }

    private String describe(WorkflowTemplate template) {
        return Stream.of(count(WorkflowTransferKind.DEFINITION, definitionsOf(template).size()), count(WorkflowTransferKind.PARAMETER,
                parametersOf(template).size()))
            .filter(StringUtils::isNotEmpty)
            .reduce((left, right) -> left + SEPARATOR + right)
            .orElse(StringUtils.EMPTY);
    }

    /**
     * @return what is said next to an item: what it is, the template it is meant for, what needs it.
     */
    private String describe(String detail, WorkflowExportPlan.Entry entry) {
        final var parts = new ArrayList<String>();
        if (StringUtils.isNotEmpty(detail)) {
            parts.add(detail);
        }
        if (entry.owner() != null) {
            parts.add(getTranslation("transfer.export.only-for", entry.owner()));
        }
        if (entry.neededBy().size() > 2) {
            parts.add(getTranslation("transfer.export.needed-by.more", entry.neededBy().getFirst(), entry.neededBy().size() - 1));
        } else if (!entry.neededBy().isEmpty()) {
            parts.add(getTranslation("transfer.export.needed-by", String.join(", ", entry.neededBy())));
        }
        return String.join(SEPARATOR, parts);
    }

    /**
     * @return the given number of items of a kind in words, like "2 definitions", and nothing for none.
     */
    private String count(WorkflowTransferKind kind, int count) {
        return LangUtil.pluralize(getTranslation("transfer.kind." + kind.name().toLowerCase()), count).orElse(StringUtils.EMPTY);
    }

    private void refreshPlan() {
        final var selection = new WorkflowExportSelection(templateIds, definitionIds, parameterNames, placeholderNames,
            dependenciesCheckbox.getValue(), secretsCheckbox.getValue());
        final var exportPlan = WorkflowExportPlan.of(selection, content);
        sections.forEach(section -> section.update(exportPlan));
        export.set(new Export(selection, formatGroup.getValue()));

        summary.setText(summarize(exportPlan));
        fileName.setText(workflowTransferService.createFileName(formatGroup.getValue()));
        fileName.setVisible(exportPlan.size() > 0);
        final var duplicateNames = Stream.of(WorkflowTransferKind.TEMPLATE, WorkflowTransferKind.DEFINITION)
            .flatMap(kind -> exportPlan.findDuplicateNames(kind).stream())
            .toList();
        problem.setText(getTranslation("transfer.error.duplicate-names", String.join(", ", duplicateNames)));
        problem.setVisible(!duplicateNames.isEmpty());
        exportButton.setText(exportPlan.size() > 0
            ? getTranslation("transfer.export.button", LangUtil.pluralize(getTranslation("transfer.kind.item"), exportPlan.size()).orElse(StringUtils.EMPTY))
            : getTranslation("transfer.export.button.empty"));
        exportButton.setEnabled(exportPlan.isExportable());
        exportAnchor.setEnabled(exportPlan.isExportable());
    }

    /**
     * @return how much is exported, kind by kind, and how many values stay out of the file.
     */
    private String summarize(WorkflowExportPlan exportPlan) {
        if (exportPlan.size() == 0) {
            return getTranslation("transfer.export.summary.empty");
        }
        final var counts = Stream.of(WorkflowTransferKind.values())
            .map(kind -> count(kind, exportPlan.count(kind)))
            .filter(StringUtils::isNotEmpty)
            .toList();
        final var text = getTranslation("transfer.export.summary", LangUtil.pluralize(getTranslation("transfer.kind.item"), exportPlan.size())
            .orElse(StringUtils.EMPTY), String.join(", ", counts));
        final var leftOutValues = exportPlan.countLeftOutValues();
        if (leftOutValues == 0) {
            return text;
        }
        return text + ' ' + getTranslation(leftOutValues == 1 ? "transfer.export.left-out.one" : "transfer.export.left-out.many", leftOutValues);
    }

    private Component createColumn(Section first, Section second) {
        final var column = new Div(first.root, second.root);
        column.addClassName("transfer-column");
        return column;
    }

    private Component createOptions() {
        summary.addClassName("transfer-summary");
        fileName.addClassName("transfer-file-name");
        problem.addClassName("text-error");
        exportButton.addThemeVariants(ButtonVariant.PRIMARY);
        exportAnchor.add(exportButton);
        final var readyTitle = new Span(getTranslation("transfer.export.ready.label"));
        readyTitle.addClassName("transfer-label");
        final var ready = new Div(readyTitle, summary, fileName, problem, exportAnchor);
        ready.addClassName("transfer-ready");

        final var secretsTitle = new Span(getTranslation("transfer.export.secrets.title"));
        secretsTitle.addClassName("transfer-label");
        final var secrets = new Div(secretsTitle, createOption(secretsCheckbox, "transfer.export.secrets.text"));
        secrets.addClassName("transfer-ready");

        final var options = new Div(new Div(formatGroup), secrets, ready);
        options.addClassName("transfer-options");
        return options;
    }

    /**
     * @return the checkbox above what it means, as a checkbox has no room for more than its label.
     */
    private Component createOption(Checkbox checkbox, String textKey) {
        final var text = new Div();
        text.addClassName("profile-text");
        // the text is a translation and names a piece of code, like the texts of a guide
        text.getElement().setProperty("innerHTML", getTranslation(textKey));
        final var option = new Div(checkbox, text);
        option.addClassName("transfer-option");
        return option;
    }

    private Component renderFormat(WorkflowBundleFormat format) {
        final var key = "transfer.export.format." + format.name().toLowerCase();
        final var title = new Div(getTranslation(key + ".title"));
        final var text = new Div(getTranslation(key + ".text"));
        text.addClassName("profile-text");
        return new Div(title, text);
    }

    /**
     * Runs on the request which downloads the file: it reads what was picked when the page was last drawn, and what
     * the instance holds by now.
     */
    private DownloadResponse download(DownloadEvent downloadEvent) {
        try {
            final var currentExport = export.get();
            final var file = workflowTransferService.export(currentExport.selection(), currentExport.format());
            return new DownloadResponse(new ByteArrayInputStream(file.content()), file.name(), file.contentType(), file.content().length);
        } catch (RuntimeException exception) {
            log.warn("An export could not be written for the request '{}'.", downloadEvent.getRequest().getPathInfo(), exception);
            return DownloadResponse.error(HTTP_CONFLICT);
        }
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    private record Export(WorkflowExportSelection selection, WorkflowBundleFormat format) {

    }

    /**
     * The items of one kind, under a header which counts the ones which are exported.
     *
     * @author Mihai Surdeanu
     * @since 2.1
     */
    private final class Section {

        private final Div root = new Div();
        private final Span count = new Span();
        private final Button pickAllButton = new Button();
        private final List<Row> rows = new ArrayList<>();

        private Section(WorkflowTransferKind kind) {
            root.addClassName("transfer-section");
            count.addClassName("profile-text");
            pickAllButton.addThemeVariants(ButtonVariant.TERTIARY, ButtonVariant.SMALL);
            pickAllButton.addClickListener(_ -> pickAll());
            final var header = new Div(new H4(getTranslation("transfer.kind." + kind.name().toLowerCase() + ".title")), count, pickAllButton);
            header.addClassName("transfer-section-header");
            root.add(header);
        }

        private void add(Row row) {
            rows.add(row);
            root.add(row.root);
        }

        private void update(WorkflowExportPlan exportPlan) {
            rows.forEach(row -> row.update(exportPlan));
            if (rows.isEmpty() && root.getComponentCount() == 1) {
                final var empty = new Div(getTranslation("transfer.export.section.empty"));
                empty.addClassNames("transfer-empty", "profile-text");
                root.add(empty);
            }
            count.setText(getTranslation("transfer.export.section.count", rows.stream().filter(row -> row.checkbox.getValue()).count(), rows.size()));
            count.setVisible(!rows.isEmpty());
            final var pickableRows = rows.stream().filter(row -> row.pickable).toList();
            pickAllButton.setVisible(!pickableRows.isEmpty());
            pickAllButton.setText(getTranslation(pickableRows.stream().allMatch(row -> row.checkbox.getValue())
                ? "transfer.export.section.pick-none" : "transfer.export.section.pick-all"));
        }

        /**
         * Picks every item which is shown and can be picked, or none of them when they all are.
         */
        private void pickAll() {
            final var pickableRows = rows.stream().filter(row -> row.pickable && row.root.isVisible()).toList();
            final var picked = pickableRows.stream().anyMatch(row -> !row.checkbox.getValue());
            pickableRows.forEach(row -> row.picker.accept(picked));
            refreshPlan();
        }

    }

    /**
     * An item with its checkbox. Whether it is ticked is told by the plan, not by the click alone: an item which
     * something else needs is ticked whatever was clicked.
     *
     * @author Mihai Surdeanu
     * @since 2.1
     */
    private final class Row {

        private final Checkbox checkbox = new Checkbox();
        private final Span info = new Span();
        private final Badge leftOutBadge = new Badge(getTranslation("transfer.export.left-out.badge"));
        private final Div root = new Div(checkbox, leftOutBadge, info);

        private final String name;
        private final String detail;
        private final Function<WorkflowExportPlan, WorkflowExportPlan.Entry> entryFinder;
        private final Predicate<WorkflowExportPlan> leftOutFinder;
        private final Consumer<Boolean> picker;

        private boolean pickable;

        private Row(String name, String detail, Function<WorkflowExportPlan, WorkflowExportPlan.Entry> entryFinder,
                    Predicate<WorkflowExportPlan> leftOutFinder, Consumer<Boolean> picker) {
            this.name = name;
            this.detail = detail;
            this.entryFinder = entryFinder;
            this.leftOutFinder = leftOutFinder;
            this.picker = picker;

            root.addClassName("transfer-row");
            info.addClassName("transfer-row-info");
            leftOutBadge.addThemeVariants(BadgeVariant.SMALL);
            leftOutBadge.addClassName("transfer-neutral");
            checkbox.setLabel(name);
            checkbox.addValueChangeListener(event -> {
                // a value set by the plan is no pick
                if (event.isFromClient()) {
                    picker.accept(event.getValue());
                    refreshPlan();
                }
            });
        }

        private void update(WorkflowExportPlan exportPlan) {
            final var entry = entryFinder.apply(exportPlan);
            pickable = !entry.locked();
            checkbox.setValue(entry.included());
            checkbox.setEnabled(pickable);
            info.setText(describe(detail, entry));
            leftOutBadge.setVisible(entry.included() && leftOutFinder.test(exportPlan));
        }

    }

}
