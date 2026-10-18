package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.shared.Tooltip;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.router.RouterLink;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowReportFormat;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.util.LangUtil;
import org.myworkflows.view.WorkflowRunView;
import org.myworkflows.view.WorkflowTemplateView;
import org.myworkflows.view.component.html.StandardPaginatedGrid;

import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * The reports, newest first. Choosing a row shows its report next to the grid.
 *
 * @author Mihai Surdeanu
 * @since 2.3
 */
public final class WorkflowReportGrid extends Composite<VerticalLayout> {

    private static final int SHORT_ID_LENGTH = 8;

    private final StandardPaginatedGrid<WorkflowReport, ?> paginatedGrid = new StandardPaginatedGrid<>();

    private final Function<WorkflowReport, Optional<WorkflowTemplate>> templateFinder;

    public WorkflowReportGrid(Function<WorkflowReport, Optional<WorkflowTemplate>> templateFinder, Consumer<WorkflowReport> onSelect) {
        this.templateFinder = templateFinder;
        paginatedGrid.asSingleSelect().addValueChangeListener(event -> {
            // a report which was deleted, or left the page, is not chosen anymore, but what is shown stays until another is
            if (event.getValue() != null) {
                onSelect.accept(event.getValue());
            }
        });
    }

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    /**
     * Reads the page on display again, for reports which were written or deleted without the user asking for it.
     */
    public void refreshCurrentPage() {
        paginatedGrid.refreshCurrentPage();
    }

    public void setDataProvider(DataProvider<WorkflowReport, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    public void select(WorkflowReport workflowReport) {
        paginatedGrid.asSingleSelect().setValue(workflowReport);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();
        layout.setPadding(false);

        final var narrowColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderPhoneCell))
            .setHeader(getTranslation("workflow-reports.grid.report.column"));
        final var reportColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderReport))
            .setHeader(getTranslation("workflow-reports.grid.report.column"))
            .setWidth("180px")
            .setFlexGrow(2);
        final var templateColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderTemplate))
            .setHeader(getTranslation("workflow-reports.grid.template.column"))
            .setWidth("120px")
            .setFlexGrow(1);
        // the grid shares its row with the report, so the columns which never change much get the width they need
        final var formatColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderFormat))
            .setHeader(getTranslation("workflow-reports.grid.format.column"))
            .setWidth("110px")
            .setFlexGrow(0);
        final var sizeColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderSize))
            .setHeader(getTranslation("workflow-reports.grid.size.column"))
            .setWidth("90px")
            .setFlexGrow(0);
        paginatedGrid.setNarrowColumn(narrowColumn, List.of(reportColumn, templateColumn, formatColumn, sizeColumn));

        layout.add(paginatedGrid);
        return layout;
    }

    /**
     * A phone shows the whole report in one cell: its title and run, then its template, format and size.
     */
    private Component renderPhoneCell(WorkflowReport workflowReport) {
        final var badges = new HorizontalLayout(renderTemplate(workflowReport), renderFormat(workflowReport), renderSize(workflowReport));
        badges.addClassName("cell-badges");
        badges.setAlignItems(FlexComponent.Alignment.CENTER);
        final var layout = new VerticalLayout(renderReport(workflowReport), badges);
        layout.addClassName("phone-cell");
        layout.setPadding(false);
        layout.setSpacing(false);
        return layout;
    }

    private Component renderReport(WorkflowReport workflowReport) {
        final var title = new Span(workflowReport.title());
        title.addClassName("workflow-report-title");

        final var details = new HorizontalLayout(renderRunLink(workflowReport), new Span(getTranslation("pretty.time.format", workflowReport.created())));
        details.addClassName("cell-badges");
        details.addClassName("cell-secondary");
        details.setAlignItems(FlexComponent.Alignment.CENTER);

        final var layout = new VerticalLayout(title, details);
        layout.setPadding(false);
        layout.setSpacing(false);
        return layout;
    }

    private Component renderTemplate(WorkflowReport workflowReport) {
        return renderTemplateBadge(templateFinder.apply(workflowReport), this::getTranslation);
    }

    private Component renderFormat(WorkflowReport workflowReport) {
        return renderFormatBadge(workflowReport.format(), this::getTranslation);
    }

    private Component renderSize(WorkflowReport workflowReport) {
        final var size = new Span(LangUtil.describeSize(workflowReport.size()));
        size.setTitle(getTranslation("workflow-reports.grid.size.tooltip", workflowReport.size()));
        return size;
    }

    private Component renderRunLink(WorkflowReport workflowReport) {
        return renderRunLink(workflowReport, this::getTranslation);
    }

    /**
     * The run of a report, as the runs page writes its id: {@code #3f9c1a7e}, a link to the run.
     */
    static Component renderRunLink(WorkflowReport workflowReport, Function<String, String> translator) {
        final var runId = workflowReport.workflowRunId().toString();
        final var routerLink = new RouterLink("#" + runId.substring(0, SHORT_ID_LENGTH), WorkflowRunView.class, runId);
        routerLink.addClassName("workflow-run-id");
        Tooltip.forComponent(routerLink).setText(translator.apply("workflow-reports.grid.run.tooltip") + " " + runId);
        return routerLink;
    }

    static Component renderTemplateBadge(Optional<WorkflowTemplate> workflowTemplate, Function<String, String> translator) {
        return workflowTemplate.map(template -> {
            final var routerLink = new RouterLink(template.getName(), WorkflowTemplateView.class, template.getId());
            routerLink.getElement().getThemeList().add("badge small");
            return (Component) routerLink;
        }).orElseGet(() -> {
            final var badge = new Badge(translator.apply("workflow-reports.grid.template.manual"));
            badge.addClassName("transfer-neutral");
            badge.addThemeVariants(BadgeVariant.SMALL);
            return badge;
        });
    }

    static Component renderFormatBadge(WorkflowReportFormat format, Function<String, String> translator) {
        final var badge = new Badge(translator.apply("workflow-reports.format." + format.getLabel()));
        badge.addThemeVariants(BadgeVariant.SMALL);
        if (format == WorkflowReportFormat.HTML) {
            badge.addClassName("workflow-report-html");
        }
        return badge;
    }

}
