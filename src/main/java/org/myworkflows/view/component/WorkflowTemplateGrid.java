package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.WorkflowTemplateEventHandler;
import org.vaadin.klaudeta.PaginatedGrid;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowTemplateGrid extends Composite<VerticalLayout> {

    private final PaginatedGrid<WorkflowTemplate, ?> paginatedGrid = new PaginatedGrid<>();

    private final WorkflowTemplateEventHandler workflowTemplateEventHandler;

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(final DataProvider<WorkflowTemplate, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();

        layout.setSizeFull();
        paginatedGrid.setAllRowsVisible(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderIsEnabled))
                .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
                .setHeader(getTranslation("workflow-templates.main-grid.name.column"))
                .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderCronExpression))
                .setHeader(getTranslation("workflow-templates.main-grid.cron-expression.column"))
                .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActions))
                .setHeader(getTranslation("workflow-templates.main-grid.actions.column"))
                .setAutoWidth(true);
        paginatedGrid.setItemDetailsRenderer(new ComponentRenderer<>(
                () -> new WorkflowTemplateDetails(workflowTemplateEventHandler),
                WorkflowTemplateDetails::setDetails)
        );
        paginatedGrid.setPageSize(10);
        paginatedGrid.setPaginatorSize(5);
        paginatedGrid.addThemeVariants(GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);
        layout.add(paginatedGrid);

        return layout;
    }

    private Component renderIsEnabled(final WorkflowTemplate workflowTemplate) {
        return new Checkbox(workflowTemplate.isEnabled());
    }

    private Component renderName(final WorkflowTemplate workflowTemplate) {
        return new Span(workflowTemplate.getName());
    }

    private Component renderCronExpression(final WorkflowTemplate workflowTemplate) {
        return new Span(workflowTemplate.getCron());
    }

    private Component renderActions(final WorkflowTemplate workflowTemplate) {
        final var layout = new HorizontalLayout();

        final var scheduleNowButton = new Button(new Icon(VaadinIcon.START_COG));
        scheduleNowButton.addThemeVariants(ButtonVariant.LUMO_ICON);
        scheduleNowButton.getElement().setProperty("title", getTranslation("workflow-templates.main-grid.actions.button.schedule.title"));
        scheduleNowButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        final var editButton = new Button(new Icon(VaadinIcon.EDIT));
        editButton.getElement().setProperty("title", getTranslation("workflow-templates.main-grid.actions.button.edit.title"));
        editButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        final var deleteButton = new Button();
        deleteButton.setIcon(new Icon(VaadinIcon.TRASH));
        deleteButton.getElement().setProperty("title", getTranslation("workflow-templates.main-grid.actions.button.delete.title"));
        deleteButton.addThemeVariants(ButtonVariant.LUMO_SMALL);

        // TODO

        layout.add(scheduleNowButton, editButton, deleteButton);
        return layout;
    }

}
