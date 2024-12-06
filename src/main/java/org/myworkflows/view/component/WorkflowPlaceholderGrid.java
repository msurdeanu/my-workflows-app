package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.handler.WorkflowPlaceholderEventHandler;
import org.myworkflows.util.PlaceholderUtil;
import org.myworkflows.view.component.html.StandardPaginatedGrid;
import org.myworkflows.view.component.html.TextFieldWithEnterShortcut;

import static java.lang.String.valueOf;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowPlaceholderGrid extends Composite<VerticalLayout> {

    private final StandardPaginatedGrid<WorkflowPlaceholder, ?> paginatedGrid = new StandardPaginatedGrid<>();

    private final WorkflowPlaceholderEventHandler workflowPlaceholderEventHandler;

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(DataProvider<WorkflowPlaceholder, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("workflow-placeholders.grid.name.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderValue))
            .setHeader(getTranslation("workflow-placeholders.grid.value.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActions))
            .setHeader(new TextFieldWithEnterShortcut(workflowPlaceholderEventHandler::onCreate)
                .allowedCharPatternAndPlaceholder(PlaceholderUtil.PLACEHOLDER_NAME_PATTERN).small())
            .setAutoWidth(true);

        layout.add(paginatedGrid);
        return layout;
    }

    private Component renderName(WorkflowPlaceholder workflowPlaceholder) {
        return new Span(valueOf(workflowPlaceholder.getName()));
    }

    private Component renderValue(WorkflowPlaceholder workflowPlaceholder) {
        return new Span(valueOf(workflowPlaceholder.getValue()));
    }

    private Component renderActions(WorkflowPlaceholder workflowPlaceholder) {
        final var layout = new HorizontalLayout();

        final var deleteButton = new Button(VaadinIcon.TRASH.create());
        deleteButton.setTooltipText(getTranslation("workflow-placeholders.grid.actions.button.delete.title"));
        deleteButton.addThemeVariants(ButtonVariant.LUMO_SMALL, ButtonVariant.LUMO_ERROR);

        deleteButton.addClickListener(event -> new DeleteConfirmDialog(workflowPlaceholder.getName(),
            item -> workflowPlaceholderEventHandler.onDelete(workflowPlaceholder)).open());

        layout.add(deleteButton);
        return layout;
    }

}
