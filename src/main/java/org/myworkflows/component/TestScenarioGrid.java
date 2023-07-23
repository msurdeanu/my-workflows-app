package org.myworkflows.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.TestScenario;
import org.myworkflows.domain.TestScenarioEventHandler;
import org.myworkflows.domain.TestScenarioType;
import org.vaadin.klaudeta.PaginatedGrid;

import static com.vaadin.flow.component.Shortcuts.addShortcutListener;
import static org.apache.commons.lang3.StringUtils.abbreviate;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class TestScenarioGrid extends Composite<VerticalLayout> {

    private final PaginatedGrid<TestScenario, ?> paginatedGrid = new PaginatedGrid<>();

    private final TestScenarioEventHandler testScenarioEventHandler;

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(final DataProvider<TestScenario, ?> dataProvider) {
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
            .setHeader(getTranslation("test-scenario.main-grid.name.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderLastRun))
            .setHeader(getTranslation("test-scenario.main-grid.last-run.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderCronExpression))
            .setHeader(getTranslation("test-scenario.main-grid.cron-expression.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActions))
            .setHeader(getTranslation("test-scenario.main-grid.actions.column"))
            .setAutoWidth(true);
        paginatedGrid.setPageSize(10);
        paginatedGrid.setPaginatorSize(5);
        paginatedGrid.addThemeVariants(GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);
        layout.add(paginatedGrid);

        return layout;
    }

    private Component renderIsEnabled(final TestScenario testScenario) {
        final var toggleButton = new Checkbox(testScenario.isEnabled());
        toggleButton.addValueChangeListener(event -> testScenarioEventHandler.onActivationChanged(testScenario));
        return toggleButton;
    }

    private Component renderName(final TestScenario testScenario) {
        final var layout = new HorizontalLayout();
        if (!testScenario.isEditable()) {
            final var name = new Label(abbreviate(testScenario.getName(), 64));
            name.addClassName(getClassName(testScenario));
            layout.add(name);
            return layout;
        }

        final var nameTextField = new TextField();
        nameTextField.addClassName("editable-field");
        nameTextField.setSuffixComponent(VaadinIcon.ENTER.create());
        nameTextField.setValue(testScenario.getName());
        layout.add(nameTextField);

        addShortcutListener(layout, () -> {
            testScenarioEventHandler.onNameChanged(testScenario, nameTextField.getValue());
            onTestScenarioUpdated(testScenario);
        }, Key.ENTER);
        addShortcutListener(layout, () -> onTestScenarioCancelled(testScenario), Key.ESCAPE);
        return layout;
    }

    private Component renderLastRun(final TestScenario testScenario) {
        final var lastRunButton = new Button("-");
        return lastRunButton;
    }

    private Component renderCronExpression(final TestScenario testScenario) {
        if (!testScenario.isEditable()) {
            return new Label(testScenario.getCron());
        }

        final var textField = new TextField();
        textField.addClassName("editable-field");
        textField.setSuffixComponent(VaadinIcon.ENTER.create());
        textField.setValue(testScenario.getCron());

        addShortcutListener(textField, () -> {
            testScenarioEventHandler.onCronExpressionChanged(testScenario, textField.getValue());
            onTestScenarioUpdated(testScenario);
        }, Key.ENTER);
        addShortcutListener(textField, () -> onTestScenarioCancelled(testScenario), Key.ESCAPE);
        return textField;
    }

    private Component renderActions(final TestScenario testScenario) {
        final var layout = new HorizontalLayout();
        return layout;
    }

    private void onTestScenarioToEdit(final TestScenario testScenario) {
        testScenario.toggleOnEditing();
        paginatedGrid.getDataProvider().refreshItem(testScenario);
    }

    private void onTestScenarioUpdated(final TestScenario testScenario) {
        testScenario.setEditable(false);
        paginatedGrid.getDataProvider().refreshItem(testScenario);
    }

    private void onTestScenarioCancelled(final TestScenario testScenario) {
        testScenario.setEditable(false);
        paginatedGrid.getDataProvider().refreshItem(testScenario);
    }

    private String getClassName(final TestScenario testScenario) {
        return TestScenarioType.DISABLED.getLabel();
    }


}
