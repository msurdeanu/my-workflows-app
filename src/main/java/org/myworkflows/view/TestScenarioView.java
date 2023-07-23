package org.myworkflows.view;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.select.Select;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.server.auth.AnonymousAllowed;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.component.TestScenarioGrid;
import org.myworkflows.domain.TestScenario;
import org.myworkflows.domain.TestScenarioEventHandler;
import org.myworkflows.domain.TestScenarioFilter;
import org.myworkflows.domain.TestScenarioType;
import org.myworkflows.layout.BaseLayout;
import org.myworkflows.layout.ResponsiveLayout;
import org.myworkflows.service.TestScenarioService;

import java.util.concurrent.TimeUnit;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@AnonymousAllowed
@Route(value = TestScenarioView.ROUTE, layout = BaseLayout.class)
public class TestScenarioView extends ResponsiveLayout implements HasDynamicTitle, TestScenarioEventHandler {

    public static final String ROUTE = "test-scenarios";

    private final TestScenarioGrid testScenarioGrid;

    private final TestScenarioFilter testScenarioFilter = new TestScenarioFilter();

    private final TestScenarioService testScenarioService;

    public TestScenarioView(final TestScenarioService testScenarioService) {
        super();
        this.testScenarioService = testScenarioService;

        final ConfigurableFilterDataProvider<TestScenario, Void, TestScenarioFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(testScenarioService::findBy, testScenarioService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(testScenarioFilter);

        testScenarioGrid = new TestScenarioGrid(this);
        testScenarioGrid.setDataProvider(configurableFilterDataProvider);

        add(createHeader(getTranslation("test-scenario.page.subtitle"), createFilterByName(), createFilterByType()));
        add(createContent(testScenarioGrid));
        add(createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.test-scenarios"));
    }

    @Override
    public void onActivationChanged(final TestScenario testScenario) {
        testScenarioService.changeActivation(testScenario);
        testScenarioGrid.refreshPage();

        Notification.show("Activation status for test scenario '" + testScenario.getName() + "' is changed successfully.");
    }

    @Override
    public void onCronExpressionChanged(final TestScenario testScenario, @NonNull final String newCronExpression) {
        if (testScenarioService.changeCronExpression(testScenario, newCronExpression)) {
            Notification.show("Cron expression changed successfully to '" + newCronExpression + "'.");
        }
    }

    @Override
    public void onDelete(final TestScenario testScenario) {
        testScenarioService.delete(testScenario);
        testScenarioGrid.refreshPage();
    }

    @Override
    public void onNameChanged(final TestScenario testScenario, @NonNull final String newName) {
        if (testScenarioService.changeName(testScenario, newName)) {
            Notification.show("Test scenario name changed successfully to '" + newName + "'.");
        }
    }

    private Component createFilterByName() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("test-scenario.main-grid.filter.by-name.placeholder"));
        filterByNameTextField.setHelperText(getTranslation("test-scenario.main-grid.filter.by-name.helper"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addValueChangeListener(event -> onFilteringByName(event.getValue()));
        return filterByNameTextField;
    }

    private Component createFilterByType() {
        final var filterByTypeSelect = new Select<TestScenarioType>();
        filterByTypeSelect.setItems(TestScenarioType.getAllItems());
        filterByTypeSelect.setPlaceholder(getTranslation("test-scenario.main-grid.filter.by-type.placeholder"));
        filterByTypeSelect.setHelperText(getTranslation("test-scenario.main-grid.filter.by-type.helper"));
        filterByTypeSelect.setItemLabelGenerator(TestScenarioType::getLabel);
        filterByTypeSelect.setValue(TestScenarioType.ALL);
        filterByTypeSelect.addValueChangeListener(event -> onFilteringByType(event.getValue()));
        return filterByTypeSelect;
    }

    private void onFilteringByName(final String value) {
        testScenarioFilter.setByNameCriteria(value.toLowerCase());

        testScenarioGrid.refreshPage();
    }

    private void onFilteringByType(final TestScenarioType value) {
        testScenarioFilter.setByTypeCriteria(value);

        testScenarioGrid.refreshPage();
    }

}
