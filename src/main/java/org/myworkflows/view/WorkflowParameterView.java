package org.myworkflows.view;

import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.RolesAllowed;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.handler.WorkflowParameterEventHandler;
import org.myworkflows.domain.filter.WorkflowParameterFilter;
import org.myworkflows.service.WorkflowParameterService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.PageFilter;
import org.myworkflows.view.component.PageGuide;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowParameterGrid;

import java.util.concurrent.TimeUnit;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@RolesAllowed("ROLE_ADMIN")
@Route(value = WorkflowParameterView.ROUTE, layout = BaseLayout.class)
public class WorkflowParameterView extends ResponsiveLayout implements HasDynamicTitle, WorkflowParameterEventHandler {

    public static final String ROUTE = "workflow/params";

    private final WorkflowParameterFilter workflowParameterFilter = new WorkflowParameterFilter();

    private final WorkflowParameterGrid workflowParameterGrid;

    private final WorkflowParameterService workflowParameterService;

    public WorkflowParameterView(WorkflowParameterService workflowParameterService) {
        super();
        this.workflowParameterService = workflowParameterService;

        final ConfigurableFilterDataProvider<WorkflowParameter, Void, WorkflowParameterFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(workflowParameterService::findBy, workflowParameterService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowParameterFilter);

        workflowParameterGrid = new WorkflowParameterGrid(this);
        workflowParameterGrid.setDataProvider(configurableFilterDataProvider);

        final var filterByName = createFilterByName();
        add(createHeader(getTranslation("workflow-params.page.title"), filterByName));
        add(createFilters(ROUTE,
            PageFilter.text("name", filterByName, value -> getTranslation("workflow-params.filter.chip.name", value))));
        add(createContent(workflowParameterGrid));
        add(createGuide());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-params"));
    }

    @Override
    public WorkflowParameter onCreate(String name) {
        final var workflowParameter = workflowParameterService.create(WorkflowParameter.of(name), true);
        workflowParameterGrid.refreshPage();
        return workflowParameter;
    }

    @Override
    public void onDelete(WorkflowParameter workflowParameter) {
        workflowParameterService.delete(workflowParameter);
        workflowParameterGrid.refreshPage();
    }

    @Override
    public void onUpdate(WorkflowParameter workflowParameter) {
        workflowParameterService.update(workflowParameter);
    }

    private PageGuide createGuide() {
        return createGuide("workflow-params")
            .legend("value", PageGuide.badge(getTranslation("workflow-params.grid.value.column")))
            .legend("edit", PageGuide.button(VaadinIcon.EDIT))
            .legend("save", PageGuide.button(VaadinIcon.CHECK, ButtonVariant.SUCCESS))
            .legend("cancel", PageGuide.button(VaadinIcon.CLOSE, ButtonVariant.ERROR))
            .legend("delete", PageGuide.button(VaadinIcon.TRASH, ButtonVariant.ERROR))
            .steps(3)
            .note("formats", PageGuide.fields(
                "true", getTranslation("workflow-parameter.type.b"),
                "42", getTranslation("workflow-parameter.type.i"),
                "3.14", getTranslation("workflow-parameter.type.f"),
                "2026-01-31", getTranslation("workflow-parameter.type.d"),
                "14:30", getTranslation("workflow-parameter.type.t"),
                "red,green,blue", getTranslation("workflow-parameter.type.ss")))
            .note("template")
            .note("debug");
    }

    private TextField createFilterByName() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("workflow-params.filter.by-name.placeholder"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addClassName("header-filter");
        filterByNameTextField.addValueChangeListener(event -> onFilterByName(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilterByName(String value) {
        workflowParameterFilter.nameCriteria(value);
        workflowParameterGrid.refreshPage();
    }

}
