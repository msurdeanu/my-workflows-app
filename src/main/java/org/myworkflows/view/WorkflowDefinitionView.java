package org.myworkflows.view;

import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.html.Paragraph;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.PermitAll;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.UserRole;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.handler.WorkflowDefinitionEventHandler;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.PageFilter;
import org.myworkflows.view.component.PageGuide;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowDefinitionGrid;
import org.myworkflows.view.component.WorkflowDuplicateDialog;

import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.apache.commons.lang3.math.NumberUtils.toInt;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@PermitAll
@Route(value = WorkflowDefinitionView.ROUTE, layout = BaseLayout.class)
public class WorkflowDefinitionView extends ResponsiveLayout implements HasDynamicTitle, HasUrlParameter<Integer>, WorkflowDefinitionEventHandler {

    public static final String ROUTE = "workflow/definitions";

    private final WorkflowDefinitionFilter workflowDefinitionFilter = new WorkflowDefinitionFilter();

    private final WorkflowDefinitionGrid workflowDefinitionGrid;

    private final WorkflowDefinitionService workflowDefinitionService;

    public WorkflowDefinitionView(WorkflowDefinitionService workflowDefinitionService) {
        super();
        this.workflowDefinitionService = workflowDefinitionService;

        final ConfigurableFilterDataProvider<WorkflowDefinition, Void, WorkflowDefinitionFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(workflowDefinitionService::findBy, workflowDefinitionService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowDefinitionFilter);

        workflowDefinitionGrid = new WorkflowDefinitionGrid(this);
        workflowDefinitionGrid.setDataProvider(configurableFilterDataProvider);

        final var filterByName = createFilterByName();
        add(createHeader(getTranslation("workflow-definitions.page.title"), filterByName));
        // the id has no field: it comes from a link to one definition, and its chip is the way back to all of them
        add(createFilters(ROUTE,
            PageFilter.text("name", filterByName, value -> getTranslation("workflow-definitions.filter.chip.name", value)),
            new PageFilter("id", null,
                () -> workflowDefinitionFilter.idCriteria() > 0 ? String.valueOf(workflowDefinitionFilter.idCriteria()) : StringUtils.EMPTY,
                value -> onFilterById(toInt(value)), value -> getTranslation("workflow-definitions.filter.chip.id", value))));
        add(createContent(workflowDefinitionGrid));
        add(createGuide("workflow-definitions")
            .legend("name", PageGuide.link(getTranslation("workflow-definitions.grid.name.column")))
            .legend("edit", PageGuide.button(VaadinIcon.EDIT), UserRole.ADMIN)
            .legend("duplicate", PageGuide.button(VaadinIcon.COPY_O), UserRole.ADMIN)
            .legend("delete", PageGuide.button(VaadinIcon.TRASH, ButtonVariant.ERROR), UserRole.ADMIN)
            .steps(4)
            .note("used")
            .note("rename")
            .note("placeholders"));
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-definitions"));
    }

    /**
     * Kept for the links pointing to {@code workflow/definitions/42}.
     */
    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter Integer id) {
        setPathFilterValues(id != null ? Map.of("id", String.valueOf(id)) : Map.of());
    }

    @Override
    public void onCreate(String name) {
        workflowDefinitionService.create(WorkflowDefinition.of(name), true);
        workflowDefinitionGrid.refreshPage();
    }

    @Override
    public void onDelete(WorkflowDefinition workflowDefinition) {
        if (workflowDefinitionService.delete(workflowDefinition) > 0) {
            workflowDefinitionGrid.refreshPage();
        }
    }

    @Override
    public void onDuplicate(WorkflowDefinition workflowDefinition) {
        final var openCheckbox = new Checkbox(getTranslation("workflow-definitions.duplicate.open.label"), true);
        final var dialog = new WorkflowDuplicateDialog(workflowDefinition.getName(), workflowDefinitionService.suggestCopyName(workflowDefinition),
            workflowDefinitionService::isNameTaken, name -> {
            final var copy = workflowDefinitionService.duplicate(workflowDefinition, name);
            Notification.show(getTranslation("workflow-definitions.duplicate.success.message", copy.getName()));
            getUI().ifPresent(ui -> {
                if (openCheckbox.getValue()) {
                    ui.navigate(WorkflowDevelopmentView.class, copy.getId());
                } else {
                    // shown alone, as the link to one definition shows it
                    ui.navigate(WorkflowDefinitionView.class, copy.getId());
                }
            });
        });
        dialog.addDetails(new Paragraph(getTranslation("workflow-definitions.duplicate.help", workflowDefinition.getName())), openCheckbox);
        dialog.open();
    }

    @Override
    public void onUpdate(WorkflowDefinition workflowDefinition, String newName) {
        workflowDefinitionService.updateName(workflowDefinition, newName);
    }

    private TextField createFilterByName() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("workflow-definitions.filter.by-name.placeholder"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addClassName("header-filter");
        filterByNameTextField.addValueChangeListener(event -> onFilterByName(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilterByName(String value) {
        workflowDefinitionFilter.nameCriteria(value);
        workflowDefinitionGrid.refreshPage();
    }

    private void onFilterById(int value) {
        workflowDefinitionFilter.idCriteria(value);
        workflowDefinitionGrid.refreshPage();
    }

}
