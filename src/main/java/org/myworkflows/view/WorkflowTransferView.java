package org.myworkflows.view;

import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.tabs.Tab;
import com.vaadin.flow.component.tabs.Tabs;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.RolesAllowed;
import org.myworkflows.service.WorkflowTransferService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.PageFilter;
import org.myworkflows.view.component.PageGuide;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowExportPanel;
import org.myworkflows.view.component.WorkflowImportPanel;

import java.util.concurrent.TimeUnit;

/**
 * Moves templates, definitions, parameters and placeholders between instances as a file: one tab writes the file, the
 * other one reads it. Both are for admins only, as a file can hold every value there is and an import can replace it.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@RolesAllowed("ROLE_ADMIN")
@Route(value = WorkflowTransferView.ROUTE, layout = BaseLayout.class)
public class WorkflowTransferView extends ResponsiveLayout implements HasDynamicTitle {

    public static final String ROUTE = "workflow/transfer";

    private final WorkflowExportPanel exportPanel;

    private final WorkflowImportPanel importPanel;

    public WorkflowTransferView(WorkflowTransferService workflowTransferService) {
        super();
        exportPanel = new WorkflowExportPanel(workflowTransferService);
        importPanel = new WorkflowImportPanel(workflowTransferService);
        importPanel.setVisible(false);

        final var exportTab = new Tab(getTranslation("transfer.export.tab"));
        final var tabs = new Tabs(exportTab, new Tab(getTranslation("transfer.import.tab")));
        tabs.addSelectedChangeListener(event -> {
            final var exporting = event.getSelectedTab() == exportTab;
            if (exporting) {
                // what an import has just brought can be exported at once
                exportPanel.refresh();
            }
            exportPanel.setVisible(exporting);
            importPanel.setVisible(!exporting);
        });
        final var body = new Div(tabs, exportPanel, importPanel);
        body.addClassName("transfer");

        final var filterByName = createFilterByName();
        add(createHeader(getTranslation("transfer.page.title"), filterByName));
        add(createFilters(ROUTE,
            PageFilter.text("name", filterByName, value -> getTranslation("transfer.filter.chip.name", value))));
        add(createContent(body));
        add(createGuide());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.transfer"));
    }

    private PageGuide createGuide() {
        return createGuide("transfer")
            .legendTitle("transfer.guide.legend.title")
            .legend("needed", createNeededSample())
            .legend("left-out", createNeutralBadge(getTranslation("transfer.export.left-out.badge")))
            .legend("new", PageGuide.badge(getTranslation("transfer.import.state.new")))
            .legend("different", PageGuide.badge(getTranslation("transfer.import.state.different"), BadgeVariant.WARNING))
            .legend("same", createNeutralBadge(getTranslation("transfer.import.state.same")))
            .steps(4)
            .note("names")
            .note("own")
            .note("secrets")
            .note("paused")
            .note("whole")
            .note("commands");
    }

    /**
     * @return a badge as the page draws what needs no attention: grey, which no variant of a badge is in Aura.
     */
    private Badge createNeutralBadge(String text) {
        final var badge = PageGuide.badge(text);
        badge.addClassName("transfer-neutral");
        return badge;
    }

    private Checkbox createNeededSample() {
        final var sample = new Checkbox(true);
        sample.setEnabled(false);
        return sample;
    }

    private TextField createFilterByName() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("transfer.filter.by-name.placeholder"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addClassName("header-filter");
        filterByNameTextField.addValueChangeListener(event -> onFilterByName(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilterByName(String value) {
        exportPanel.setNameFilter(value);
        importPanel.setNameFilter(value);
    }

}
