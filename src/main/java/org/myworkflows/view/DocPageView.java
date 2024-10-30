package org.myworkflows.view;

import com.flowingcode.vaadin.addons.markdown.BaseMarkdownComponent;
import com.flowingcode.vaadin.addons.markdown.MarkdownEditor;
import com.flowingcode.vaadin.addons.markdown.MarkdownViewer;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.tabs.Tab;
import com.vaadin.flow.component.tabs.Tabs;
import com.vaadin.flow.component.tabs.TabsVariant;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.QueryParameters;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.PermitAll;
import org.myworkflows.domain.DocPage;
import org.myworkflows.domain.DocPageEventHandler;
import org.myworkflows.domain.UserRole;
import org.myworkflows.service.DocPageService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.ResponsiveLayout;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@PermitAll
@Route(value = DocPageView.ROUTE, layout = BaseLayout.class)
public class DocPageView extends ResponsiveLayout implements HasDynamicTitle, HasUrlParameter<String>, DocPageEventHandler {

    public static final String ROUTE = "doc-pages";

    private static final String EDIT_MODE = "em";

    private final Tabs tabs = new Tabs();
    private final Map<String, Tab> tabMap = new HashMap<>();
    private final VerticalLayout tabContent = new VerticalLayout();

    private final boolean isLoggedAsAdmin = UserRole.ADMIN.validate();

    private final DocPageService docPageService;

    private boolean editable;

    public DocPageView(DocPageService docPageService) {
        this.docPageService = docPageService;

        docPageService.getAllNames().forEach(name -> {
            final var tab = new Tab(name);
            tabMap.put(name, tab);
            tabs.add(tab);
        });
        tabs.addThemeVariants(TabsVariant.LUMO_CENTERED);
        tabs.addSelectedChangeListener(event -> setTabContent(event.getSelectedTab()));
        setTabContent(tabs.getSelectedTab());

        add(createHeader(getTranslation("doc-pages.page.title"), createSwitchModeButton()),
            createContent(tabs, tabContent),
            createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("doc-pages.page.title"));
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String pageName) {
        ofNullable(pageName).flatMap(name -> ofNullable(tabMap.get(name))).ifPresent(tab -> {
            tabs.setSelectedTab(tab);
            setTabContent(tabs.getSelectedTab());
        });
        processEditModeIfPresent(beforeEvent.getLocation().getQueryParameters());
    }

    @Override
    public void onDelete(DocPage docPage) {
        docPageService.delete(docPage);
    }

    @Override
    public void onValueUpdated(DocPage docPage, String newValue) {
        docPageService.updateValue(docPage, newValue);
    }

    private void processEditModeIfPresent(QueryParameters queryParameters) {
        final var parameters = queryParameters.getParameters();
        if (parameters.isEmpty()) {
            return;
        }
        editable = ofNullable(parameters.get(EDIT_MODE)).isPresent();
        setTabContent(tabs.getSelectedTab());
    }

    private void setTabContent(Tab tab) {
        tabContent.removeAll();

        docPageService.findByName(tab.getLabel()).ifPresent(docPage -> {
            if (editable) {
                tabContent.add(createMarkdownEditor(docPage));
            } else {
                tabContent.add(createMarkdownViewer(docPage));
            }
        });
    }

    private Component createMarkdownEditor(DocPage docPage) {
        final var layout = new VerticalLayout();
        final var markdownEditor = new MarkdownEditor();
        markdownEditor.setWidthFull();
        markdownEditor.setHeight("400px");
        markdownEditor.setMaxLength(32768);
        markdownEditor.setDataColorMode(BaseMarkdownComponent.DataColorMode.LIGHT);
        markdownEditor.setContent(docPage.getValue());
        layout.add(markdownEditor);
        if (isLoggedAsAdmin) {
            final var horizontalLayout = new HorizontalLayout();
            horizontalLayout.setSpacing(true);
            horizontalLayout.setWidthFull();

            final var updateDocPageButton = new Button(getTranslation("doc-pages.update.button"), VaadinIcon.EDIT.create());
            updateDocPageButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
            updateDocPageButton.getStyle().set("flex", "1 1 50%");
            updateDocPageButton.addClickListener(event -> onValueUpdated(docPage, markdownEditor.getContent()));
            // TODO: use confirmation dialog for deletes
            final var deleteDocPageButton = new Button(getTranslation("doc-pages.delete.button"), VaadinIcon.TRASH.create());
            deleteDocPageButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
            deleteDocPageButton.getStyle().set("flex", "1 1 50%");
            deleteDocPageButton.addClickListener(event -> onDelete(docPage));

            horizontalLayout.add(updateDocPageButton, deleteDocPageButton);
            layout.add(horizontalLayout);
        }
        return layout;
    }

    private Component createMarkdownViewer(DocPage docPage) {
        final var markdownViewer = new MarkdownViewer();
        markdownViewer.setSizeFull();
        markdownViewer.setDataColorMode(BaseMarkdownComponent.DataColorMode.LIGHT);
        markdownViewer.setContent(docPage.getValue());
        return markdownViewer;
    }

    private Component createSwitchModeButton() {
        final var switchModeButton = new Button(VaadinIcon.EXCHANGE.create());
        switchModeButton.addClickListener(event -> {
            Map<String, List<String>> queryParams = getUI()
                .map(ui -> new HashMap<>(ui.getInternals().getActiveViewLocation().getQueryParameters().getParameters()))
                .orElse(new HashMap<>());
            if (queryParams.containsKey(EDIT_MODE)) {
                queryParams.remove(EDIT_MODE);
            } else {
                queryParams.put(EDIT_MODE, List.of(Boolean.TRUE.toString()));
            }
            getUI().ifPresent(ui -> {
                switchMode();
                ui.navigate(getClass(), new QueryParameters(queryParams));
            });
        });
        return switchModeButton;
    }

    private void switchMode() {
        editable = !editable;
        setTabContent(tabs.getSelectedTab());
    }

}
