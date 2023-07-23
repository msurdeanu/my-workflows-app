package org.myworkflows.layout;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.applayout.AppLayout;
import com.vaadin.flow.component.applayout.DrawerToggle;
import com.vaadin.flow.component.html.Footer;
import com.vaadin.flow.component.html.H2;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.html.Image;
import com.vaadin.flow.component.html.Nav;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.router.RouterLink;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.view.LoginView;
import org.myworkflows.view.TestScenarioView;
import org.myworkflows.view.WorkflowSubmitView;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public class BaseLayout extends AppLayout {

    private static final boolean IS_OPTIMIZED_FOR_MOBILE = true;

    public BaseLayout() {
        setPrimarySection(Section.DRAWER);

        final var logo = new Image(getTranslation("app.logo.src"), getTranslation("app.logo.alt"));
        logo.setHeight("44px");
        addToNavbar(IS_OPTIMIZED_FOR_MOBILE, new DrawerToggle(), logo);

        addToDrawer(createDrawerContent(List.of(
            createRouterLink("Login", "la la-file", LoginView.class),
            createRouterLink("Workflow Submit", "la la-file", WorkflowSubmitView.class),
            createRouterLink("Test scenario", "la la-file", TestScenarioView.class)
        )));
    }

    private RouterLink createRouterLink(final String label, final String icon, final Class<? extends Component> target) {
        final var routerLink = new RouterLink();
        routerLink.addClassNames("flex", "mx-s", "p-s", "relative", "text-secondary");
        routerLink.setRoute(target);

        final var span = new Span();
        span.addClassNames("me-s", "text-l");
        if (StringUtils.isNotEmpty(icon)) {
            span.addClassNames(icon);
        }

        final var text = new Span(routerLink.getTranslation(label));
        text.addClassNames("font-medium", "text-s");

        routerLink.add(span, text);
        return routerLink;
    }

    private Component createDrawerContent(final List<RouterLink> routerLinks) {
        final var appName = new H2(getTranslation("app.name"));
        appName.addClassNames("flex", "items-center", "h-xl", "m-0", "px-m", "text-m");

        final var mainMenu = new H3(getTranslation("menu.main"));
        mainMenu.addClassNames("flex", "h-m", "items-center", "mx-m", "my-0", "text-s", "text-tertiary");

        final var section = new com.vaadin.flow.component.html.Section(appName, mainMenu, createNavigation(routerLinks), createFooter());
        section.addClassNames("flex", "flex-col", "items-stretch", "max-h-full", "min-h-full");
        return section;
    }

    private Nav createNavigation(final List<RouterLink> routerLinks) {
        final var nav = new Nav();
        nav.addClassNames("border-b", "border-contrast-10", "flex-grow", "overflow-auto");

        routerLinks.forEach(nav::add);
        return nav;
    }

    private Footer createFooter() {
        final var layout = new Footer();
        layout.addClassNames("flex", "items-center", "my-s", "px-m", "py-xs");
        layout.add(new Span("v1.0"));
        return layout;
    }

}
