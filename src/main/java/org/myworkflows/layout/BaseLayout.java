package org.myworkflows.layout;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.applayout.AppLayout;
import com.vaadin.flow.component.applayout.DrawerToggle;
import com.vaadin.flow.component.avatar.Avatar;
import com.vaadin.flow.component.contextmenu.ContextMenu;
import com.vaadin.flow.component.html.Footer;
import com.vaadin.flow.component.html.H1;
import com.vaadin.flow.component.html.Header;
import com.vaadin.flow.component.html.Image;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.Scroller;
import com.vaadin.flow.component.sidenav.SideNav;
import com.vaadin.flow.component.sidenav.SideNavItem;
import com.vaadin.flow.spring.security.AuthenticationContext;
import com.vaadin.flow.theme.lumo.LumoUtility;
import org.myworkflows.view.LoginView;
import org.myworkflows.view.StatisticView;
import org.myworkflows.view.WorkflowDefinitionDevView;
import org.myworkflows.view.WorkflowTemplatesView;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public class BaseLayout extends AppLayout {

    public BaseLayout(final AuthenticationContext authContext) {
        createHeader(authContext);
        createDrawer(createNavItems(authContext.isAuthenticated()));
    }

    private void createHeader(final AuthenticationContext authContext) {
        final var logoLayout = new HorizontalLayout();
        final var logo = new Image(getTranslation("app.logo.src"), getTranslation("app.logo.alt"));
        logo.setHeight("44px");
        logoLayout.add(logo);

        var header = new HorizontalLayout(new DrawerToggle(), logoLayout);
        authContext.getAuthenticatedUser(UserDetails.class)
                .map(user -> {
                    final var avatar = new Avatar(user.getUsername());
                    avatar.setTooltipEnabled(true);
                    final var contextMenu = new ContextMenu(avatar);
                    contextMenu.setOpenOnClick(true);
                    contextMenu.addItem(getTranslation("menu.main.logout"), event -> authContext.logout());
                    return avatar;
                })
                .ifPresent(header::add);
        header.setDefaultVerticalComponentAlignment(FlexComponent.Alignment.CENTER);
        header.expand(logoLayout);
        header.setWidthFull();

        addToNavbar(header);
    }

    private void createDrawer(final List<SideNavItem> routerLinks) {
        final var appName = new H1(getTranslation("app.name"));
        appName.addClassNames(LumoUtility.FontSize.LARGE, LumoUtility.Margin.NONE);
        final var header = new Header(appName);
        final var scroller = new Scroller(createNavigation(routerLinks));
        addToDrawer(header, scroller, createFooter());
    }

    private SideNav createNavigation(final List<SideNavItem> sideNavItems) {
        final var appNav = new SideNav();
        sideNavItems.forEach(appNav::addItem);
        return appNav;
    }

    private Component createFooter() {
        final var layout = new Footer();
        layout.addClassNames("flex", "items-center", "my-s", "px-m", "py-xs");
        layout.add(new Span("v1.0")); // TODO: Use version from settings table
        return layout;
    }

    private List<SideNavItem> createNavItems(final boolean isAuthenticated) {
        final var sideNavItems = new ArrayList<SideNavItem>();
        sideNavItems.add(new SideNavItem(getTranslation("menu.main.workflow-templates"), WorkflowTemplatesView.class, VaadinIcon.LIST.create()));
        sideNavItems.add(new SideNavItem(getTranslation("menu.main.workflow-development"), WorkflowDefinitionDevView.class, VaadinIcon.CODE.create()));
        sideNavItems.add(new SideNavItem(getTranslation("menu.main.statistics"), StatisticView.class, VaadinIcon.CHART.create()));

        if (!isAuthenticated) {
            sideNavItems.add(new SideNavItem(getTranslation("menu.main.login"), LoginView.class, VaadinIcon.SIGN_IN.create()));
        }

        return sideNavItems;
    }

}
