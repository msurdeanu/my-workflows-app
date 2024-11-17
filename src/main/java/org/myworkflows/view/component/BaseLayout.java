package org.myworkflows.view.component;

import com.vaadin.flow.component.applayout.AppLayout;
import com.vaadin.flow.component.applayout.DrawerToggle;
import com.vaadin.flow.component.avatar.Avatar;
import com.vaadin.flow.component.contextmenu.ContextMenu;
import com.vaadin.flow.component.html.Footer;
import com.vaadin.flow.component.html.H1;
import com.vaadin.flow.component.html.Header;
import com.vaadin.flow.component.html.Image;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.Scroller;
import com.vaadin.flow.component.sidenav.SideNav;
import com.vaadin.flow.component.sidenav.SideNavItem;
import com.vaadin.flow.spring.security.AuthenticationContext;
import com.vaadin.flow.theme.lumo.LumoUtility;
import org.myworkflows.config.BaseConfig;
import org.myworkflows.repository.MenuItemRepository;
import org.myworkflows.view.transformer.MenuItemsToSideNavItemsTransformer;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public class BaseLayout extends AppLayout {

    public BaseLayout(AuthenticationContext authContext, BaseConfig baseConfig, MenuItemRepository menuItemRepository) {
        createHeader(authContext, baseConfig);

        addDrawerContent(new MenuItemsToSideNavItemsTransformer().transform(menuItemRepository.findByOrderByPosition()), baseConfig);
    }

    private void createHeader(AuthenticationContext authContext, BaseConfig baseConfig) {
        final var logoLayout = new HorizontalLayout();
        final var logo = new Image(baseConfig.getLogoSrc(), baseConfig.getLogoAlt());
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

    private void addDrawerContent(List<SideNavItem> routerLinks, BaseConfig baseConfig) {
        final var appName = new H1(getTranslation("app.name"));
        appName.addClassNames(LumoUtility.FontSize.LARGE, LumoUtility.Margin.NONE);
        final var header = new Header(appName);
        final var scroller = new Scroller(createNavigation(routerLinks));
        addToDrawer(header, scroller, createFooter(baseConfig));
    }

    private SideNav createNavigation(List<SideNavItem> routerLinks) {
        final var appNav = new SideNav();
        routerLinks.forEach(appNav::addItem);
        return appNav;
    }

    private Footer createFooter(BaseConfig baseConfig) {
        final var layout = new Footer();
        layout.addClassNames("flex", "items-center", "my-s", "px-m", "py-xs");
        layout.add(new Span("v" + baseConfig.getVersion()));
        return layout;
    }

}
