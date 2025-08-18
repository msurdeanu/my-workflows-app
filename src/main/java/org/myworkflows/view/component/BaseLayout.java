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
import com.vaadin.flow.server.streams.DownloadHandler;
import com.vaadin.flow.spring.security.AuthenticationContext;
import com.vaadin.flow.theme.lumo.LumoUtility;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.repository.MenuItemRepository;
import org.myworkflows.view.transformer.MenuItemsToSideNavItemsTransformer;
import org.springframework.security.core.userdetails.UserDetails;

import java.io.File;
import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
public class BaseLayout extends AppLayout {

    public BaseLayout(AuthenticationContext authContext, SettingProvider settingProvider, MenuItemRepository menuItemRepository) {
        createHeader(authContext, settingProvider);

        addDrawerContent(new MenuItemsToSideNavItemsTransformer().transform(menuItemRepository.findByOrderByPosition()), settingProvider);
    }

    private void createHeader(AuthenticationContext authContext, SettingProvider settingProvider) {
        final var logoLayout = new HorizontalLayout();
        final var logo = loadLogoImage(settingProvider);
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

    private void addDrawerContent(List<SideNavItem> routerLinks, SettingProvider settingProvider) {
        final var appName = new H1(getTranslation("app.name"));
        appName.addClassNames(LumoUtility.FontSize.LARGE, LumoUtility.Margin.NONE);
        final var header = new Header(appName);
        final var scroller = new Scroller(createNavigation(routerLinks));
        addToDrawer(header, scroller, createFooter(settingProvider));
    }

    private SideNav createNavigation(List<SideNavItem> routerLinks) {
        final var appNav = new SideNav();
        routerLinks.forEach(appNav::addItem);
        return appNav;
    }

    private Footer createFooter(SettingProvider settingProvider) {
        final var layout = new Footer();
        layout.addClassNames("flex", "items-center", "my-s", "px-m", "py-xs");
        layout.add(new Span("v" + settingProvider.getOrDefault("version", "")));
        return layout;
    }

    private Image loadLogoImage(SettingProvider settingProvider) {
        final var logo = settingProvider.getOrDefault("logo", "/logo.png");
        final var downloadHandler = "/logo.png".equals(logo)
            ? DownloadHandler.forClassResource(getClass(), "/logo.png")
            : DownloadHandler.forFile(new File(logo));
        return new Image(downloadHandler, settingProvider.getOrDefault("name", "My Workflows"));
    }

}
