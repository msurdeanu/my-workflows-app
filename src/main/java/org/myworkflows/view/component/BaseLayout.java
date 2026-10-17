package org.myworkflows.view.component;

import com.vaadin.flow.component.applayout.AppLayout;
import com.vaadin.flow.component.applayout.DrawerToggle;
import com.vaadin.flow.component.avatar.Avatar;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.contextmenu.ContextMenu;
import com.vaadin.flow.component.fullscreen.Fullscreen;
import com.vaadin.flow.component.fullscreen.FullscreenState;
import com.vaadin.flow.component.html.Footer;
import com.vaadin.flow.component.html.H4;
import com.vaadin.flow.component.html.Header;
import com.vaadin.flow.component.html.Image;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.Scroller;
import com.vaadin.flow.component.sidenav.SideNav;
import com.vaadin.flow.component.sidenav.SideNavItem;
import com.vaadin.flow.server.streams.DownloadHandler;
import com.vaadin.flow.spring.security.AuthenticationContext;
import jakarta.annotation.security.PermitAll;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.repository.MenuItemRepository;
import org.myworkflows.service.GlobalSearchService;
import org.myworkflows.view.ProfileView;
import org.myworkflows.view.transformer.MenuItemsToSideNavItemsTransformer;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.info.BuildProperties;
import org.springframework.security.core.userdetails.UserDetails;

import java.io.File;
import java.util.List;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@PermitAll
public class BaseLayout extends AppLayout {

    private static final String UNKNOWN_VERSION = "dev";

    public BaseLayout(AuthenticationContext authContext, SettingProvider settingProvider, MenuItemRepository menuItemRepository,
                      ObjectProvider<BuildProperties> buildProperties, GlobalSearchService globalSearchService) {
        createHeader(authContext, settingProvider, globalSearchService);

        addDrawerContent(new MenuItemsToSideNavItemsTransformer().transform(menuItemRepository.findByOrderByPosition()), buildProperties);
    }

    private void createHeader(AuthenticationContext authContext, SettingProvider settingProvider, GlobalSearchService globalSearchService) {
        final var logoLayout = new HorizontalLayout();
        final var logo = loadLogoImage(settingProvider);
        logo.setHeight("44px");
        logoLayout.add(logo);

        var header = new HorizontalLayout(new DrawerToggle(), logoLayout, new GlobalSearch(globalSearchService), createFullscreenEnterButton(),
            createFullscreenExitButton());
        authContext.getAuthenticatedUser(UserDetails.class)
            .map(user -> {
                final var avatar = new Avatar(user.getUsername());
                avatar.setTooltipEnabled(true);
                final var contextMenu = new ContextMenu(avatar);
                contextMenu.setOpenOnClick(true);
                contextMenu.addItem(getTranslation("menu.main.profile"), _ -> getUI().ifPresent(ui -> ui.navigate(ProfileView.class)));
                contextMenu.addItem(getTranslation("menu.main.logout"), _ -> authContext.logout());
                return avatar;
            })
            .ifPresent(header::add);
        header.setDefaultVerticalComponentAlignment(FlexComponent.Alignment.CENTER);
        header.expand(logoLayout);
        header.setWidthFull();

        // on a phone the bar renders at the bottom of the screen, within reach of a thumb
        addToNavbar(true, header);
    }

    /**
     * Browsers only let a page go full screen from inside a user gesture, so the request is bound to the click on
     * the client side rather than sent from a server round-trip. The button is hidden where the Fullscreen API is
     * missing (e.g. Safari on iPhone).
     */
    private Button createFullscreenEnterButton() {
        final var button = createFullscreenButton(VaadinIcon.EXPAND_FULL, "menu.main.fullscreen.enter");
        Fullscreen.onClick(button).enter();
        button.bindVisible(Fullscreen.stateSignal()
            .map(state -> state != FullscreenState.FULLSCREEN && state != FullscreenState.UNSUPPORTED));
        return button;
    }

    private Button createFullscreenExitButton() {
        final var button = createFullscreenButton(VaadinIcon.COMPRESS_SQUARE, "menu.main.fullscreen.exit");
        button.addClickListener(_ -> Fullscreen.exit());
        button.bindVisible(Fullscreen.stateSignal().map(state -> state == FullscreenState.FULLSCREEN));
        return button;
    }

    private Button createFullscreenButton(VaadinIcon icon, String translationKey) {
        final var button = new Button(icon.create());
        button.addThemeVariants(ButtonVariant.TERTIARY);
        button.setTooltipText(getTranslation(translationKey));
        button.setAriaLabel(getTranslation(translationKey));
        return button;
    }

    private void addDrawerContent(List<SideNavItem> routerLinks, ObjectProvider<BuildProperties> buildProperties) {
        final var mainMenu = new H4(getTranslation("app.name"));
        mainMenu.getStyle().set("text-align", "center");
        mainMenu.setWidthFull();
        final var header = new Header(mainMenu);
        final var scroller = new Scroller(createNavigation(routerLinks));
        addToDrawer(header, scroller, createFooter(buildProperties));
    }

    private SideNav createNavigation(List<SideNavItem> routerLinks) {
        final var appNav = new SideNav();
        routerLinks.forEach(appNav::addItem);
        return appNav;
    }

    /**
     * The version comes from the jar that is running - `build-info` writes the Maven version into
     * `META-INF/build-info.properties` - rather than from a setting somebody has to keep in step with it.
     */
    private Footer createFooter(ObjectProvider<BuildProperties> buildProperties) {
        final var version = ofNullable(buildProperties.getIfAvailable())
            .map(BuildProperties::getVersion)
            .orElse(UNKNOWN_VERSION);
        final var layout = new Footer();
        layout.addClassNames("flex", "items-center", "my-s", "px-m", "py-xs");
        layout.add(new Span("v" + version));
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
