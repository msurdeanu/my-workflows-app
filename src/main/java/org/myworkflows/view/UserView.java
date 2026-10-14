package org.myworkflows.view;

import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.RolesAllowed;
import org.myworkflows.domain.User;
import org.myworkflows.domain.filter.UserFilter;
import org.myworkflows.domain.handler.UserEventHandler;
import org.myworkflows.exception.UserException;
import org.myworkflows.service.UserService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.PageFilter;
import org.myworkflows.view.component.PageGuide;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.UserGrid;
import org.myworkflows.view.component.UserPasswordDialog;

import java.util.concurrent.TimeUnit;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
@RolesAllowed("ROLE_ADMIN")
@Route(value = UserView.ROUTE, layout = BaseLayout.class)
public class UserView extends ResponsiveLayout implements HasDynamicTitle, UserEventHandler {

    public static final String ROUTE = "users";

    private static final int ERROR_NOTIFICATION_DURATION = 10_000;

    private final UserFilter userFilter = new UserFilter();

    private final UserGrid userGrid;

    private final UserService userService;

    public UserView(UserService userService) {
        super();
        this.userService = userService;

        final ConfigurableFilterDataProvider<User, Void, UserFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(userService::findBy, userService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(userFilter);

        userGrid = new UserGrid(this);
        userGrid.setDataProvider(configurableFilterDataProvider);

        final var filterByName = createFilterByName();
        add(createHeader(getTranslation("users.page.title"), filterByName));
        add(createFilters(ROUTE,
            PageFilter.text("name", filterByName, value -> getTranslation("users.filter.chip.name", value))));
        add(createContent(userGrid));
        add(createGuide());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.users"));
    }

    @Override
    public void onCreate(String username) {
        try {
            final var temporaryPassword = userService.create(username);
            userGrid.refreshPage();
            new UserPasswordDialog(temporaryPassword).open();
        } catch (UserException exception) {
            showError(exception);
        }
    }

    @Override
    public void onEnabledChanged(User user, boolean enabled) {
        change(() -> userService.setEnabled(user.getId(), enabled));
    }

    @Override
    public void onAdminChanged(User user, boolean admin) {
        change(() -> userService.setAdmin(user.getId(), admin));
    }

    @Override
    public void onPasswordReset(User user) {
        change(() -> new UserPasswordDialog(userService.resetPassword(user.getId())).open());
    }

    /**
     * The users are read again whatever the outcome: a change which was refused has to be taken back from the row too.
     */
    private void change(Runnable change) {
        try {
            change.run();
        } catch (UserException exception) {
            showError(exception);
        } finally {
            userGrid.refreshCurrentPage();
        }
    }

    private void showError(UserException exception) {
        Notification.show(getTranslation(exception.getReason().getTranslationKey()), ERROR_NOTIFICATION_DURATION, Notification.Position.BOTTOM_START)
            .addThemeVariants(NotificationVariant.ERROR);
    }

    private PageGuide createGuide() {
        return createGuide("users")
            .legend("active", new Checkbox(getTranslation("users.grid.active.column"), true))
            .legend("admin", new Checkbox(getTranslation("users.grid.admin.column"), true))
            .legend("temporary", PageGuide.badge(getTranslation("users.grid.password.temporary"), BadgeVariant.WARNING))
            .legend("chosen", PageGuide.badge(getTranslation("users.grid.password.chosen"), BadgeVariant.SUCCESS))
            .legend("reset", PageGuide.button(VaadinIcon.KEY))
            .steps(3)
            .note("own")
            .note("sessions")
            .note("token");
    }

    private TextField createFilterByName() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("users.filter.by-name.placeholder"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addClassName("header-filter");
        filterByNameTextField.addValueChangeListener(event -> onFilterByName(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilterByName(String value) {
        userFilter.nameCriteria(value);
        userGrid.refreshPage();
    }

}
