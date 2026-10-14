package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.confirmdialog.ConfirmDialog;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.User;
import org.myworkflows.domain.handler.UserEventHandler;
import org.myworkflows.service.UserService;
import org.myworkflows.util.SecurityUtil;
import org.myworkflows.view.component.html.StandardPaginatedGrid;
import org.myworkflows.view.component.html.TextFieldWithEnterShortcut;

import java.util.List;
import java.util.function.BiConsumer;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
@RequiredArgsConstructor
public final class UserGrid extends Composite<VerticalLayout> {

    private final StandardPaginatedGrid<User, ?> paginatedGrid = new StandardPaginatedGrid<>();

    private final Integer currentUserId = SecurityUtil.findCurrentUserId().orElse(null);

    private final UserEventHandler userEventHandler;

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    /**
     * Reads the users again after one of them was changed, without leaving the page on display.
     */
    public void refreshCurrentPage() {
        paginatedGrid.refreshCurrentPage();
    }

    public void setDataProvider(DataProvider<User, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        // the field that creates a user lives in a column header, so the stacked column carries its own copy of it
        final var narrowColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderPhoneCell))
            .setHeader(createUsernameField());
        final var nameColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("users.grid.name.column"))
            .setWidth("150px")
            .setFlexGrow(1);
        final var activeColumn = paginatedGrid.addColumn(new ComponentRenderer<>(user -> renderActive(user, false)))
            .setHeader(getTranslation("users.grid.active.column"))
            .setAutoWidth(true)
            .setFlexGrow(0);
        final var adminColumn = paginatedGrid.addColumn(new ComponentRenderer<>(user -> renderAdmin(user, false)))
            .setHeader(getTranslation("users.grid.admin.column"))
            .setAutoWidth(true)
            .setFlexGrow(0);
        final var passwordColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderPassword))
            .setHeader(getTranslation("users.grid.password.column"))
            .setAutoWidth(true)
            .setFlexGrow(0);
        // wide enough for a username to be typed in its header, which the one button of a row would not ask for
        final var actionsColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActions))
            .setHeader(createUsernameField())
            .setWidth("200px")
            .setFlexGrow(0);
        paginatedGrid.setNarrowColumn(narrowColumn, List.of(nameColumn, activeColumn, adminColumn, passwordColumn, actionsColumn));

        layout.add(paginatedGrid);
        return layout;
    }

    private Component createUsernameField() {
        return new TextFieldWithEnterShortcut(userEventHandler::onCreate)
            .allowedCharPatternAndPlaceholder(UserService.USERNAME_ALLOWED_CHARS)
            .small();
    }

    /**
     * A phone shows the whole user in one cell: the name, then what the user is allowed, then the password.
     */
    private Component renderPhoneCell(User user) {
        final var rights = new HorizontalLayout(renderActive(user, true), renderAdmin(user, true));
        rights.addClassName("cell-badges");
        final var password = new HorizontalLayout(renderPassword(user), renderActions(user));
        password.addClassName("cell-badges");
        password.setAlignItems(FlexComponent.Alignment.CENTER);
        final var layout = new VerticalLayout(renderName(user), rights, password);
        layout.addClassName("phone-cell");
        layout.setPadding(false);
        layout.setSpacing(false);
        return layout;
    }

    private Component renderName(User user) {
        final var name = new Badge(user.getUsername());
        if (!user.isEnabled()) {
            name.addThemeVariants(BadgeVariant.CONTRAST);
        }
        final var layout = new HorizontalLayout(name);
        layout.addClassName("cell-badges");
        if (isCurrentUser(user)) {
            final var you = new Badge(getTranslation("users.grid.name.you"));
            you.addThemeVariants(BadgeVariant.CONTRAST, BadgeVariant.SMALL);
            layout.add(you);
        }
        return layout;
    }

    private Component renderActive(User user, boolean labelled) {
        return createCheckbox(user, "users.grid.active.column", user.isEnabled(), labelled, userEventHandler::onEnabledChanged);
    }

    private Component renderAdmin(User user, boolean labelled) {
        return createCheckbox(user, "users.grid.admin.column", user.isAdmin(), labelled, userEventHandler::onAdminChanged);
    }

    /**
     * @param labelled whether the checkbox names itself, as it has to where no column header does it.
     */
    private Checkbox createCheckbox(User user, String nameKey, boolean value, boolean labelled, BiConsumer<User, Boolean> onChange) {
        final var checkbox = new Checkbox(value);
        if (labelled) {
            checkbox.setLabel(getTranslation(nameKey));
        } else {
            checkbox.setAriaLabel(getTranslation(nameKey));
        }
        if (isCurrentUser(user)) {
            // nobody takes their own rights away, see the notes of the page
            checkbox.setEnabled(false);
            checkbox.setTooltipText(getTranslation("users.grid.own.tooltip"));
        } else {
            checkbox.addValueChangeListener(event -> onChange.accept(user, event.getValue()));
        }
        return checkbox;
    }

    private Component renderPassword(User user) {
        final var badge = user.isPasswordChangeRequired()
            ? new Badge(getTranslation("users.grid.password.temporary"))
            : new Badge(getTranslation("users.grid.password.chosen"));
        badge.addThemeVariants(user.isPasswordChangeRequired() ? BadgeVariant.WARNING : BadgeVariant.SUCCESS, BadgeVariant.SMALL);
        return badge;
    }

    private Component renderActions(User user) {
        final var resetButton = new Button(VaadinIcon.KEY.create());
        resetButton.addThemeVariants(ButtonVariant.SMALL);
        resetButton.setAriaLabel(getTranslation("users.grid.actions.button.reset.title"));
        if (isCurrentUser(user)) {
            resetButton.setEnabled(false);
            resetButton.setTooltipText(getTranslation("users.grid.own.tooltip"));
        } else {
            resetButton.setTooltipText(getTranslation("users.grid.actions.button.reset.title"));
            resetButton.addClickListener(_ -> askToResetPassword(user));
        }
        final var layout = new HorizontalLayout(resetButton);
        layout.addClassName("cell-badges");
        return layout;
    }

    private void askToResetPassword(User user) {
        final var dialog = new ConfirmDialog();
        dialog.setHeader(getTranslation("dialog.user-password.reset.header", user.getUsername()));
        dialog.setText(getTranslation("dialog.user-password.reset.text"));
        dialog.setCancelable(true);
        dialog.setConfirmText(getTranslation("dialog.user-password.reset.button.confirm"));
        dialog.setConfirmButtonTheme("error primary");
        dialog.addConfirmListener(_ -> userEventHandler.onPasswordReset(user));
        dialog.open();
    }

    private boolean isCurrentUser(User user) {
        return currentUserId != null && currentUserId == user.getId();
    }

}
