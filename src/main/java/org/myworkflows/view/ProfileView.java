package org.myworkflows.view;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.confirmdialog.ConfirmDialog;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H4;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.select.Select;
import com.vaadin.flow.component.textfield.Autocomplete;
import com.vaadin.flow.component.textfield.PasswordField;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.textfield.TextFieldBase;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.PermitAll;
import org.myworkflows.config.FeatureConfig;
import org.myworkflows.domain.User;
import org.myworkflows.domain.UserToken;
import org.myworkflows.exception.UserException;
import org.myworkflows.exception.UserException.Reason;
import org.myworkflows.provider.TranslationProvider;
import org.myworkflows.service.UserService;
import org.myworkflows.service.UserTokenService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.UserTokenDialog;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.stream.Stream;

/**
 * What a user manages about their own account: the password and the API tokens. A user who still has to choose a
 * password is brought here by {@link org.myworkflows.config.UserSessionGuard} whatever page they ask for.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@PermitAll
@Route(value = ProfileView.ROUTE, layout = BaseLayout.class)
public class ProfileView extends ResponsiveLayout implements HasDynamicTitle {

    public static final String ROUTE = "profile";

    private static final int ERROR_NOTIFICATION_DURATION = 10_000;

    // for how many days a new token can be valid, where no day at all stands for a token which does not expire
    private static final int NO_EXPIRY = 0;
    private static final Integer[] TOKEN_VALIDITY_DAYS = {7, 30, 90, 365, NO_EXPIRY};
    private static final int DEFAULT_TOKEN_VALIDITY_DAYS = 90;

    private final PasswordField currentPasswordField = new PasswordField(getTranslation("profile.password.current.label"));
    private final PasswordField newPasswordField = new PasswordField(getTranslation("profile.password.new.label"));
    private final PasswordField confirmPasswordField = new PasswordField(getTranslation("profile.password.confirm.label"));
    private final TextField tokenNameField = new TextField(getTranslation("profile.token.name.label"));
    private final Select<Integer> tokenValiditySelect = new Select<>();
    private final Div tokenList = new Div();
    private final Div passwordRequiredNotice = new Div(getTranslation("profile.password.required.message"));

    private final UserService userService;

    private final UserTokenService userTokenService;

    public ProfileView(UserService userService, UserTokenService userTokenService, FeatureConfig featureConfig) {
        this.userService = userService;
        this.userTokenService = userTokenService;

        final var user = userService.findCurrentUser().orElseThrow(() -> new UserException(Reason.USER_UNKNOWN));
        passwordRequiredNotice.addClassNames("workflow-status", "running");
        passwordRequiredNotice.setVisible(user.isPasswordChangeRequired());

        final var body = new Div(passwordRequiredNotice, createAccountSection(user), createPasswordSection(),
            createTokenSection(featureConfig.isRestApiEnabled()));
        body.addClassName("profile");
        add(createHeader(getTranslation("profile.page.title")), createContent(body));
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("profile.page.title"));
    }

    private Component createAccountSection(User user) {
        final var username = new Badge(user.getUsername());
        final var role = new Badge(getTranslation(user.isAdmin() ? "users.grid.role.admin" : "users.grid.role.user"));
        role.addThemeVariants(BadgeVariant.CONTRAST);
        final var badges = new HorizontalLayout(username, role);
        badges.addClassName("cell-badges");
        return createSection("profile.account.title", "profile.account.text", badges);
    }

    private Component createPasswordSection() {
        currentPasswordField.setAutocomplete(Autocomplete.CURRENT_PASSWORD);
        newPasswordField.setAutocomplete(Autocomplete.NEW_PASSWORD);
        newPasswordField.setHelperText(getTranslation("profile.password.new.helper", UserService.MIN_PASSWORD_LENGTH));
        confirmPasswordField.setAutocomplete(Autocomplete.NEW_PASSWORD);

        final var changeButton = new Button(getTranslation("profile.password.button"), VaadinIcon.CHECK.create(), _ -> changePassword());
        changeButton.addThemeVariants(ButtonVariant.PRIMARY);
        Stream.of(currentPasswordField, newPasswordField, confirmPasswordField).forEach(PasswordField::setWidthFull);
        final var section = createSection("profile.password.title", "profile.password.text", currentPasswordField, newPasswordField,
            confirmPasswordField, changeButton);
        // Enter works from any of the three fields, once the one being typed in has handed over its value
        changeButton.addClickShortcut(Key.ENTER).listenOn(section).resetFocusOnActiveElement();
        return section;
    }

    private Component createTokenSection(boolean restApiEnabled) {
        tokenList.addClassName("profile-tokens");
        refreshTokens();

        tokenNameField.setPlaceholder(getTranslation("profile.token.name.placeholder"));
        tokenNameField.setMaxLength(UserTokenService.MAX_NAME_LENGTH);
        tokenNameField.setWidthFull();
        tokenValiditySelect.setLabel(getTranslation("profile.token.validity.label"));
        tokenValiditySelect.setItems(TOKEN_VALIDITY_DAYS);
        tokenValiditySelect.setItemLabelGenerator(days -> days == NO_EXPIRY
            ? getTranslation("profile.token.validity.never")
            : getTranslation("profile.token.validity.days", days));
        tokenValiditySelect.setValue(DEFAULT_TOKEN_VALIDITY_DAYS);

        final var createButton = new Button(getTranslation("profile.token.create.button"), VaadinIcon.PLUS.create(), _ -> createToken());
        // Enter is listened to on the name alone: on the whole section it would also answer to the buttons of the tokens
        createButton.addClickShortcut(Key.ENTER).listenOn(tokenNameField).resetFocusOnActiveElement();
        final var validity = new HorizontalLayout(tokenValiditySelect, createButton);
        validity.addClassName("cell-badges");
        validity.setAlignItems(FlexComponent.Alignment.END);

        final var section = createSection("profile.token.title", "profile.token.text", tokenList, tokenNameField, validity);
        if (!restApiEnabled) {
            final var disabledNotice = new Div(getTranslation("profile.token.disabled.message"));
            disabledNotice.addClassName("profile-text");
            section.add(disabledNotice);
        }
        return section;
    }

    private Div createSection(String titleKey, String textKey, Component... components) {
        final var text = new Div(getTranslation(textKey));
        text.addClassName("profile-text");
        final var section = new Div(new H4(getTranslation(titleKey)), text);
        section.addClassName("profile-section");
        section.add(components);
        return section;
    }

    private void changePassword() {
        Stream.of(currentPasswordField, newPasswordField, confirmPasswordField).forEach(field -> field.setInvalid(false));
        if (!newPasswordField.getValue().equals(confirmPasswordField.getValue())) {
            markAsInvalid(confirmPasswordField, getTranslation("profile.password.confirm.error"));
            return;
        }

        try {
            userService.changeOwnPassword(currentPasswordField.getValue(), newPasswordField.getValue());
        } catch (UserException exception) {
            markAsInvalid(exception.getReason() == Reason.CURRENT_PASSWORD_WRONG ? currentPasswordField : newPasswordField,
                getTranslation(exception.getReason().getTranslationKey(), UserService.MIN_PASSWORD_LENGTH));
            return;
        }

        Stream.of(currentPasswordField, newPasswordField, confirmPasswordField).forEach(PasswordField::clear);
        passwordRequiredNotice.setVisible(false);
        Notification.show(getTranslation("profile.password.changed.message"));
    }

    private void markAsInvalid(TextFieldBase<?, String> field, String errorMessage) {
        field.setErrorMessage(errorMessage);
        field.setInvalid(true);
        field.focus();
    }

    /**
     * The tokens are read again whenever one of them is created or revoked - and when that could not be done, as the
     * list on display is then older than what is saved.
     */
    private void refreshTokens() {
        tokenList.removeAll();
        final var userTokens = userTokenService.findOwnTokens();
        if (userTokens.isEmpty()) {
            final var emptyNotice = new Div(getTranslation("profile.token.empty.message"));
            emptyNotice.addClassName("profile-text");
            tokenList.add(emptyNotice);
        }
        userTokens.forEach(userToken -> tokenList.add(renderToken(userToken)));
    }

    private Component renderToken(UserToken userToken) {
        final var badges = new HorizontalLayout(new Badge(userToken.getName()), renderTokenExpiry(userToken));
        badges.addClassName("cell-badges");
        final var created = new Div(getTranslation("profile.token.created",
            getTranslation(TranslationProvider.PRETTY_TIME_FORMAT, userToken.getCreated())));
        created.addClassName("profile-text");
        created.setTitle(userToken.getCreated().toString());

        final var revokeButton = new Button(VaadinIcon.TRASH.create(), _ -> askToRevokeToken(userToken));
        revokeButton.addThemeVariants(ButtonVariant.SMALL);
        revokeButton.setAriaLabel(getTranslation("profile.token.revoke.button"));
        revokeButton.setTooltipText(getTranslation("profile.token.revoke.button"));

        final var row = new Div(new Div(badges, created), revokeButton);
        row.addClassName("profile-token");
        return row;
    }

    private Component renderTokenExpiry(UserToken userToken) {
        final var expires = userToken.getExpires();
        final Badge badge;
        if (expires == null) {
            badge = new Badge(getTranslation("profile.token.never-expires"));
            badge.addThemeVariants(BadgeVariant.CONTRAST);
        } else if (userToken.isExpiredAt(Instant.now())) {
            badge = new Badge(getTranslation("profile.token.expired", toDate(expires)));
            badge.addThemeVariants(BadgeVariant.ERROR);
        } else {
            badge = new Badge(getTranslation("profile.token.expires", toDate(expires)));
        }
        if (expires != null) {
            badge.getElement().setAttribute("title", expires.toString());
        }
        badge.addThemeVariants(BadgeVariant.SMALL);
        return badge;
    }

    /**
     * @return the day of an instant where the application runs, which is also where the crons of the templates are read.
     */
    private LocalDate toDate(Instant instant) {
        return LocalDate.ofInstant(instant, ZoneId.systemDefault());
    }

    private void createToken() {
        tokenNameField.setInvalid(false);
        final int validityDays = tokenValiditySelect.getValue();
        try {
            final var tokenSecret = userTokenService.createOwnToken(tokenNameField.getValue(),
                validityDays == NO_EXPIRY ? null : Instant.now().plus(validityDays, ChronoUnit.DAYS));
            tokenNameField.clear();
            refreshTokens();
            new UserTokenDialog(tokenSecret).open();
        } catch (UserException exception) {
            markAsInvalid(tokenNameField, getTranslation(exception.getReason().getTranslationKey(), UserTokenService.MAX_NAME_LENGTH));
        }
    }

    private void askToRevokeToken(UserToken userToken) {
        final var dialog = new ConfirmDialog();
        dialog.setHeader(getTranslation("profile.token.revoke.dialog.header", userToken.getName()));
        dialog.setText(getTranslation("profile.token.revoke.dialog.text"));
        dialog.setCancelable(true);
        dialog.setConfirmText(getTranslation("profile.token.revoke.button"));
        dialog.setConfirmButtonTheme("error primary");
        dialog.addConfirmListener(_ -> revokeToken(userToken));
        dialog.open();
    }

    private void revokeToken(UserToken userToken) {
        try {
            userTokenService.revokeOwnToken(userToken.getId());
            // a name which was refused because this token had it is free now
            tokenNameField.setInvalid(false);
            Notification.show(getTranslation("profile.token.revoked.message"));
        } catch (UserException exception) {
            Notification.show(getTranslation(exception.getReason().getTranslationKey()), ERROR_NOTIFICATION_DURATION, Notification.Position.BOTTOM_START)
                .addThemeVariants(NotificationVariant.ERROR);
        } finally {
            refreshTokens();
        }
    }

}
