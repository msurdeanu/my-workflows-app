package org.myworkflows.config;

import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.server.ServiceInitEvent;
import com.vaadin.flow.server.VaadinServiceInitListener;
import com.vaadin.flow.spring.security.AuthenticationContext;
import lombok.RequiredArgsConstructor;
import org.myworkflows.service.UserService;
import org.myworkflows.util.SecurityUtil;
import org.myworkflows.view.LoginView;
import org.myworkflows.view.ProfileView;
import org.springframework.stereotype.Component;

/**
 * Looks at the user behind a session every time a page is about to be shown. A session is opened once and then lives
 * on its own, so this is where it learns what happened to its user since then.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Component
@RequiredArgsConstructor
public class UserSessionGuard implements VaadinServiceInitListener {

    private final UserService userService;

    private final AuthenticationContext authenticationContext;

    @Override
    public void serviceInit(ServiceInitEvent serviceInitEvent) {
        serviceInitEvent.getSource().addUIInitListener(uiInitEvent -> uiInitEvent.getUI().addBeforeEnterListener(this::beforeEnter));
    }

    /**
     * Ends the session of a user who was disabled, changed role or got another password, and lets a user who still has
     * to choose a password see nothing but the page where it is chosen.
     */
    void beforeEnter(BeforeEnterEvent beforeEnterEvent) {
        final var account = SecurityUtil.findCurrentAccount().orElse(null);
        if (account == null || LoginView.class.equals(beforeEnterEvent.getNavigationTarget())) {
            return;
        }

        final var user = userService.findById(account.getUserId())
            .filter(item -> !account.isOutdatedBy(item))
            .orElse(null);
        if (user == null) {
            // the login page is the one page which can still be shown to a session that is going away
            authenticationContext.logout();
            beforeEnterEvent.rerouteTo(LoginView.class);
        } else if (user.isPasswordChangeRequired() && !ProfileView.class.equals(beforeEnterEvent.getNavigationTarget())) {
            beforeEnterEvent.forwardTo(ProfileView.class);
        }
    }

}
