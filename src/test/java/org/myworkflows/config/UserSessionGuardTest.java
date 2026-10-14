package org.myworkflows.config;

import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.spring.security.AuthenticationContext;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.domain.User;
import org.myworkflows.domain.UserAccountDetails;
import org.myworkflows.service.UserService;
import org.myworkflows.view.LoginView;
import org.myworkflows.view.ProfileView;
import org.myworkflows.view.WorkflowRunView;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
@ExtendWith(MockitoExtension.class)
public final class UserSessionGuardTest {

    private static final int USER_ID = 7;

    @Mock
    private UserService userService;

    @Mock
    private AuthenticationContext authenticationContext;

    @Mock
    private BeforeEnterEvent beforeEnterEvent;

    private UserSessionGuard userSessionGuard;

    @BeforeEach
    public void init() {
        userSessionGuard = new UserSessionGuard(userService, authenticationContext);
        lenient().doReturn(WorkflowRunView.class).when(beforeEnterEvent).getNavigationTarget();
    }

    @AfterEach
    public void clearContext() {
        SecurityContextHolder.clearContext();
    }

    @Test
    public void whenNobodyIsSignedInThenNothingIsChecked() {
        // when
        userSessionGuard.beforeEnter(beforeEnterEvent);

        // then
        verifyNoInteractions(userService, authenticationContext);
    }

    @Test
    public void whenLoginPageIsOpenedThenNothingIsChecked() {
        // given
        signIn();
        doReturn(LoginView.class).when(beforeEnterEvent).getNavigationTarget();

        // when
        userSessionGuard.beforeEnter(beforeEnterEvent);

        // then
        verifyNoInteractions(userService, authenticationContext);
    }

    @Test
    public void whenUserIsAsTheySignedInThenThePageIsShown() {
        // given
        signIn();
        final var user = createUser();
        when(userService.findById(USER_ID)).thenReturn(Optional.of(user));

        // when
        userSessionGuard.beforeEnter(beforeEnterEvent);

        // then
        verifyPageIsShown();
    }

    @Test
    public void whenUserWasDisabledThenTheSessionEnds() {
        // given
        signIn();
        final var user = createUser();
        user.setEnabled(false);
        when(userService.findById(USER_ID)).thenReturn(Optional.of(user));

        // when
        userSessionGuard.beforeEnter(beforeEnterEvent);

        // then
        verifySessionEnds();
    }

    @Test
    public void whenUserChangedRoleThenTheSessionEnds() {
        // given
        signIn();
        final var user = createUser();
        user.setAdmin(true);
        when(userService.findById(USER_ID)).thenReturn(Optional.of(user));

        // when
        userSessionGuard.beforeEnter(beforeEnterEvent);

        // then
        verifySessionEnds();
    }

    @Test
    public void whenUserGotAnotherPasswordThenTheSessionEnds() {
        // given
        signIn();
        final var user = createUser();
        user.setPassword("another hash");
        when(userService.findById(USER_ID)).thenReturn(Optional.of(user));

        // when
        userSessionGuard.beforeEnter(beforeEnterEvent);

        // then
        verifySessionEnds();
    }

    @Test
    public void whenUserIsGoneThenTheSessionEnds() {
        // given
        signIn();
        when(userService.findById(USER_ID)).thenReturn(Optional.empty());

        // when
        userSessionGuard.beforeEnter(beforeEnterEvent);

        // then
        verifySessionEnds();
    }

    @Test
    public void whenUserHasToChooseAPasswordThenAnyPageLeadsToTheProfile() {
        // given
        signIn();
        final var user = createUser();
        user.setPasswordChangeRequired(true);
        when(userService.findById(USER_ID)).thenReturn(Optional.of(user));

        // when
        userSessionGuard.beforeEnter(beforeEnterEvent);

        // then
        verify(beforeEnterEvent).forwardTo(ProfileView.class);
        verify(authenticationContext, never()).logout();
    }

    @Test
    public void whenUserHasToChooseAPasswordThenTheProfileIsShown() {
        // given
        signIn();
        final var user = createUser();
        user.setPasswordChangeRequired(true);
        when(userService.findById(USER_ID)).thenReturn(Optional.of(user));
        doReturn(ProfileView.class).when(beforeEnterEvent).getNavigationTarget();

        // when
        userSessionGuard.beforeEnter(beforeEnterEvent);

        // then
        verifyPageIsShown();
    }

    private void verifyPageIsShown() {
        verify(authenticationContext, never()).logout();
        verify(beforeEnterEvent, never()).forwardTo(any(Class.class));
        verify(beforeEnterEvent, never()).rerouteTo(any(Class.class));
    }

    private void verifySessionEnds() {
        verify(authenticationContext).logout();
        verify(beforeEnterEvent).rerouteTo(LoginView.class);
    }

    private void signIn() {
        SecurityContextHolder.getContext().setAuthentication(new UsernamePasswordAuthenticationToken(new UserAccountDetails(createUser()), null, List.of()));
    }

    /**
     * @return an enabled user who is not an admin and has chosen their password, the same one every time.
     */
    private User createUser() {
        final var user = spy(User.of("somebody", "a hash"));
        user.setPasswordChangeRequired(false);
        lenient().doReturn(USER_ID).when(user).getId();
        return user;
    }

}
