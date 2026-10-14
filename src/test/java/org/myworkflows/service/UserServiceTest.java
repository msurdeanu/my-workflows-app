package org.myworkflows.service;

import com.vaadin.flow.data.provider.Query;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.domain.User;
import org.myworkflows.domain.UserAccountDetails;
import org.myworkflows.domain.filter.UserFilter;
import org.myworkflows.exception.UserException;
import org.myworkflows.exception.UserException.Reason;
import org.myworkflows.repository.UserRepository;
import org.myworkflows.util.SecurityUtil;
import org.springframework.data.domain.Sort;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
@ExtendWith(MockitoExtension.class)
public final class UserServiceTest {

    private static final int ADMIN_ID = 1;
    private static final int OTHER_ID = 2;
    private static final String ADMIN_PASSWORD = "the password of the admin";
    private static final String OTHER_PASSWORD = "the password of the other";

    // the weakest hash there is: the tests check what is done with a password, not how long it takes to guess it
    private final PasswordEncoder passwordEncoder = new BCryptPasswordEncoder(4);

    @Mock
    private UserRepository userRepository;

    private UserService userService;

    private User admin;

    private User other;

    @BeforeEach
    public void init() {
        userService = new UserService(userRepository, passwordEncoder);
        admin = createUser(ADMIN_ID, "admin", ADMIN_PASSWORD, true);
        other = createUser(OTHER_ID, "other", OTHER_PASSWORD, false);
        lenient().when(userRepository.findById(ADMIN_ID)).thenReturn(Optional.of(admin));
        lenient().when(userRepository.findById(OTHER_ID)).thenReturn(Optional.of(other));
        lenient().when(userRepository.save(any(User.class))).thenAnswer(invocation -> invocation.getArgument(0));
        signInAs(admin);
    }

    @AfterEach
    public void clearContext() {
        SecurityContextHolder.clearContext();
    }

    @Test
    public void whenUserIsCreatedThenTheyGetATemporaryPassword() {
        // when
        final var temporaryPassword = userService.create("new.user-1");

        // then
        final var user = temporaryPassword.user();
        verify(userRepository).save(user);
        assertEquals("new.user-1", user.getUsername());
        assertTrue(user.isEnabled());
        assertFalse(user.isAdmin());
        assertTrue(user.isPasswordChangeRequired());
        assertEquals(16, temporaryPassword.password().length());
        assertTrue(passwordEncoder.matches(temporaryPassword.password(), user.getPassword()));
        assertNotEquals(userService.create("another").password(), temporaryPassword.password());
    }

    @Test
    public void whenUsernameIsNotValidThenNoUserIsCreated() {
        for (var username : new String[] {null, "", "ab", "with space", "semi;colon", "x".repeat(33)}) {
            assertReason(Reason.USERNAME_INVALID, () -> userService.create(username));
        }
        verify(userRepository, never()).save(any(User.class));
    }

    @Test
    public void whenUsernameIsTakenWhateverItsCaseThenNoUserIsCreated() {
        // given
        when(userRepository.existsByUsernameIgnoreCase("Admin")).thenReturn(true);

        // when and then
        assertReason(Reason.USERNAME_TAKEN, () -> userService.create("Admin"));
        verify(userRepository, never()).save(any(User.class));
    }

    @Test
    public void whenUserIsNotAnAdminThenTheyManageNobody() {
        // given
        signInAs(other);

        // when and then
        assertReason(Reason.NOT_ALLOWED, () -> userService.create("somebody"));
        assertReason(Reason.NOT_ALLOWED, () -> userService.setEnabled(ADMIN_ID, false));
        assertReason(Reason.NOT_ALLOWED, () -> userService.setAdmin(OTHER_ID, true));
        assertReason(Reason.NOT_ALLOWED, () -> userService.resetPassword(ADMIN_ID));
        verify(userRepository, never()).save(any(User.class));
    }

    @Test
    public void whenAdminLostTheRoleSinceTheySignedInThenTheyManageNobody() {
        // given a session opened by an admin, who is not one anymore
        admin.setAdmin(false);

        // when and then
        assertReason(Reason.NOT_ALLOWED, () -> userService.setEnabled(OTHER_ID, false));
        assertTrue(other.isEnabled());
    }

    @Test
    public void whenAdminWasDisabledSinceTheySignedInThenTheyManageNobody() {
        // given
        admin.setEnabled(false);

        // when and then
        assertReason(Reason.NOT_ALLOWED, () -> userService.setAdmin(OTHER_ID, true));
        assertFalse(other.isAdmin());
    }

    @Test
    public void whenAnotherUserIsDisabledOrPromotedThenTheChangeIsSaved() {
        // when
        userService.setEnabled(OTHER_ID, false);

        // then
        assertFalse(other.isEnabled());
        verify(userRepository).save(other);

        // when
        userService.setAdmin(OTHER_ID, true);

        // then
        assertTrue(other.isAdmin());
    }

    @Test
    public void whenAdminChangesTheirOwnAccountThenNothingIsChanged() {
        assertReason(Reason.OWN_ACCOUNT, () -> userService.setEnabled(ADMIN_ID, false));
        assertReason(Reason.OWN_ACCOUNT, () -> userService.setAdmin(ADMIN_ID, false));
        assertReason(Reason.OWN_ACCOUNT, () -> userService.resetPassword(ADMIN_ID));
        assertTrue(admin.isEnabled());
        assertTrue(admin.isAdmin());
        assertTrue(passwordEncoder.matches(ADMIN_PASSWORD, admin.getPassword()));
        verify(userRepository, never()).save(any(User.class));
    }

    @Test
    public void whenUserDoesNotExistThenNothingIsChanged() {
        // given
        when(userRepository.findById(404)).thenReturn(Optional.empty());

        // when and then
        assertReason(Reason.USER_UNKNOWN, () -> userService.setEnabled(404, false));
    }

    @Test
    public void whenPasswordIsResetThenTheOldOneIsGoneAndTheNewOneIsTemporary() {
        // given a user who had chosen their password
        other.setPasswordChangeRequired(false);

        // when
        final var temporaryPassword = userService.resetPassword(OTHER_ID);

        // then
        assertFalse(passwordEncoder.matches(OTHER_PASSWORD, other.getPassword()));
        assertTrue(passwordEncoder.matches(temporaryPassword.password(), other.getPassword()));
        assertTrue(other.isPasswordChangeRequired());
        verify(userRepository).save(other);
    }

    @Test
    public void whenCurrentPasswordIsWrongThenThePasswordIsKept() {
        assertReason(Reason.CURRENT_PASSWORD_WRONG, () -> userService.changeOwnPassword("a guess", "a new password"));
        assertReason(Reason.CURRENT_PASSWORD_WRONG, () -> userService.changeOwnPassword(null, "a new password"));
        assertTrue(passwordEncoder.matches(ADMIN_PASSWORD, admin.getPassword()));
        verify(userRepository, never()).save(any(User.class));
    }

    @Test
    public void whenNewPasswordIsTooShortOrTheSameThenThePasswordIsKept() {
        assertReason(Reason.PASSWORD_TOO_SHORT, () -> userService.changeOwnPassword(ADMIN_PASSWORD, "7 chars"));
        assertReason(Reason.PASSWORD_TOO_SHORT, () -> userService.changeOwnPassword(ADMIN_PASSWORD, null));
        assertReason(Reason.PASSWORD_UNCHANGED, () -> userService.changeOwnPassword(ADMIN_PASSWORD, ADMIN_PASSWORD));
        assertTrue(passwordEncoder.matches(ADMIN_PASSWORD, admin.getPassword()));
        verify(userRepository, never()).save(any(User.class));
    }

    @Test
    public void whenOwnPasswordIsChangedThenItIsNoLongerTemporaryAndTheSessionGoesOn() {
        // given a user who still has the password they were given
        admin.setPasswordChangeRequired(true);
        final var account = SecurityUtil.findCurrentAccount().orElseThrow();

        // when
        userService.changeOwnPassword(ADMIN_PASSWORD, "8 chars!");

        // then
        assertTrue(passwordEncoder.matches("8 chars!", admin.getPassword()));
        assertFalse(admin.isPasswordChangeRequired());
        verify(userRepository).save(admin);
        // the session which changed the password is not one of those opened with the old password
        assertFalse(account.isOutdatedBy(admin));
    }

    @Test
    public void whenNobodyIsSignedInThenThereIsNoOwnAccountToChange() {
        // given
        SecurityContextHolder.clearContext();

        // when and then
        assertReason(Reason.USER_UNKNOWN, () -> userService.changeOwnPassword(ADMIN_PASSWORD, "a new password"));
        assertEquals(Optional.empty(), userService.findCurrentUser());
    }

    @Test
    public void whenUsersAreListedThenTheyAreFilteredAndPaged() {
        // given
        final var third = createUser(3, "another", "whatever", false);
        when(userRepository.findAll(any(Sort.class))).thenReturn(List.of(admin, third, other));
        final var filter = new UserFilter().nameCriteria("OTHER");

        // when and then
        assertEquals(List.of(third, other), userService.findBy(new Query<>(0, 10, List.of(), null, filter)).toList());
        assertEquals(List.of(other), userService.findBy(new Query<>(1, 10, List.of(), null, filter)).toList());
        assertEquals(List.of(third), userService.findBy(new Query<>(0, 1, List.of(), null, filter)).toList());
        assertEquals(2, userService.countBy(new Query<>(filter)));
        assertEquals(3, userService.countBy(new Query<>()));
    }

    private void assertReason(Reason reason, Runnable call) {
        assertEquals(reason, assertThrows(UserException.class, call::run).getReason());
    }

    private User createUser(int id, String username, String password, boolean isAdmin) {
        final var user = spy(User.of(username, passwordEncoder.encode(password)));
        user.setAdmin(isAdmin);
        user.setPasswordChangeRequired(false);
        lenient().doReturn(id).when(user).getId();
        return user;
    }

    /**
     * The session gets a copy of the user, as it does when somebody logs in: the user itself is read again from the
     * database for every call.
     */
    private void signInAs(User user) {
        final var copy = createUser(user.getId(), user.getUsername(), "whatever", user.isAdmin());
        copy.setPassword(user.getPassword());
        SecurityContextHolder.getContext().setAuthentication(new UsernamePasswordAuthenticationToken(new UserAccountDetails(copy), null, List.of()));
    }

}
