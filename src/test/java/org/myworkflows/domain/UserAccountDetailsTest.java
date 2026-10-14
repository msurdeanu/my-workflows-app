package org.myworkflows.domain;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class UserAccountDetailsTest {

    @Test
    public void whenUserIsAsTheySignedInThenAccountIsNotOutdated() {
        // given
        final var account = new UserAccountDetails(createUser());
        final var currentUser = createUser();
        // a password that still has to be chosen does not end a session
        currentUser.setPasswordChangeRequired(true);

        // when and then
        assertFalse(account.isOutdatedBy(currentUser));
    }

    @Test
    public void whenUserWasDisabledThenAccountIsOutdated() {
        // given
        final var currentUser = createUser();
        currentUser.setEnabled(false);

        // when and then
        assertTrue(new UserAccountDetails(createUser()).isOutdatedBy(currentUser));
    }

    @Test
    public void whenUserChangedRoleThenAccountIsOutdated() {
        // given
        final var admin = createUser();
        admin.setAdmin(true);

        // when and then
        assertTrue(new UserAccountDetails(createUser()).isOutdatedBy(admin));
        assertTrue(new UserAccountDetails(admin).isOutdatedBy(createUser()));
    }

    @Test
    public void whenUserGotAnotherPasswordThenAccountIsOutdatedUnlessItFollowedThePassword() {
        // given
        final var account = new UserAccountDetails(createUser());
        final var currentUser = createUser();
        currentUser.setPassword("another hash");

        // when and then
        assertTrue(account.isOutdatedBy(currentUser));
        account.onPasswordChanged("another hash");
        assertFalse(account.isOutdatedBy(currentUser));
        assertEquals("another hash", account.getPassword());
    }

    private User createUser() {
        return User.of("somebody", "a hash");
    }

}
