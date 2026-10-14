package org.myworkflows.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.domain.User;
import org.myworkflows.domain.UserToken;
import org.myworkflows.exception.UserException;
import org.myworkflows.exception.UserException.Reason;
import org.myworkflows.repository.UserTokenRepository;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
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
public final class UserTokenServiceTest {

    private static final int USER_ID = 7;
    private static final int OTHER_ID = 8;

    @Mock
    private UserTokenRepository userTokenRepository;

    @Mock
    private UserService userService;

    private UserTokenService userTokenService;

    private User user;

    @BeforeEach
    public void init() {
        userTokenService = new UserTokenService(userTokenRepository, userService);
        user = createUser(USER_ID);
        lenient().when(userService.findCurrentUser()).thenReturn(Optional.of(user));
        lenient().when(userService.findById(USER_ID)).thenReturn(Optional.of(user));
        lenient().when(userTokenRepository.save(any(UserToken.class))).thenAnswer(invocation -> invocation.getArgument(0));
    }

    @Test
    public void whenTokenIsCreatedThenItsSecretIsReturnedAndOnlyItsDigestIsSaved() {
        // given
        final var expires = Instant.now().plus(30, ChronoUnit.DAYS);

        // when
        final var tokenSecret = userTokenService.createOwnToken("  backup script ", expires);

        // then
        final var userToken = tokenSecret.userToken();
        verify(userTokenRepository).save(userToken);
        assertTrue(tokenSecret.secret().matches("[0-9A-F]{64}"));
        assertEquals(USER_ID, userToken.getUserId());
        assertEquals("backup script", userToken.getName());
        assertEquals(UserToken.hashOf(tokenSecret.secret()), userToken.getTokenHash());
        assertEquals(expires, userToken.getExpires());
        assertNotEquals(tokenSecret.secret(), userTokenService.createOwnToken("another", null).secret());
    }

    @Test
    public void whenTokenIsCreatedWithoutAnExpiryThenItHasNone() {
        assertNull(userTokenService.createOwnToken("forever", null).userToken().getExpires());
    }

    @Test
    public void whenNameIsNotValidThenNoTokenIsCreated() {
        for (var name : new String[] {null, "", "   ", "x".repeat(UserTokenService.MAX_NAME_LENGTH + 1)}) {
            assertReason(Reason.TOKEN_NAME_INVALID, () -> userTokenService.createOwnToken(name, null));
        }
        verify(userTokenRepository, never()).save(any(UserToken.class));
    }

    @Test
    public void whenNameIsAsLongAsItCanBeThenTheTokenIsCreated() {
        // given
        final var name = "x".repeat(UserTokenService.MAX_NAME_LENGTH);

        // when and then
        assertEquals(name, userTokenService.createOwnToken(name, null).userToken().getName());
    }

    @Test
    public void whenUserAlreadyHasATokenWithThatNameThenNoTokenIsCreated() {
        // given
        when(userTokenRepository.existsByUserIdAndNameIgnoreCase(USER_ID, "Backup")).thenReturn(true);

        // when and then
        assertReason(Reason.TOKEN_NAME_TAKEN, () -> userTokenService.createOwnToken("Backup", null));
        verify(userTokenRepository, never()).save(any(UserToken.class));
    }

    @Test
    public void whenExpiryIsNotInTheFutureThenNoTokenIsCreated() {
        assertReason(Reason.TOKEN_EXPIRY_INVALID, () -> userTokenService.createOwnToken("late", Instant.now().minusSeconds(1)));
        verify(userTokenRepository, never()).save(any(UserToken.class));
    }

    @Test
    public void whenOwnTokensAreListedThenOnlyTheOnesOfTheCurrentUserAreRead() {
        // given
        final var userTokens = List.of(UserToken.of(USER_ID, "one", "a token", null));
        when(userTokenRepository.findByUserIdOrderByCreatedDescIdDesc(USER_ID)).thenReturn(userTokens);

        // when and then
        assertEquals(userTokens, userTokenService.findOwnTokens());
    }

    @Test
    public void whenOwnTokenIsRevokedThenItIsDeleted() {
        // given
        final var userToken = UserToken.of(USER_ID, "one", "a token", null);
        when(userTokenRepository.findByIdAndUserId(3, USER_ID)).thenReturn(Optional.of(userToken));

        // when
        userTokenService.revokeOwnToken(3);

        // then
        verify(userTokenRepository).delete(userToken);
    }

    @Test
    public void whenTokenBelongsToSomebodyElseThenItIsNotRevoked() {
        // given a token which exists, but is not one of the current user
        when(userTokenRepository.findByIdAndUserId(3, USER_ID)).thenReturn(Optional.empty());

        // when and then
        assertReason(Reason.TOKEN_UNKNOWN, () -> userTokenService.revokeOwnToken(3));
        verify(userTokenRepository, never()).delete(any(UserToken.class));
    }

    @Test
    public void whenNobodyIsSignedInThenThereAreNoOwnTokens() {
        // given
        when(userService.findCurrentUser()).thenReturn(Optional.empty());

        // when and then
        assertReason(Reason.USER_UNKNOWN, () -> userTokenService.findOwnTokens());
        assertReason(Reason.USER_UNKNOWN, () -> userTokenService.createOwnToken("one", null));
        assertReason(Reason.USER_UNKNOWN, () -> userTokenService.revokeOwnToken(3));
        verify(userTokenRepository, never()).save(any(UserToken.class));
    }

    @Test
    public void whenTokenIsKnownThenItsUserIsFoundByItsDigest() {
        // given
        final var forever = UserToken.of(USER_ID, "forever", "TOKEN-1", null);
        final var forAWhile = UserToken.of(USER_ID, "for a while", "TOKEN-2", Instant.now().plus(1, ChronoUnit.DAYS));
        when(userTokenRepository.findByTokenHash(UserToken.hashOf("TOKEN-1"))).thenReturn(Optional.of(forever));
        when(userTokenRepository.findByTokenHash(UserToken.hashOf("TOKEN-2"))).thenReturn(Optional.of(forAWhile));

        // when and then
        assertEquals(Optional.of(user), userTokenService.findUserOf("TOKEN-1"));
        assertEquals(Optional.of(user), userTokenService.findUserOf("TOKEN-2"));
    }

    @Test
    public void whenTokenIsNotKnownThenNobodyIsFound() {
        // given
        when(userTokenRepository.findByTokenHash(anyString())).thenReturn(Optional.empty());

        // when and then
        assertEquals(Optional.empty(), userTokenService.findUserOf("TOKEN-1"));
    }

    @Test
    public void whenThereIsNoTokenThenNobodyIsFoundAndNothingIsRead() {
        assertEquals(Optional.empty(), userTokenService.findUserOf(null));
        assertEquals(Optional.empty(), userTokenService.findUserOf(" "));
        verify(userTokenRepository, never()).findByTokenHash(anyString());
    }

    @Test
    public void whenTokenHasExpiredThenNobodyIsFound() {
        // given
        final var expired = UserToken.of(USER_ID, "expired", "TOKEN-1", Instant.now().minusSeconds(1));
        when(userTokenRepository.findByTokenHash(UserToken.hashOf("TOKEN-1"))).thenReturn(Optional.of(expired));

        // when and then
        assertEquals(Optional.empty(), userTokenService.findUserOf("TOKEN-1"));
        verify(userService, never()).findById(anyInt());
    }

    @Test
    public void whenUserOfTheTokenIsDisabledOrGoneThenNobodyIsFound() {
        // given
        when(userTokenRepository.findByTokenHash(UserToken.hashOf("TOKEN-1"))).thenReturn(Optional.of(UserToken.of(USER_ID, "one", "TOKEN-1", null)));
        when(userTokenRepository.findByTokenHash(UserToken.hashOf("TOKEN-2"))).thenReturn(Optional.of(UserToken.of(OTHER_ID, "two", "TOKEN-2", null)));
        when(userService.findById(OTHER_ID)).thenReturn(Optional.empty());
        user.setEnabled(false);

        // when and then
        assertEquals(Optional.empty(), userTokenService.findUserOf("TOKEN-1"));
        assertEquals(Optional.empty(), userTokenService.findUserOf("TOKEN-2"));
    }

    private void assertReason(Reason reason, Runnable call) {
        assertEquals(reason, assertThrows(UserException.class, call::run).getReason());
    }

    private User createUser(int id) {
        final var createdUser = spy(User.of("user-" + id, "a hash"));
        lenient().doReturn(id).when(createdUser).getId();
        return createdUser;
    }

}
