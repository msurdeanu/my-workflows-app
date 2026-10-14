package org.myworkflows.domain;

import org.junit.jupiter.api.Test;

import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class UserTokenTest {

    @Test
    public void whenTokenIsHashedThenItsSha256DigestIsReturned() {
        // the digest of "abc" is the one every description of SHA-256 gives
        assertEquals("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad", UserToken.hashOf("abc"));
        assertNotEquals(UserToken.hashOf("abc"), UserToken.hashOf("ABC"));
    }

    @Test
    public void whenTokenIsCreatedThenItsDigestIsKeptAndNotTheTokenItself() {
        // when
        final var userToken = UserToken.of(7, "backup script", "a token", null);

        // then
        assertEquals(7, userToken.getUserId());
        assertEquals("backup script", userToken.getName());
        assertEquals(UserToken.hashOf("a token"), userToken.getTokenHash());
        assertFalse(userToken.getCreated().isAfter(Instant.now()));
    }

    @Test
    public void whenTokenHasNoExpiryThenItNeverExpires() {
        assertFalse(UserToken.of(7, "forever", "a token", null).isExpiredAt(Instant.MAX));
    }

    @Test
    public void whenTokenHasAnExpiryThenItIsExpiredFromThatInstantOn() {
        // given
        final var expires = Instant.parse("2026-10-12T10:00:00Z");
        final var userToken = UserToken.of(7, "for a while", "a token", expires);

        // when and then
        assertFalse(userToken.isExpiredAt(expires.minusMillis(1)));
        assertTrue(userToken.isExpiredAt(expires));
        assertTrue(userToken.isExpiredAt(expires.plusMillis(1)));
    }

}
