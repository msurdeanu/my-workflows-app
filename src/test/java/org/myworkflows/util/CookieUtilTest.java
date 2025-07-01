package org.myworkflows.util;

import jakarta.servlet.http.Cookie;
import org.junit.jupiter.api.Test;
import org.myworkflows.holder.EncryptionHolder;

import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class CookieUtilTest {

    @Test
    public void testSimpleCookie() {
        // given
        final var cookie = CookieUtil.createSimpleCookie("a", "b");

        // when and then
        assertEquals("a", cookie.getName());
        assertEquals("b", cookie.getValue());
        assertTrue(cookie.getSecure());
        assertTrue(cookie.isHttpOnly());
        assertEquals((int) TimeUnit.DAYS.toSeconds(30), cookie.getMaxAge());
    }

    @Test
    public void testFindCookie() {
        // given
        final var cookie = CookieUtil.createSimpleCookie("a", "b");
        final var cookies = new Cookie[] {cookie};

        // when and then
        assertTrue(CookieUtil.findCookie(cookies, "-").isEmpty());
        assertTrue(CookieUtil.findCookie(cookies, "a").isPresent());
    }

    @Test
    public void testEncryptedCookie() {
        // given
        EncryptionHolder.INSTANCE.setAlgorithm("AES");
        EncryptionHolder.INSTANCE.setSecretKey("key");
        final var cookie = CookieUtil.createSimpleCookie("a", "b");
        final var cookies = new Cookie[] {cookie};

        // when and then
        final var optionalCookie = CookieUtil.createEncryptedCookie(cookies, "-", "c");
        assertTrue(optionalCookie.isPresent());
        final var encryptedCookie = optionalCookie.get();
        assertEquals("e_-", encryptedCookie.getName());
        assertNotEquals(cookie.getValue(), encryptedCookie.getValue());
        final var newCookies = new Cookie[] {cookie, encryptedCookie};
        final var decryptedCookieValue = CookieUtil.findDecryptedCookieValue(newCookies, "-");
        assertTrue(decryptedCookieValue.isPresent());
        assertEquals("c", decryptedCookieValue.get());
    }

}
