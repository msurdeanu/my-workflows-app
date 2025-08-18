package org.myworkflows.util;

import jakarta.servlet.http.Cookie;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.myworkflows.holder.EncryptionHolder;

import java.util.Optional;
import java.util.concurrent.TimeUnit;

import static java.util.Arrays.stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class CookieUtil {

    private static final int COOKIE_EXPIRY_DAYS = 30;

    private static final String ENCRYPTED_PREFIX = "e_";

    public static Cookie createSimpleCookie(String name, String value) {
        final var cookie = new Cookie(name, value);
        cookie.setSecure(true);
        cookie.setHttpOnly(true);
        cookie.setMaxAge((int) TimeUnit.DAYS.toSeconds(COOKIE_EXPIRY_DAYS));
        return cookie;
    }

    public static Optional<Cookie> createEncryptedCookie(Cookie[] cookies, String name, String value) {
        final var fullName = ENCRYPTED_PREFIX + name;
        return EncryptionHolder.INSTANCE.encrypt(value).map(encryptedValue -> {
            final var cookie = findCookie(cookies, fullName).orElseGet(() -> createSimpleCookie(fullName, encryptedValue));
            cookie.setValue(encryptedValue);
            cookie.setMaxAge((int) TimeUnit.DAYS.toSeconds(COOKIE_EXPIRY_DAYS));
            return cookie;
        });
    }

    public static Optional<Cookie> findCookie(Cookie[] cookies, String name) {
        return stream(cookies).filter(cookie -> cookie.getName().equals(name)).findFirst();
    }

    public static Optional<String> findDecryptedCookieValue(Cookie[] cookies, String name) {
        return findCookie(cookies, ENCRYPTED_PREFIX + name)
            .flatMap(cookie -> EncryptionHolder.INSTANCE.decrypt(cookie.getValue()));
    }

}
