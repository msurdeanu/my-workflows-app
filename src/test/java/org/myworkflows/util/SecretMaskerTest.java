package org.myworkflows.util;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class SecretMaskerTest {

    @Test
    public void whenNameHoldsPasswordInAnyCaseThenItNamesASecret() {
        assertTrue(SecretMasker.isSecretName("db.password"));
        assertTrue(SecretMasker.isSecretName("SMTP_PASSWORD"));
        assertTrue(SecretMasker.isSecretName("myPassword1"));
        assertFalse(SecretMasker.isSecretName("token"));
        assertFalse(SecretMasker.isSecretName(null));
    }

    @Test
    public void whenValueIsMaskedByNameThenOnlyTheBlanksStay() {
        assertEquals("****** ***", SecretMasker.maskByName("db.password", "secret 123"));
        assertEquals("secret 123", SecretMasker.maskByName("db.user", "secret 123"));
    }

    @Test
    public void whenTextHoldsKnownSecretsThenEachOccurrenceIsMasked() {
        // given
        final var text = "jdbc:postgresql://db/app?user=app&password=s3cr3t-db and again s3cr3t-db, token abcdef";

        // when
        final var masked = SecretMasker.maskText(text, List.of("s3cr3t-db", "abcdef"));

        // then
        assertEquals("jdbc:postgresql://db/app?user=app&password=******** and again ********, token ********", masked);
    }

    @Test
    public void whenOneSecretHoldsAnotherThenTheLongerIsMaskedWhole() {
        // when
        final var masked = SecretMasker.maskText("key=abcd-1234", List.of("abcd", "abcd-1234"));

        // then the shorter secret does not leave the end of the longer one behind
        assertEquals("key=********", masked);
    }

    @Test
    public void whenSecretIsBlankShortOrNullThenItIsNotSearchedFor() {
        // given
        final var secrets = Arrays.asList(null, "", "   ", "abc");

        // when
        final var masked = SecretMasker.maskText("abc stays as it is", secrets);

        // then
        assertEquals("abc stays as it is", masked);
    }

    @Test
    public void whenThereIsNoTextOrNoSecretThenTheTextIsReturnedAsItIs() {
        final var text = "nothing to hide";
        assertSame(text, SecretMasker.maskText(text, List.of()));
        assertNull(SecretMasker.maskText(null, List.of("secret")));
        assertEquals("", SecretMasker.maskText("", List.of("secret")));
    }

    @Test
    public void whenStructureIsMaskedThenSecretKeysAndKnownValuesAreHiddenAtAnyDepth() {
        // given
        final var inner = new LinkedHashMap<String, Object>();
        inner.put("dbPassword", "hunter22");
        inner.put("url", "jdbc:mysql://db?pwd=topsecret");
        final var value = new LinkedHashMap<String, Object>();
        value.put("name", "nightly");
        value.put("connection", inner);
        value.put("hosts", new String[] {"a.local", "topsecret.local"});
        value.put("count", 3);

        // when
        final var masked = SecretMasker.mask(value, List.of("topsecret"));

        // then
        final var expectedInner = new LinkedHashMap<String, Object>();
        expectedInner.put("dbPassword", "********");
        expectedInner.put("url", "jdbc:mysql://db?pwd=********");
        final var expected = new LinkedHashMap<String, Object>();
        expected.put("name", "nightly");
        expected.put("connection", expectedInner);
        expected.put("hosts", List.of("a.local", "********.local"));
        expected.put("count", "3");
        assertEquals(expected, masked);
    }

    @Test
    public void whenStructureHoldsItselfThenItIsCutAtTheMaximumDepth() {
        // given
        final var list = new ArrayList<Object>();
        list.add("item");
        list.add(list);

        // when
        Object masked = SecretMasker.mask(list, List.of());

        // then the copy ends with a mask instead of going on for ever
        for (int depth = 0; depth < SecretMasker.MAX_DEPTH; depth++) {
            assertEquals("item", ((List<?>) masked).getFirst());
            masked = ((List<?>) masked).get(1);
        }
        assertEquals(List.of(SecretMasker.MASK, SecretMasker.MASK), masked);
    }

    @Test
    public void whenValueIsNullThenNullIsReturned() {
        assertNull(SecretMasker.mask(null, List.of("secret")));
        final Map<String, Object> withNull = new HashMap<>();
        withNull.put("key", null);
        assertEquals(withNull, SecretMasker.mask(withNull, List.of("secret")));
    }

}
