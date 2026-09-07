package org.myworkflows.holder;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class EncryptionHolderTest {

    @Test
    public void testEncryptionHolder() {
        // given
        final var data = "data";
        EncryptionHolder.INSTANCE.setAlgorithm("AES");
        EncryptionHolder.INSTANCE.setSecretKey("key");

        // when and then
        final var optionalEncrypt = EncryptionHolder.INSTANCE.encrypt(data);
        assertTrue(optionalEncrypt.isPresent());
        final var optionalDecrypt = EncryptionHolder.INSTANCE.decrypt(optionalEncrypt.get());
        assertTrue(optionalDecrypt.isPresent());
        assertEquals(data, optionalDecrypt.get());
    }

    @Test
    public void whenValueContainsNonAsciiCharactersThenItSurvivesTheRoundTrip() {
        // given
        final var data = "parolă-sécurisée-\u00e9\u00e8\u0219\u021b";
        EncryptionHolder.INSTANCE.setAlgorithm("AES");
        EncryptionHolder.INSTANCE.setSecretKey("key");

        // when and then
        final var optionalEncrypt = EncryptionHolder.INSTANCE.encrypt(data);
        assertTrue(optionalEncrypt.isPresent());
        assertEquals(data, EncryptionHolder.INSTANCE.decrypt(optionalEncrypt.get()).orElse(null));
    }

    @Test
    public void testExceptions() {
        // given
        EncryptionHolder.INSTANCE.setAlgorithm("AES");
        EncryptionHolder.INSTANCE.setSecretKey("key");

        // when and then
        assertTrue(EncryptionHolder.INSTANCE.encrypt(null).isEmpty());
        assertTrue(EncryptionHolder.INSTANCE.decrypt(null).isEmpty());
    }

}
