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

        // when & then
        final var optionalEncrypt = EncryptionHolder.INSTANCE.encrypt(data);
        assertTrue(optionalEncrypt.isPresent());
        final var optionalDecrypt = EncryptionHolder.INSTANCE.decrypt(optionalEncrypt.get());
        assertTrue(optionalDecrypt.isPresent());
        assertEquals(data, optionalDecrypt.get());
    }

}
