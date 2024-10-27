package org.myworkflows.holder;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.exception.WorkflowRuntimeException;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.Optional;

import static java.util.Arrays.copyOf;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
public enum EncryptionHolder {
    INSTANCE;

    private static final Charset CHARSET = StandardCharsets.UTF_8;

    @Setter
    private String algorithm;

    private SecretKeySpec secretKeySpec;

    public Optional<String> encrypt(String value) {
        try {
            final var cipher = Cipher.getInstance(algorithm);
            cipher.init(Cipher.ENCRYPT_MODE, secretKeySpec);
            return ofNullable(Base64.getEncoder().encodeToString(cipher.doFinal(value.getBytes(CHARSET))));
        } catch (Exception exception) {
            log.warn("An exception occurred during process of encrypting a value using AES.", exception);
            return empty();
        }
    }

    public Optional<String> decrypt(String value) {
        try {
            final var cipher = Cipher.getInstance(algorithm);
            cipher.init(Cipher.DECRYPT_MODE, secretKeySpec);
            return of(new String(cipher.doFinal(Base64.getDecoder().decode(value))));
        } catch (Exception exception) {
            log.warn("An exception occurred during process of encrypting a value using AES.", exception);
            return empty();
        }
    }

    public void setSecretKey(String secretKey) {
        try {
            final var sha = MessageDigest.getInstance("SHA-1");
            secretKeySpec = new SecretKeySpec(copyOf(sha.digest(secretKey.getBytes(CHARSET)), 16), algorithm);
        } catch (NoSuchAlgorithmException exception) {
            throw new WorkflowRuntimeException(exception);
        }
    }

}
