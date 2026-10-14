package org.myworkflows.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.HexFormat;

/**
 * One of the API tokens of a user. The token itself is not kept, only its digest: a call is matched by the digest of
 * the token it carries, and nobody who reads the database can call the REST API with what they find there.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Entity
@Table(name = "user_tokens")
public class UserToken {

    @Id
    @Getter
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int id;

    @Getter
    @Column(name = "user_id")
    private int userId;

    @Getter
    private String name;

    @Getter
    @Column(name = "token_hash")
    private String tokenHash;

    @Getter
    private Instant created;

    /**
     * When the token stops working, or nothing for a token which works until it is revoked.
     */
    @Getter
    private Instant expires;

    public static UserToken of(int userId, String name, String token, Instant expires) {
        final var userToken = new UserToken();
        userToken.userId = userId;
        userToken.name = name;
        userToken.tokenHash = hashOf(token);
        userToken.created = Instant.now();
        userToken.expires = expires;
        return userToken;
    }

    /**
     * A token is 32 random bytes, which nobody finds by trying: a plain digest is enough, and it can be looked up by
     * an index - unlike the salted hash of a password.
     */
    public static String hashOf(String token) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(token.getBytes(StandardCharsets.UTF_8)));
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException(exception);
        }
    }

    public boolean isExpiredAt(Instant instant) {
        return expires != null && !expires.isAfter(instant);
    }

}
