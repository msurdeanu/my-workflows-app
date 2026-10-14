package org.myworkflows.service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.User;
import org.myworkflows.domain.UserToken;
import org.myworkflows.domain.UserTokenSecret;
import org.myworkflows.exception.UserException;
import org.myworkflows.exception.UserException.Reason;
import org.myworkflows.repository.UserTokenRepository;
import org.springframework.stereotype.Service;

import java.security.SecureRandom;
import java.time.Instant;
import java.util.HexFormat;
import java.util.List;
import java.util.Optional;

/**
 * The API tokens of the users. A user manages their own tokens and nobody else's, and like {@link UserService} this
 * caches nothing: what is read here decides whose REST API call is accepted.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Service
@RequiredArgsConstructor
public class UserTokenService {

    public static final int MAX_NAME_LENGTH = 64;

    private static final int TOKEN_BYTES = 32;

    private final SecureRandom secureRandom = new SecureRandom();

    private final UserTokenRepository userTokenRepository;

    private final UserService userService;

    /**
     * @return the tokens of the current user, the newest first. The expired ones are among them, until they are
     *     revoked.
     */
    public List<UserToken> findOwnTokens() {
        return userTokenRepository.findByUserIdOrderByCreatedDescIdDesc(getCurrentUserId());
    }

    /**
     * @param name what the user tells the token from their other ones by.
     * @param expires when the token stops working, or nothing for a token which works until it is revoked.
     * @return the new token of the current user, together with the only copy of the token itself.
     */
    @Transactional
    public UserTokenSecret createOwnToken(String name, Instant expires) {
        final var userId = getCurrentUserId();
        final var trimmedName = name == null ? "" : name.trim();
        if (trimmedName.isEmpty() || trimmedName.length() > MAX_NAME_LENGTH) {
            throw new UserException(Reason.TOKEN_NAME_INVALID);
        }
        if (userTokenRepository.existsByUserIdAndNameIgnoreCase(userId, trimmedName)) {
            throw new UserException(Reason.TOKEN_NAME_TAKEN);
        }
        if (expires != null && !expires.isAfter(Instant.now())) {
            throw new UserException(Reason.TOKEN_EXPIRY_INVALID);
        }
        final var secret = generateToken();
        return new UserTokenSecret(userTokenRepository.save(UserToken.of(userId, trimmedName, secret, expires)), secret);
    }

    /**
     * Revokes a token of the current user, which stops working at once.
     */
    @Transactional
    public void revokeOwnToken(int tokenId) {
        userTokenRepository.delete(userTokenRepository.findByIdAndUserId(tokenId, getCurrentUserId())
            .orElseThrow(() -> new UserException(Reason.TOKEN_UNKNOWN)));
    }

    /**
     * @return the user a REST API call with this token is made by: nobody for a token which is not known or has
     *     expired, and nobody for the token of a disabled user.
     */
    public Optional<User> findUserOf(String token) {
        if (token == null || token.isBlank()) {
            return Optional.empty();
        }
        return userTokenRepository.findByTokenHash(UserToken.hashOf(token))
            .filter(userToken -> !userToken.isExpiredAt(Instant.now()))
            .flatMap(userToken -> userService.findById(userToken.getUserId()))
            .filter(User::isEnabled);
    }

    private int getCurrentUserId() {
        return userService.findCurrentUser().orElseThrow(() -> new UserException(Reason.USER_UNKNOWN)).getId();
    }

    private String generateToken() {
        final var bytes = new byte[TOKEN_BYTES];
        secureRandom.nextBytes(bytes);
        return HexFormat.of().withUpperCase().formatHex(bytes);
    }

}
