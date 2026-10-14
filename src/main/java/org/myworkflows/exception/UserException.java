package org.myworkflows.exception;

import lombok.Getter;

/**
 * Raised when something asked about a user account cannot be done. The reason is meant for the user who asked: its
 * {@link Reason#getTranslationKey() translation} says what to do differently.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Getter
public final class UserException extends RuntimeException {

    private final Reason reason;

    public UserException(Reason reason) {
        super(reason.name());
        this.reason = reason;
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public enum Reason {
        USERNAME_INVALID,
        USERNAME_TAKEN,
        USER_UNKNOWN,
        NOT_ALLOWED,
        OWN_ACCOUNT,
        CURRENT_PASSWORD_WRONG,
        PASSWORD_TOO_SHORT,
        PASSWORD_UNCHANGED,
        TOKEN_NAME_INVALID,
        TOKEN_NAME_TAKEN,
        TOKEN_EXPIRY_INVALID,
        TOKEN_UNKNOWN;

        public String getTranslationKey() {
            return "users.error." + name().toLowerCase().replace('_', '-');
        }
    }

}
