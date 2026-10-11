package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.User;
import org.myworkflows.domain.UserAccountDetails;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.Optional;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 2.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class SecurityUtil {

    /**
     * Finds the user behind the current thread. It has to be called on the thread serving the user - a request
     * or a UI thread - because the security context does not follow the work handed over to a pool.
     */
    public static Optional<Integer> findCurrentUserId() {
        return ofNullable(SecurityContextHolder.getContext().getAuthentication())
            .map(Authentication::getPrincipal)
            .flatMap(SecurityUtil::findUserId);
    }

    private static Optional<Integer> findUserId(Object principal) {
        // a page is served on behalf of the account details, while a REST API call carries the user itself
        if (principal instanceof UserAccountDetails userAccountDetails) {
            return Optional.of(userAccountDetails.getUserId());
        } else if (principal instanceof User user) {
            return Optional.of(user.getId());
        }
        return Optional.empty();
    }

}
