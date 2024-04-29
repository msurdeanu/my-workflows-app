package org.myworkflows.domain;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@AllArgsConstructor
public enum UserRole {

    LOGGED("ROLE_LOGGED"),
    USER("ROLE_USER"),
    ADMIN("ROLE_ADMIN");

    private final String label;

    public boolean validate() {
        return (UserRole.LOGGED.equals(this) && isAuthenticated()) || hasRole(getLabel());
    }

    private boolean isAuthenticated() {
        return SecurityContextHolder.getContext().getAuthentication().getPrincipal() instanceof UserDetails;
    }

    private boolean hasRole(final String role) {
        return ofNullable(SecurityContextHolder.getContext().getAuthentication())
            .map(auth -> auth.getAuthorities().stream().anyMatch(granted -> granted.getAuthority().equals(role)))
            .orElse(false);
    }

}
