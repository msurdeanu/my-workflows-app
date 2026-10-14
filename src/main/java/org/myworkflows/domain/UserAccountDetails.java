package org.myworkflows.domain;

import lombok.RequiredArgsConstructor;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.Collection;
import java.util.List;
import java.util.Objects;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@RequiredArgsConstructor
public final class UserAccountDetails implements UserDetails {

    private final transient User user;

    public int getUserId() {
        return user.getId();
    }

    /**
     * The account is a copy taken when the session was opened, and the session goes on with it whatever happens to
     * the user in the meantime.
     *
     * @param currentUser the user as it is now.
     * @return whether the user was disabled, changed role or got another password since then, in which case the
     *     session has to end.
     */
    public boolean isOutdatedBy(User currentUser) {
        return !currentUser.isEnabled()
            || currentUser.isAdmin() != user.isAdmin()
            || !Objects.equals(currentUser.getPassword(), user.getPassword());
    }

    /**
     * Follows a password the user of this session has just chosen, so that this session - unlike the other ones of the
     * same user - is not taken for an outdated one.
     */
    public void onPasswordChanged(String encodedPassword) {
        user.setPassword(encodedPassword);
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        return List.of(new SimpleGrantedAuthority(user.isAdmin() ? "ROLE_ADMIN" : "ROLE_USER"));
    }

    @Override
    public String getPassword() {
        return user.getPassword();
    }

    @Override
    public String getUsername() {
        return user.getUsername();
    }

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return user.isEnabled();
    }

}

