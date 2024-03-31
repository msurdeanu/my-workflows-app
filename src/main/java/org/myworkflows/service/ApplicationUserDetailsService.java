package org.myworkflows.service;

import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.UserAccountDetails;
import org.myworkflows.repository.UserRepository;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class ApplicationUserDetailsService implements UserDetailsService {

    private final UserRepository userRepository;

    @Override
    public UserDetails loadUserByUsername(final String username) throws UsernameNotFoundException {
        return userRepository.findByUsername(username)
                .map(UserAccountDetails::new)
                .orElseThrow(() -> new UsernameNotFoundException("Could not find user with name '" + username + "'."));
    }

}

