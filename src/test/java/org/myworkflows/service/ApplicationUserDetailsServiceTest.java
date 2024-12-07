package org.myworkflows.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.User;
import org.myworkflows.repository.UserRepository;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public class ApplicationUserDetailsServiceTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private UserRepository userRepository;

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(UserRepository.class)).thenReturn(userRepository);
    }

    @Test
    public void whenNoUserIsFoundThenExceptionIsRaised() {
        // given
        when(userRepository.findByUsername(any(String.class))).thenReturn(Optional.empty());

        // when & then
        final var service = new ApplicationUserDetailsService(applicationManager);
        assertThrows(UsernameNotFoundException.class, () -> service.loadUserByUsername("test"));
    }

    @Test
    public void whenUserIsFoundThenExceptionIsRaised() {
        // given
        when(userRepository.findByUsername(any(String.class))).thenReturn(Optional.of(new User()));

        // when & then
        final var userDetails = new ApplicationUserDetailsService(applicationManager).loadUserByUsername("test");
        assertNotNull(userDetails);
    }

}
