package org.myworkflows.restapi.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.domain.User;
import org.myworkflows.repository.UserRepository;

import java.io.IOException;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public final class UserTokenFilterTest {

    @Mock
    private UserRepository userRepository;

    @Mock
    private HttpServletRequest httpServletRequest;

    @Mock
    private HttpServletResponse httpServletResponse;

    @Mock
    private FilterChain filterChain;

    @Test
    public void testLoginPath() throws ServletException, IOException {
        // given
        when(httpServletRequest.getRequestURI()).thenReturn("/login");

        // when and then
        final var userTokenFilter = new UserTokenFilter(userRepository);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(filterChain).doFilter(httpServletRequest, httpServletResponse);
    }

    @Test
    public void testMissingToken() throws ServletException, IOException {
        // given
        when(httpServletRequest.getRequestURI()).thenReturn("/api");

        // when and then
        final var userTokenFilter = new UserTokenFilter(userRepository);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(httpServletResponse).sendError(HttpServletResponse.SC_UNAUTHORIZED, "API Key is missing");
    }

    @Test
    public void testUserTokenNotFound() throws ServletException, IOException {
        // given
        when(httpServletRequest.getRequestURI()).thenReturn("/api");
        when(httpServletRequest.getParameter("token")).thenReturn("1234");
        when(userRepository.findByToken("1234")).thenReturn(Optional.empty());

        // when and then
        final var userTokenFilter = new UserTokenFilter(userRepository);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(httpServletResponse).sendError(HttpServletResponse.SC_UNAUTHORIZED, "Invalid API Key");
    }

    @Test
    public void testUserDisabled() throws ServletException, IOException {
        // given
        when(httpServletRequest.getRequestURI()).thenReturn("/api");
        when(httpServletRequest.getParameter("token")).thenReturn("1234");
        final var user = new User();
        user.setEnabled(false);
        when(userRepository.findByToken("1234")).thenReturn(Optional.of(user));

        // when and then
        final var userTokenFilter = new UserTokenFilter(userRepository);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(httpServletResponse).sendError(HttpServletResponse.SC_UNAUTHORIZED, "Invalid API Key");
    }

    @Test
    public void testUserSuccessfulRequest() throws ServletException, IOException {
        // given
        when(httpServletRequest.getRequestURI()).thenReturn("/api");
        when(httpServletRequest.getParameter("token")).thenReturn("1234");
        final var user = new User();
        user.setEnabled(true);
        when(userRepository.findByToken("1234")).thenReturn(Optional.of(user));

        // when and then
        final var userTokenFilter = new UserTokenFilter(userRepository);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(filterChain).doFilter(httpServletRequest, httpServletResponse);
        verify(httpServletResponse, never()).sendError(anyInt(), anyString());
    }

}
