package org.myworkflows.restapi.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.domain.User;
import org.myworkflows.service.UserTokenService;
import org.springframework.security.core.context.SecurityContextHolder;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.lenient;
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
    private UserTokenService userTokenService;

    @Mock
    private HttpServletRequest httpServletRequest;

    @Mock
    private HttpServletResponse httpServletResponse;

    @Mock
    private FilterChain filterChain;

    @AfterEach
    public void clearContext() {
        SecurityContextHolder.clearContext();
    }

    @Test
    public void testLoginPath() throws ServletException, IOException {
        // given
        when(httpServletRequest.getRequestURI()).thenReturn("/login");

        // when and then
        final var userTokenFilter = new UserTokenFilter(userTokenService);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(filterChain).doFilter(httpServletRequest, httpServletResponse);
    }

    @Test
    public void testMissingToken() throws ServletException, IOException {
        // given
        when(httpServletRequest.getRequestURI()).thenReturn("/api");

        // when and then
        final var userTokenFilter = new UserTokenFilter(userTokenService);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(httpServletResponse).setHeader("WWW-Authenticate", "Bearer");
        verify(httpServletResponse).sendError(HttpServletResponse.SC_UNAUTHORIZED, "API Key is missing");
        verify(filterChain, never()).doFilter(any(), any());
    }

    @Test
    public void testBearerWithoutToken() throws ServletException, IOException {
        // given a header which says how the call is authenticated, and nothing more
        when(httpServletRequest.getRequestURI()).thenReturn("/api");
        when(httpServletRequest.getHeader("Authorization")).thenReturn("Bearer   ");

        // when and then
        final var userTokenFilter = new UserTokenFilter(userTokenService);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(httpServletResponse).sendError(HttpServletResponse.SC_UNAUTHORIZED, "API Key is missing");
        verify(userTokenService, never()).findUserOf(anyString());
    }

    @Test
    public void testUserTokenNotFound() throws ServletException, IOException {
        // given a token nobody has, or one which has expired, or the one of a disabled user
        when(httpServletRequest.getRequestURI()).thenReturn("/api");
        when(httpServletRequest.getHeader("Authorization")).thenReturn("Bearer 1234");
        when(userTokenService.findUserOf("1234")).thenReturn(Optional.empty());

        // when and then
        final var userTokenFilter = new UserTokenFilter(userTokenService);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(httpServletResponse).setHeader("WWW-Authenticate", "Bearer error=\"invalid_token\"");
        verify(httpServletResponse).sendError(HttpServletResponse.SC_UNAUTHORIZED, "Invalid API Key");
        verify(filterChain, never()).doFilter(any(), any());
    }

    @Test
    public void testUserSuccessfulRequestWithBearerToken() throws ServletException, IOException {
        // given
        final var user = new User();
        when(httpServletRequest.getRequestURI()).thenReturn("/api");
        when(httpServletRequest.getHeader("Authorization")).thenReturn("Bearer 1234");
        when(userTokenService.findUserOf("1234")).thenReturn(Optional.of(user));
        // the call is served on behalf of the owner of the token
        doAnswer(_ -> {
            assertSame(user, SecurityContextHolder.getContext().getAuthentication().getPrincipal());
            return null;
        }).when(filterChain).doFilter(httpServletRequest, httpServletResponse);

        // when and then
        final var userTokenFilter = new UserTokenFilter(userTokenService);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(filterChain).doFilter(httpServletRequest, httpServletResponse);
        verify(httpServletResponse, never()).sendError(anyInt(), anyString());
        // the chain is stateless, so the authentication must not stay behind on this thread
        assertNull(SecurityContextHolder.getContext().getAuthentication());
    }

    @Test
    public void testCallIsMadeWithTheRoleOfTheOwnerOfTheToken() throws ServletException, IOException {
        // given an admin and a user
        final var admin = new User();
        admin.setAdmin(true);
        when(httpServletRequest.getRequestURI()).thenReturn("/api");
        when(httpServletRequest.getHeader("Authorization")).thenReturn("Bearer 1234", "Bearer 5678");
        when(userTokenService.findUserOf("1234")).thenReturn(Optional.of(admin));
        when(userTokenService.findUserOf("5678")).thenReturn(Optional.of(new User()));
        final var authorities = new ArrayList<String>();
        doAnswer(_ -> {
            SecurityContextHolder.getContext().getAuthentication().getAuthorities()
                .forEach(authority -> authorities.add(authority.getAuthority()));
            return null;
        }).when(filterChain).doFilter(httpServletRequest, httpServletResponse);

        // when
        final var userTokenFilter = new UserTokenFilter(userTokenService);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);

        // then what only an admin may do is told apart by the same role as on a page
        assertEquals(List.of("ROLE_ADMIN", "ROLE_USER"), authorities);
    }

    @Test
    public void testBearerSchemeIsNotCaseSensitive() throws ServletException, IOException {
        // given
        when(httpServletRequest.getRequestURI()).thenReturn("/api");
        when(httpServletRequest.getHeader("Authorization")).thenReturn("bearer  1234 ");
        when(userTokenService.findUserOf("1234")).thenReturn(Optional.of(new User()));

        // when and then
        final var userTokenFilter = new UserTokenFilter(userTokenService);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(filterChain).doFilter(httpServletRequest, httpServletResponse);
    }

    @Test
    public void testTokenParameterIsIgnored() throws ServletException, IOException {
        // given a call which carries a good token in its URL, as the calls did before the header was read
        when(httpServletRequest.getRequestURI()).thenReturn("/api");
        lenient().when(httpServletRequest.getParameter("token")).thenReturn("1234");
        lenient().when(userTokenService.findUserOf("1234")).thenReturn(Optional.of(new User()));

        // when and then
        final var userTokenFilter = new UserTokenFilter(userTokenService);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(httpServletResponse).sendError(HttpServletResponse.SC_UNAUTHORIZED, "API Key is missing");
        verify(filterChain, never()).doFilter(any(), any());
        verify(httpServletRequest, never()).getParameter(anyString());
        verify(userTokenService, never()).findUserOf(anyString());
    }

    @Test
    public void testAnotherAuthorizationSchemeCarriesNoToken() throws ServletException, IOException {
        // given
        when(httpServletRequest.getRequestURI()).thenReturn("/api");
        when(httpServletRequest.getHeader("Authorization")).thenReturn("Basic dXNlcjpwYXNzd29yZA==");

        // when and then
        final var userTokenFilter = new UserTokenFilter(userTokenService);
        userTokenFilter.doFilterInternal(httpServletRequest, httpServletResponse, filterChain);
        verify(httpServletResponse).sendError(HttpServletResponse.SC_UNAUTHORIZED, "API Key is missing");
        verify(filterChain, never()).doFilter(any(), any());
    }

}
