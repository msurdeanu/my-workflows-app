package org.myworkflows.restapi.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.myworkflows.service.UserTokenService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.http.HttpHeaders;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Collections;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Component
@RequiredArgsConstructor
@ConditionalOnProperty(name = "my-workflows.config.feature.restApiEnabled", havingValue = "true")
public class UserTokenFilter extends OncePerRequestFilter {

    private static final String BEARER = "Bearer";

    private static final String BEARER_PREFIX = BEARER + " ";

    private final UserTokenService userTokenService;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
        throws ServletException, IOException {
        final var path = request.getRequestURI();
        if (!path.startsWith("/api")) {
            filterChain.doFilter(request, response);
            return;
        }

        final var token = findToken(request);
        if (token == null || token.isBlank()) {
            response.setHeader(HttpHeaders.WWW_AUTHENTICATE, BEARER);
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "API Key is missing");
            return;
        }

        final var user = userTokenService.findUserOf(token);
        if (user.isEmpty()) {
            response.setHeader(HttpHeaders.WWW_AUTHENTICATE, BEARER + " error=\"invalid_token\"");
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Invalid API Key");
            return;
        }

        SecurityContextHolder.getContext().setAuthentication(new UsernamePasswordAuthenticationToken(
            user.get(), null, Collections.emptyList()));
        try {
            filterChain.doFilter(request, response);
        } finally {
            // The chain is stateless, so the authentication must not survive on the thread handling the request.
            SecurityContextHolder.clearContext();
        }
    }

    /**
     * The token is read from the {@code Authorization} header and from nowhere else. The {@code token} URL parameter
     * the calls used to carry is ignored on purpose: an access log and a proxy write a URL down, token included.
     */
    private String findToken(HttpServletRequest request) {
        final var authorization = request.getHeader(HttpHeaders.AUTHORIZATION);
        if (authorization != null && authorization.regionMatches(true, 0, BEARER_PREFIX, 0, BEARER_PREFIX.length())) {
            return authorization.substring(BEARER_PREFIX.length()).trim();
        }
        return null;
    }

}
