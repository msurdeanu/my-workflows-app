package org.myworkflows.restapi.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.myworkflows.repository.UserRepository;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Collections;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Component
@RequiredArgsConstructor
@ConditionalOnProperty(name = "my-workflows.config.features.rest-api.enabled", havingValue = "true")
public class UserTokenFilter extends OncePerRequestFilter {

    private final UserRepository userRepository;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
        throws ServletException, IOException {
        final var path = request.getRequestURI();
        if (!path.startsWith("/api")) {
            filterChain.doFilter(request, response);
            return;
        }

        final var token = request.getParameter("token");
        if (token == null || token.isBlank()) {
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "API Key is missing");
            return;
        }

        final var user = userRepository.findByToken(token);
        if (user.isEmpty() || !user.get().isEnabled()) {
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Invalid API Key");
            return;
        }

        SecurityContextHolder.getContext().setAuthentication(new UsernamePasswordAuthenticationToken(
            user.get(), null, Collections.emptyList()));
        filterChain.doFilter(request, response);
    }

}
