package org.myworkflows.restapi.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.Feature;
import org.myworkflows.service.FeatureService;
import org.springframework.http.MediaType;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

/**
 * The first filter of the REST API: while an admin has it turned off on the features page, every call is answered
 * with {@code 404}, before a token is even read, as if there were no API. The controllers are there all the time, so
 * that turning it on takes effect at once. Not a bean, so that it is not registered a second time for every request
 * of the pages too.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@RequiredArgsConstructor
public final class RestApiFeatureFilter extends OncePerRequestFilter {

    static final String TURNED_OFF_BODY = """
        {"type":"about:blank","title":"Not Found","status":404,"detail":"The REST API is turned off on this installation."}""";

    private final FeatureService featureService;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
        throws ServletException, IOException {
        if (featureService.isEnabled(Feature.REST_API)) {
            filterChain.doFilter(request, response);
            return;
        }

        response.setStatus(HttpServletResponse.SC_NOT_FOUND);
        response.setContentType(MediaType.APPLICATION_PROBLEM_JSON_VALUE);
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());
        response.getWriter().write(TURNED_OFF_BODY);
    }

}
