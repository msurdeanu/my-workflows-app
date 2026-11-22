package org.myworkflows.restapi.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.domain.Feature;
import org.myworkflows.service.FeatureService;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
@ExtendWith(MockitoExtension.class)
public final class RestApiFeatureFilterTest {

    @Mock
    private FeatureService featureService;

    @Mock
    private HttpServletRequest request;

    @Mock
    private HttpServletResponse response;

    @Mock
    private FilterChain filterChain;

    @Test
    public void whenRestApiIsOnThenTheCallGoesOn() throws ServletException, IOException {
        // given
        when(featureService.isEnabled(Feature.REST_API)).thenReturn(true);

        // when
        new RestApiFeatureFilter(featureService).doFilter(request, response, filterChain);

        // then
        verify(filterChain).doFilter(request, response);
        verify(response, never()).setStatus(any(Integer.class));
    }

    @Test
    public void whenRestApiIsOffThenTheCallGetsNotFoundBeforeAnythingElse() throws ServletException, IOException {
        // given
        when(featureService.isEnabled(Feature.REST_API)).thenReturn(false);
        final var body = new StringWriter();
        when(response.getWriter()).thenReturn(new PrintWriter(body));

        // when
        new RestApiFeatureFilter(featureService).doFilter(request, response, filterChain);

        // then
        verify(filterChain, never()).doFilter(any(), any());
        verify(response).setStatus(HttpServletResponse.SC_NOT_FOUND);
        verify(response).setContentType("application/problem+json");
        assertEquals(RestApiFeatureFilter.TURNED_OFF_BODY, body.toString());
    }

}
