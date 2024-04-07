package org.myworkflows.domain.command;

import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.util.Map;

import static java.time.Duration.ofMillis;
import static java.util.Optional.ofNullable;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.valueOf;
import static org.springframework.web.util.UriComponentsBuilder.fromUriString;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class HttpRequestCommand extends AbstractCommand {

    @ExecutionMethod
    public ResponseEntity<String> httpRequest(@MandatoryParam final String url,
                                              @OptionalParam final String method,
                                              @OptionalParam final String body,
                                              @OptionalParam final Map<String, String> headers,
                                              @OptionalParam final Long connectionTimeout,
                                              @OptionalParam final Long readTimeout) {
        final var resolvedMethod = ofNullable(method).map(item -> valueOf(item.toUpperCase())).orElse(GET);
        final var resolvedBody = ofNullable(body).orElse("");
        final var resolvedHeaders = ofNullable(headers).orElse(Map.of());
        final var resolvedConnectionTimeout = ofNullable(connectionTimeout).orElse(15_000L);
        final var resolvedReadTimeout = ofNullable(readTimeout).orElse(60_000L);

        return createRestTemplate(resolvedConnectionTimeout, resolvedReadTimeout)
                .exchange(fromUriString(url).build(true).toUri(), resolvedMethod,
                        createHttpEntity(resolvedBody, resolvedHeaders), String.class);
    }

    private RestTemplate createRestTemplate(final long connectionTimeout,
                                            final long readTimeout) {
        return new RestTemplateBuilder()
                .setConnectTimeout(ofMillis(connectionTimeout))
                .setReadTimeout(ofMillis(readTimeout))
                .build();
    }

    private HttpEntity<String> createHttpEntity(final String body,
                                                final Map<String, String> headers) {
        final var httpHeaders = new HttpHeaders();
        headers.forEach(httpHeaders::set);
        return new HttpEntity<>(body, httpHeaders);
    }

}
