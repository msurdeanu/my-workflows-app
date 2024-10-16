package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.util.Map;
import java.util.Set;

import static java.time.Duration.ofMillis;
import static java.util.Optional.ofNullable;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.valueOf;
import static org.springframework.web.util.UriComponentsBuilder.fromUriString;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class HttpRequestCommand extends AbstractCommand {

    public HttpRequestCommand(String name,
                              Set<ExpressionNameValue> ifs,
                              Set<ExpressionNameValue> inputs,
                              Set<ExpressionNameValue> asserts,
                              Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod(prefix = "httpRequest")
    public ResponseEntity<String> httpRequest(@ExecutionParam String url,
                                              @ExecutionParam(required = false) String method,
                                              @ExecutionParam(required = false) String body,
                                              @ExecutionParam(required = false) Map<String, String> headers,
                                              @ExecutionParam(required = false) Long connectionTimeout,
                                              @ExecutionParam(required = false) Long readTimeout) {
        final var resolvedMethod = ofNullable(method).map(item -> valueOf(item.toUpperCase())).orElse(GET);
        final var resolvedBody = ofNullable(body).orElse("");
        final var resolvedHeaders = ofNullable(headers).orElse(Map.of());
        final var resolvedConnectionTimeout = ofNullable(connectionTimeout).orElse(15_000L);
        final var resolvedReadTimeout = ofNullable(readTimeout).orElse(60_000L);

        return createRestTemplate(resolvedConnectionTimeout, resolvedReadTimeout)
            .exchange(fromUriString(url).build(true).toUri(), resolvedMethod,
                createHttpEntity(resolvedBody, resolvedHeaders), String.class);
    }

    private RestTemplate createRestTemplate(long connectionTimeout, long readTimeout) {
        return new RestTemplateBuilder()
            .setConnectTimeout(ofMillis(connectionTimeout))
            .setReadTimeout(ofMillis(readTimeout))
            .build();
    }

    private HttpEntity<String> createHttpEntity(String body, Map<String, String> headers) {
        final var httpHeaders = new HttpHeaders();
        headers.forEach(httpHeaders::set);
        return new HttpEntity<>(body, httpHeaders);
    }

}
