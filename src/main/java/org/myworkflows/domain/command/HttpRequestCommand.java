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
import static org.springframework.http.HttpMethod.valueOf;
import static org.springframework.web.util.UriComponentsBuilder.fromUriString;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class HttpRequestCommand extends AbstractCommand {

    public static final String PREFIX = "httpRequest";

    public HttpRequestCommand(String name,
                              Set<ExpressionNameValue> ifs,
                              Set<ExpressionNameValue> inputs,
                              Set<ExpressionNameValue> asserts,
                              Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod(prefix = "httpRequest")
    public ResponseEntity<String> httpRequest(@ExecutionParam String url,
                                              @ExecutionParam(required = false, defaultValue = "GET") String method,
                                              @ExecutionParam(required = false) String body,
                                              @ExecutionParam(required = false, defaultValue = ":of") Map<String, String> headers,
                                              @ExecutionParam(required = false, defaultValue = "60000") Long timeout) {
        return createRestTemplate(timeout).exchange(fromUriString(url).build(true).toUri(), valueOf(method),
            createHttpEntity(body, headers), String.class);
    }

    private RestTemplate createRestTemplate(long timeout) {
        return new RestTemplateBuilder()
            .connectTimeout(ofMillis(timeout))
            .readTimeout(ofMillis(timeout))
            .build();
    }

    private HttpEntity<String> createHttpEntity(String body, Map<String, String> headers) {
        final var httpHeaders = new HttpHeaders();
        headers.forEach(httpHeaders::set);
        return new HttpEntity<>(body, httpHeaders);
    }

}
