package org.myworkflows.restapi;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;
import tools.jackson.core.JacksonException;
import tools.jackson.databind.exc.InvalidFormatException;

import java.util.Arrays;

/**
 * Answers a call of the REST API which is refused with a JSON body which says why, as RFC 9457 describes it: the
 * status, its title and a {@code detail}. It covers what a controller refuses - an unknown id, a script which is not
 * valid, a name which is taken - and what never reaches one: a body which is not JSON, an id which is not a number.
 *
 * @author Mihai Surdeanu
 * @since 2.2
 */
@RestControllerAdvice(basePackages = "org.myworkflows.restapi")
@ConditionalOnProperty(name = "my-workflows.config.feature.restApiEnabled", havingValue = "true")
public class RestApiExceptionHandler extends ResponseEntityExceptionHandler {

    @Override
    protected ResponseEntity<Object> handleHttpMessageNotReadable(HttpMessageNotReadableException exception, HttpHeaders headers,
                                                                  HttpStatusCode status, WebRequest request) {
        final var body = createProblemDetail(exception, status, describe(exception), null, null, request);
        return handleExceptionInternal(exception, body, headers, status, request);
    }

    /**
     * The default detail is the same for every body which cannot be read, so the field the parser stopped at is named
     * instead, and for an enum the values it accepts.
     */
    private String describe(HttpMessageNotReadableException exception) {
        if (!(exception.getCause() instanceof JacksonException cause)) {
            return exception.getCause() == null ? "The body is missing" : "The body cannot be read";
        }
        final var field = new StringBuilder();
        cause.getPath().forEach(reference -> {
            if (reference.getPropertyName() != null) {
                field.append(field.isEmpty() ? "" : ".").append(reference.getPropertyName());
            } else {
                field.append('[').append(reference.getIndex()).append(']');
            }
        });
        if (field.isEmpty()) {
            return "The body is not valid JSON: " + cause.getOriginalMessage();
        }
        if (cause instanceof InvalidFormatException invalidFormat && invalidFormat.getTargetType().isEnum()) {
            return "'" + field + "' cannot be " + invalidFormat.getValue() + ": it is one of "
                + Arrays.toString(invalidFormat.getTargetType().getEnumConstants());
        }
        return "'" + field + "' cannot be read: " + cause.getOriginalMessage();
    }

}
