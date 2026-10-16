package org.myworkflows.restapi;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.myworkflows.exception.WorkflowDuplicateException;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import java.util.HashSet;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Pattern;

import static java.util.Optional.ofNullable;

/**
 * What the controllers of the REST API check in a request, and how they say no. A refusal is answered with its status
 * and a JSON body which tells why, see {@link RestApiExceptionHandler}.
 *
 * @author Mihai Surdeanu
 * @since 2.2
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class RestApiUtil {

    /**
     * @return the given text without the blanks around it.
     * @throws ResponseStatusException with {@code 400}, when there is no text.
     */
    public static String requireText(String value, String field) {
        if (value == null || value.isBlank()) {
            throw badRequest("'" + field + "' is missing");
        }
        return value.trim();
    }

    /**
     * @return the given name, which can be told by its characters only: a parameter or a placeholder is found by it.
     * @throws ResponseStatusException with {@code 400}, when it holds another character.
     */
    public static String requireName(String value, String allowedChars) {
        final var name = requireText(value, "name");
        if (!Pattern.matches(allowedChars + "+", name)) {
            throw badRequest("'name' accepts " + allowedChars + " only");
        }
        return name;
    }

    /**
     * @return what the given keys stand for, in their order: the definitions or the parameters of a template.
     * @throws ResponseStatusException with {@code 400}, when a key is not known or comes twice.
     */
    public static <K, T> List<T> requireAll(List<K> keys, String field, Function<K, T> finder) {
        final var seen = new HashSet<K>();
        return ofNullable(keys).orElse(List.of()).stream()
            .map(key -> {
                if (!seen.add(key)) {
                    throw badRequest("'" + field + "' holds " + key + " twice");
                }
                return ofNullable(finder.apply(key)).orElseThrow(() -> badRequest("'" + field + "' holds " + key + ", which is not known"));
            })
            .toList();
    }

    /**
     * @return what the service answered with: the copy of a definition or a template.
     * @throws ResponseStatusException with {@code 400} when the name of the copy is blank, with {@code 409} when it is
     *     taken or the original changed in the meantime.
     */
    public static <T> T duplicate(Supplier<T> duplicate) {
        try {
            return duplicate.get();
        } catch (WorkflowDuplicateException exception) {
            throw switch (exception.getReason()) {
                case NAME_MISSING -> badRequest("'name' is missing");
                case NAME_TAKEN -> conflict("'name' is taken: " + exception.getName());
                case ORIGINAL_CHANGED -> conflict("What '" + exception.getName() + "' is made of changed in the meantime");
            };
        }
    }

    public static ResponseStatusException badRequest(String reason) {
        return new ResponseStatusException(HttpStatus.BAD_REQUEST, reason);
    }

    public static ResponseStatusException forbidden(String reason) {
        return new ResponseStatusException(HttpStatus.FORBIDDEN, reason);
    }

    public static ResponseStatusException notFound(String reason) {
        return new ResponseStatusException(HttpStatus.NOT_FOUND, reason);
    }

    public static ResponseStatusException conflict(String reason) {
        return new ResponseStatusException(HttpStatus.CONFLICT, reason);
    }

}
