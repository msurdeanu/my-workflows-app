package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.abbreviate;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@RequiredArgsConstructor
public final class ExecutionPrint {

    private static final String NULL_AS_STR = "null";

    private final String name;

    private final Object value;

    public String getType() {
        return ofNullable(value).map(item -> item.getClass().getSimpleName().toLowerCase()).orElse(NULL_AS_STR);
    }

    public String getAbbrValue() {
        return ofNullable(value).map(item -> abbreviate(item.toString(), 64)).orElse(NULL_AS_STR);
    }

    public String getFullValue() {
        return ofNullable(value).map(Object::toString).orElse(NULL_AS_STR);
    }

}
