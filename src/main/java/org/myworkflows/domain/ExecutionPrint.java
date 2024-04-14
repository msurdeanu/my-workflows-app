package org.myworkflows.domain;

import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.abbreviate;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public record ExecutionPrint(String name, Object value) {

    private static final String NULL_AS_STR = "null";

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
