package org.myworkflows.domain;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class Variable {

    @JsonProperty("name")
    private String name;

    @JsonProperty("default")
    private Object value;

}
