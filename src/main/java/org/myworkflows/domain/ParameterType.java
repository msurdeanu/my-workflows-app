package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import static java.util.Arrays.stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@RequiredArgsConstructor
public enum ParameterType {

    STR("str", String.class) {
        public Object getComputedDefaultValue(String defaultValue) {
            return defaultValue;
        }
    },
    PASS("pass", String.class) {
        public Object getComputedDefaultValue(String defaultValue) {
            return defaultValue;
        }
    },
    INT("int", Integer.class) {
        public Object getComputedDefaultValue(String defaultValue) {
            return Integer.valueOf(defaultValue);
        }
    },
    LONG("long", Long.class) {
        public Object getComputedDefaultValue(String defaultValue) {
            return Long.valueOf(defaultValue);
        }
    },
    FLOAT("float", Float.class) {
        public Object getComputedDefaultValue(String defaultValue) {
            return Float.valueOf(defaultValue);
        }
    },
    DOUBLE("double", Double.class) {
        public Object getComputedDefaultValue(String defaultValue) {
            return Double.valueOf(defaultValue);
        }
    },
    BOOL("bool", Boolean.class) {
        public Object getComputedDefaultValue(String defaultValue) {
            return Boolean.valueOf(defaultValue);
        }
    };

    private final String value;

    private final Class<?> clazz;

    public abstract Object getComputedDefaultValue(String defaultValue);

    public static ParameterType of(String value) {
        return stream(values())
            .filter(settingType -> settingType.getValue().equals(value))
            .findFirst()
            .orElse(null);
    }

}
