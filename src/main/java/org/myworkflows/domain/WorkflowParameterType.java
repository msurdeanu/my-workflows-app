package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.Optional;

import static java.util.Arrays.stream;
import static java.util.Optional.empty;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@RequiredArgsConstructor
public enum WorkflowParameterType {

    STR("str", String.class) {
        public Object getComputedValue(String defaultValue) {
            return defaultValue;
        }

        public Optional<String> validate(String value) {
            return empty();
        }
    },
    PASS("pass", String.class) {
        public Object getComputedValue(String defaultValue) {
            return defaultValue;
        }

        @Override
        public Optional<String> validate(String value) {
            return empty();
        }
    },
    INT("int", Integer.class) {
        public Object getComputedValue(String defaultValue) {
            return Integer.valueOf(defaultValue);
        }

        @Override
        public Optional<String> validate(String value) {
            try {
                Integer.parseInt(value);
                return empty();
            } catch (NumberFormatException notUsed) {
                return Optional.of("Value is not an integer");
            }
        }
    },
    LONG("long", Long.class) {
        public Object getComputedValue(String defaultValue) {
            return Long.valueOf(defaultValue);
        }

        @Override
        public Optional<String> validate(String value) {
            try {
                Long.parseLong(value);
                return empty();
            } catch (NumberFormatException notUsed) {
                return Optional.of("Value is not a long");
            }
        }
    },
    FLOAT("float", Float.class) {
        public Object getComputedValue(String defaultValue) {
            return Float.valueOf(defaultValue);
        }

        @Override
        public Optional<String> validate(String value) {
            try {
                Float.parseFloat(value);
                return empty();
            } catch (NumberFormatException notUsed) {
                return Optional.of("Value is not a float");
            }
        }
    },
    DOUBLE("double", Double.class) {
        public Object getComputedValue(String defaultValue) {
            return Double.valueOf(defaultValue);
        }

        @Override
        public Optional<String> validate(String value) {
            try {
                Double.parseDouble(value);
                return empty();
            } catch (NumberFormatException notUsed) {
                return Optional.of("Value is not a double");
            }
        }
    },
    BOOL("bool", Boolean.class) {
        public Object getComputedValue(String defaultValue) {
            return Boolean.valueOf(defaultValue);
        }

        @Override
        public Optional<String> validate(String value) {
            return Boolean.TRUE.toString().equalsIgnoreCase(value) || Boolean.FALSE.toString().equalsIgnoreCase(value)
                ? empty() : Optional.of("Value is not a boolean");
        }
    };

    private final String value;

    private final Class<?> clazz;

    public abstract Object getComputedValue(String defaultValue);

    public abstract Optional<String> validate(String value);

    public static WorkflowParameterType of(String value) {
        return stream(values())
                .filter(settingType -> settingType.getValue().equals(value))
                .findFirst()
                .orElse(null);
    }

}
