package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.Arrays;
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
    STR("str") {
        public Object getComputedValue(String defaultValue) {
            return defaultValue;
        }

        public Optional<String> validate(String value) {
            return empty();
        }
    },
    S_STR("s_str") {
        public Object getComputedValue(String defaultValue) {
            return Arrays.asList(defaultValue.split(","));
        }

        public Optional<String> validate(String value) {
            return empty();
        }
    },
    M_STR("m_str") {
        public Object getComputedValue(String defaultValue) {
            return Arrays.asList(defaultValue.split(","));
        }

        public Optional<String> validate(String value) {
            return empty();
        }
    },
    PASS("pass") {
        public Object getComputedValue(String defaultValue) {
            return defaultValue;
        }

        @Override
        public Optional<String> validate(String value) {
            return empty();
        }
    },
    INT("int") {
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
    DOUBLE("double") {
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
    BOOL("bool") {
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

    public abstract Object getComputedValue(String defaultValue);

    public abstract Optional<String> validate(String value);

    public static WorkflowParameterType of(String value) {
        return stream(values())
                .filter(type -> type.getValue().equals(value))
                .findFirst()
                .orElse(null);
    }

}
