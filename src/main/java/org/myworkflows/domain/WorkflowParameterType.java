package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeParseException;
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
    STR("str", "String") {
        @Override
        public Object getComputedValue(String value) {
            return value;
        }

        @Override
        public Optional<String> validate(String value) {
            return empty();
        }
    },
    S_STR("s_str", "String Select") {
        @Override
        public Object getComputedValue(String value) {
            return Arrays.asList(value.split(","));
        }

        @Override
        public Optional<String> validate(String value) {
            return empty();
        }
    },
    PASS("pass", "Password") {
        @Override
        public Object getComputedValue(String value) {
            return value;
        }

        @Override
        public Optional<String> validate(String value) {
            return empty();
        }
    },
    DATE("date", "Date") {
        @Override
        public Object getComputedValue(String value) {
            return LocalDate.parse(value);
        }

        @Override
        public Optional<String> validate(String value) {
            try {
                LocalDate.parse(value);
                return empty();
            } catch (DateTimeParseException notUsed) {
                return Optional.of("Format: yyyy-MM-dd");
            }
        }
    },
    TIME("time", "Time") {
        @Override
        public Object getComputedValue(String value) {
            return LocalTime.parse(value);
        }

        @Override
        public Optional<String> validate(String value) {
            try {
                LocalTime.parse(value);
                return empty();
            } catch (DateTimeParseException notUsed) {
                return Optional.of("Format: HH:mm");
            }
        }
    },
    INT("int", "Integer") {
        @Override
        public Object getComputedValue(String value) {
            return Integer.valueOf(value);
        }

        @Override
        public Optional<String> validate(String value) {
            try {
                Integer.parseInt(value);
                return empty();
            } catch (NumberFormatException notUsed) {
                return Optional.of("Value is not an int");
            }
        }
    },
    DOUBLE("dbl", "Double") {
        @Override
        public Object getComputedValue(String value) {
            return Double.valueOf(value);
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
    BOOL("bool", "Boolean") {
        @Override
        public Object getComputedValue(String value) {
            return Boolean.valueOf(value);
        }

        @Override
        public Optional<String> validate(String value) {
            return Boolean.TRUE.toString().equalsIgnoreCase(value) || Boolean.FALSE.toString().equalsIgnoreCase(value)
                ? empty() : Optional.of("Value is not a bool");
        }
    };

    private final String value;

    private final String label;

    public abstract Object getComputedValue(String value);

    public abstract Optional<String> validate(String value);

    public static WorkflowParameterType of(String value) {
        return stream(values())
                .filter(type -> type.getValue().equals(value))
                .findFirst()
                .orElse(null);
    }

    @Override
    public String toString() {
        return label;
    }

}
