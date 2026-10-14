package org.myworkflows.domain.command;

/**
 * An input of a command type, as the parameter of its method declares it with {@code @ExecutionParam}: the input
 * {@code time} of the type {@code sleep} is read from the variable {@code sleep.time}.
 *
 * @param type the Java type the value is turned into, as a workflow author reads it: {@code List<String>}.
 * @param required whether the command fails when the input is neither set nor given a default value.
 * @param defaultValue the value of an input which is not set, or an empty text when it has none.
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record CommandInput(String name, String type, boolean required, String defaultValue) {

    public boolean hasDefaultValue() {
        return !defaultValue.isEmpty();
    }

}
