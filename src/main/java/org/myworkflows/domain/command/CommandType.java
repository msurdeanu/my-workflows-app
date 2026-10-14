package org.myworkflows.domain.command;

import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;
import java.util.regex.Pattern;

import static java.lang.String.format;
import static java.lang.reflect.Modifier.isAbstract;
import static java.lang.reflect.Modifier.isPublic;
import static java.util.Arrays.stream;
import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.joining;

/**
 * A type of command, as a workflow definition names it in the {@code class} field of a command. Its name is the prefix
 * of the method which runs it, its inputs are the parameters of that method - the input {@code time} of the type
 * {@code sleep} is read from the variable {@code sleep.time} - and its output is what the method returns.
 *
 * @param output the Java type of what the command returns, as a workflow author reads it, or {@code void}.
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record CommandType(String name, Class<? extends AbstractCommand> type, List<CommandInput> inputs, String output, boolean builtIn) {

    private static final String NO_OUTPUT = "void";

    private static final Pattern NAME_PATTERN = Pattern.compile("[a-zA-Z][a-zA-Z0-9_]*");

    /**
     * @throws WorkflowRuntimeException when the class cannot be run as a command; its message says why.
     */
    public static CommandType of(Class<?> type, boolean builtIn) {
        if (!AbstractCommand.class.isAssignableFrom(type) || isAbstract(type.getModifiers()) || !isPublic(type.getModifiers())) {
            throw new WorkflowRuntimeException(format("Class '%s' has to be a public class, not an abstract one, which extends '%s'.",
                type.getName(), AbstractCommand.class.getName()));
        }

        final var method = findExecutionMethod(type);
        final var name = method.getAnnotation(ExecutionMethod.class).prefix();
        if (!NAME_PATTERN.matcher(name).matches()) {
            throw new WorkflowRuntimeException(format("Class '%s' cannot be named '%s' as a command: the name starts with a letter, "
                + "followed by letters, digits and underscores.", type.getName(), name));
        }
        return new CommandType(name, type.asSubclass(AbstractCommand.class), findInputs(method), describe(method.getGenericReturnType()), builtIn);
    }

    /**
     * @return the name of the type of the given command. A command of a type which is not known keeps the name it was
     *     written with.
     */
    public static String nameOf(AbstractCommand command) {
        if (command instanceof UnknownCommand unknownCommand) {
            return unknownCommand.getType();
        }
        return stream(command.getClass().getDeclaredMethods())
            .filter(method -> method.isAnnotationPresent(ExecutionMethod.class))
            .map(method -> method.getAnnotation(ExecutionMethod.class).prefix())
            .findFirst()
            .orElse(null);
    }

    public boolean hasSubcommands() {
        return AbstractSubCommand.class.isAssignableFrom(type);
    }

    /**
     * @return whether the command returns something. The asserts and the outputs of a command which returns nothing
     *     are not evaluated.
     */
    public boolean hasOutput() {
        return !NO_OUTPUT.equals(output);
    }

    public List<String> inputNames() {
        return inputs.stream().map(CommandInput::name).toList();
    }

    /**
     * @return the inputs a command of this type cannot run without.
     */
    public List<CommandInput> requiredInputs() {
        return inputs.stream().filter(CommandInput::required).toList();
    }

    private static Method findExecutionMethod(Class<?> type) {
        final var methods = stream(type.getDeclaredMethods())
            .filter(method -> method.isAnnotationPresent(ExecutionMethod.class))
            .toList();
        if (methods.size() != 1 || !isPublic(methods.getFirst().getModifiers())) {
            throw new WorkflowRuntimeException(format("Class '%s' has to declare one public method annotated with @%s, and only one.",
                type.getName(), ExecutionMethod.class.getSimpleName()));
        }
        return methods.getFirst();
    }

    private static List<CommandInput> findInputs(Method method) {
        return stream(method.getParameters())
            // the run itself is handed over to the command, it is not an input
            .filter(parameter -> !WorkflowRun.class.equals(parameter.getType()))
            .map(parameter -> findInput(method, parameter))
            .toList();
    }

    private static CommandInput findInput(Method method, Parameter parameter) {
        // a parameter which is not annotated is looked up like an optional one, see AbstractCommand
        final var executionParam = ofNullable(parameter.getDeclaredAnnotation(ExecutionParam.class));
        return new CommandInput(findInputName(method, parameter), describe(parameter.getParameterizedType()),
            executionParam.map(ExecutionParam::required).orElse(false), executionParam.map(ExecutionParam::defaultValue).orElse(""));
    }

    private static String findInputName(Method method, Parameter parameter) {
        if (!parameter.isNamePresent()) {
            throw new WorkflowRuntimeException(format("Class '%s' was compiled without the '-parameters' option, so the names of its inputs are lost.",
                method.getDeclaringClass().getName()));
        }
        return parameter.getName();
    }

    /**
     * @return the type without its packages, type arguments included: {@code Map<String, Object>}.
     */
    private static String describe(Type type) {
        if (type instanceof Class<?> clazz) {
            return clazz.getSimpleName();
        } else if (type instanceof ParameterizedType parameterizedType) {
            return describe(parameterizedType.getRawType()) + stream(parameterizedType.getActualTypeArguments())
                .map(CommandType::describe)
                .collect(joining(", ", "<", ">"));
        }
        return type.getTypeName();
    }

}
