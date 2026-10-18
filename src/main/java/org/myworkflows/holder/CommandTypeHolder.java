package org.myworkflows.holder;

import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.domain.command.ApprovalCommand;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.domain.command.DatabaseCommand;
import org.myworkflows.domain.command.GroovyCommand;
import org.myworkflows.domain.command.HttpRequestCommand;
import org.myworkflows.domain.command.JavaCommand;
import org.myworkflows.domain.command.LoopCommand;
import org.myworkflows.domain.command.MailCommand;
import org.myworkflows.domain.command.NothingCommand;
import org.myworkflows.domain.command.PrintCommand;
import org.myworkflows.domain.command.ReportCommand;
import org.myworkflows.domain.command.SleepCommand;
import org.myworkflows.domain.command.SshExecCommand;
import org.myworkflows.domain.command.SshShellCommand;
import org.myworkflows.domain.command.WaitUntilSubPassesCommand;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static java.util.Collections.unmodifiableMap;
import static java.util.Optional.ofNullable;

/**
 * The command types a workflow definition can use: the ones of the application, plus the ones brought by the libraries.
 * A library brings a command by naming its class in {@code META-INF/services/org.myworkflows.domain.command.AbstractCommand},
 * the way a provider is declared to {@link ServiceLoader}.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Slf4j
public enum CommandTypeHolder {
    INSTANCE;

    /**
     * Replaced as a whole when the libraries are loaded, never changed: whoever reads it sees a list which is complete.
     */
    private volatile Map<String, CommandType> commandTypes = unmodifiableMap(toMap(findBuiltInTypes()));

    public Collection<CommandType> getAll() {
        return commandTypes.values();
    }

    public Optional<CommandType> find(String name) {
        return ofNullable(commandTypes.get(name));
    }

    /**
     * Replaces the command types brought by the libraries with the ones which can be reached through the given class
     * loader. A command which cannot be used - its class does not load, or it takes the name of another command - is
     * left out and reported in the log, and so it does not stand in the way of the other ones.
     *
     * @return the command types which are brought by the libraries from now on.
     */
    public synchronized List<CommandType> loadPlugins(ClassLoader classLoader) {
        final var newCommandTypes = toMap(findBuiltInTypes());
        final var pluginTypes = new ArrayList<CommandType>();
        final var providers = ServiceLoader.load(AbstractCommand.class, classLoader).stream().iterator();
        while (true) {
            try {
                if (!providers.hasNext()) {
                    break;
                }
                final var pluginType = CommandType.of(providers.next().type(), false);
                if (newCommandTypes.putIfAbsent(pluginType.name(), pluginType) != null) {
                    throw new WorkflowRuntimeException("Class '" + pluginType.type().getName() + "' cannot be named '" + pluginType.name()
                        + "' as a command: another command has this name.");
                }
                pluginTypes.add(pluginType);
            } catch (WorkflowRuntimeException | ServiceConfigurationError | LinkageError notUsable) {
                // the iterator has moved past the provider which failed, so the next round looks at the next one
                log.warn("A command brought by a library is left out. {}", notUsable.getMessage());
            }
        }
        commandTypes = unmodifiableMap(newCommandTypes);
        return pluginTypes;
    }

    public synchronized void unloadPlugins() {
        commandTypes = unmodifiableMap(toMap(findBuiltInTypes()));
    }

    /**
     * @return something made out of the command types, which is made again once they change. It is what keeps a
     *     parser or a schema in step with the libraries, without it being told when they are loaded.
     */
    public <T> Supplier<T> derive(Function<Collection<CommandType>, T> factory) {
        final var lastDerived = new AtomicReference<Derived<T>>();
        return () -> {
            final var currentCommandTypes = commandTypes;
            var derived = lastDerived.get();
            if (derived == null || derived.source() != currentCommandTypes) {
                derived = new Derived<>(currentCommandTypes, factory.apply(currentCommandTypes.values()));
                lastDerived.set(derived);
            }
            return derived.value();
        };
    }

    private static Stream<CommandType> findBuiltInTypes() {
        return Stream.of(ApprovalCommand.class, DatabaseCommand.class, GroovyCommand.class, HttpRequestCommand.class, JavaCommand.class,
                LoopCommand.class, MailCommand.class, NothingCommand.class, PrintCommand.class, ReportCommand.class, SleepCommand.class,
                SshExecCommand.class, SshShellCommand.class, WaitUntilSubPassesCommand.class)
            .map(type -> CommandType.of(type, true));
    }

    private static Map<String, CommandType> toMap(Stream<CommandType> types) {
        final var typesByName = new LinkedHashMap<String, CommandType>();
        types.forEach(type -> typesByName.put(type.name(), type));
        return typesByName;
    }

    private record Derived<T>(Map<String, CommandType> source, T value) {

    }

}
