package org.myworkflows.holder;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.domain.command.EchoCommand;
import org.myworkflows.domain.command.SleepCommand;
import org.myworkflows.domain.command.api.ExecutionMethod;

import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class CommandTypeHolderTest {

    private static final List<String> BUILT_IN_NAMES = List.of("approval", "database", "groovy", "httpRequest", "java", "loop", "mail", "nothing",
        "print", "report", "sleep", "sshExec", "sshShell", "waitUntilSubPasses");

    @TempDir
    private Path tempDir;

    @AfterEach
    public void unloadPlugins() {
        CommandTypeHolder.INSTANCE.unloadPlugins();
    }

    @Test
    public void whenNoLibraryIsLoadedThenOnlyTheCommandsOfTheApplicationAreKnown() {
        // when
        final var commandTypes = CommandTypeHolder.INSTANCE.getAll();

        // then
        assertEquals(BUILT_IN_NAMES, commandTypes.stream().map(CommandType::name).toList());
        assertTrue(commandTypes.stream().allMatch(CommandType::builtIn));
        assertEquals(Optional.of(SleepCommand.class), CommandTypeHolder.INSTANCE.find("sleep").map(CommandType::type));
        assertEquals(Optional.empty(), CommandTypeHolder.INSTANCE.find(EchoCommand.PREFIX));
        assertEquals(Optional.empty(), CommandTypeHolder.INSTANCE.find(null));
    }

    @Test
    public void whenLibrariesBringCommandsThenTheOnesWhichCanBeUsedAreAdded() throws Exception {
        // given four classes named by a library, of which only the first one can be used as a command
        try (var classLoader = createClassLoader(EchoCommand.class.getName(), "org.myworkflows.MissingCommand", SecondSleepCommand.class.getName(),
            String.class.getName())) {
            // when
            final var pluginTypes = CommandTypeHolder.INSTANCE.loadPlugins(classLoader);

            // then
            assertEquals(List.of(EchoCommand.PREFIX), pluginTypes.stream().map(CommandType::name).toList());
            final var echoType = CommandTypeHolder.INSTANCE.find(EchoCommand.PREFIX).orElseThrow();
            assertEquals(EchoCommand.class, echoType.type());
            assertFalse(echoType.builtIn());
            assertEquals(BUILT_IN_NAMES.size() + 1, CommandTypeHolder.INSTANCE.getAll().size());
            // a library does not take the name of a command of the application
            assertEquals(Optional.of(SleepCommand.class), CommandTypeHolder.INSTANCE.find("sleep").map(CommandType::type));
        }

        // when the libraries are unloaded
        CommandTypeHolder.INSTANCE.unloadPlugins();

        // then
        assertEquals(Optional.empty(), CommandTypeHolder.INSTANCE.find(EchoCommand.PREFIX));
    }

    @Test
    public void whenCommandTypesChangeThenWhatIsDerivedFromThemIsMadeAgain() throws Exception {
        // given
        final var timesMade = new AtomicInteger();
        final var derived = CommandTypeHolder.INSTANCE.derive(commandTypes -> timesMade.incrementAndGet() + ":" + commandTypes.size());

        // when nothing changes
        assertEquals("1:" + BUILT_IN_NAMES.size(), derived.get());
        assertEquals("1:" + BUILT_IN_NAMES.size(), derived.get());

        // when a library brings a command, then takes it away
        try (var classLoader = createClassLoader(EchoCommand.class.getName())) {
            CommandTypeHolder.INSTANCE.loadPlugins(classLoader);
            assertEquals("2:" + (BUILT_IN_NAMES.size() + 1), derived.get());
            assertEquals("2:" + (BUILT_IN_NAMES.size() + 1), derived.get());
        }
        CommandTypeHolder.INSTANCE.unloadPlugins();

        // then
        assertEquals("3:" + BUILT_IN_NAMES.size(), derived.get());
    }

    /**
     * @return a class loader which finds the given classes declared as commands, the way the JAR file of a library
     *     declares its own.
     */
    private URLClassLoader createClassLoader(String... commandClassNames) throws Exception {
        final var services = Files.createDirectories(tempDir.resolve("META-INF").resolve("services"));
        Files.write(services.resolve(AbstractCommand.class.getName()), List.of(commandClassNames));
        return new URLClassLoader(new URL[] {tempDir.toUri().toURL()}, getClass().getClassLoader());
    }

    public static final class SecondSleepCommand extends AbstractCommand {

        @ExecutionMethod(prefix = "sleep")
        public void sleep() {
            // nothing to do
        }

    }

}
