package org.myworkflows.service.loader;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.config.LibraryConfig;
import org.myworkflows.domain.Library;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.domain.command.UnknownCommand;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.CommandTypeHolder;
import org.myworkflows.serializer.SerializerFactory;
import org.myworkflows.service.LibraryService;
import org.myworkflows.service.WorkflowDefinitionValidatorService;

import javax.tools.ToolProvider;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.HexFormat;
import java.util.List;
import java.util.Optional;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class LibraryServiceLoaderTest {

    private static final String COMMAND_CLASS_NAME = "org.sample.GreetCommand";

    private static final String COMMAND_SOURCE = """
        package org.sample;

        import org.myworkflows.domain.command.AbstractCommand;
        import org.myworkflows.domain.command.api.ExecutionMethod;
        import org.myworkflows.domain.command.api.ExecutionParam;

        public final class GreetCommand extends AbstractCommand {

            @ExecutionMethod(prefix = "greet")
            public String greet(@ExecutionParam String name,
                                @ExecutionParam(required = false, defaultValue = "Hello") String greeting) {
                return greeting + ", " + name + "!";
            }

        }
        """;

    private static final String SCRIPT = """
        commands:
        - class: greet
          name: Greet the world
          inputs:
          - name: greet.name
            value: World
          outputs:
          - name: greeting
            value: output
            class: groovy
        """;

    @TempDir
    private Path tempDir;

    @Test
    public void whenLibraryBringsACommandThenWorkflowsCanUseItUntilTheLibraryIsUnloaded() throws Exception {
        // given a JAR file, in the directory of the libraries, which declares a command of its own
        final var jarFile = createLibrary("-parameters");
        final var libraryConfig = new LibraryConfig();
        libraryConfig.setBaseDirectory(jarFile.getParent().toString());
        final var libraryService = mock(LibraryService.class);
        final var libraryServiceLoader = new LibraryServiceLoader(libraryConfig, libraryService);
        final var validatorService = new WorkflowDefinitionValidatorService();

        // when the libraries are loaded
        libraryServiceLoader.load();
        try {
            // then the library is listed with its command, which is known from now on with its inputs
            verify(libraryService).create(describe(jarFile, List.of("greet")), false);
            assertEquals(Optional.of(List.of("name", "greeting")), CommandTypeHolder.INSTANCE.find("greet").map(CommandType::inputNames));

            // and a workflow which uses the command is valid, can be read, run and written back
            assertEquals(List.of(), validatorService.validate(SCRIPT));
            final var script = SerializerFactory.toObject(SCRIPT, WorkflowDefinitionScript.class);
            final var workflowRun = new WorkflowRun();
            script.getCommands().getFirst().run(workflowRun);
            assertEquals("Hello, World!", workflowRun.getCache().get("greeting"));
            assertEquals(SCRIPT, SerializerFactory.toString(script, null));
        } finally {
            libraryServiceLoader.unload();
        }

        // when the libraries are unloaded, then the command is not known anymore
        assertEquals(Optional.empty(), CommandTypeHolder.INSTANCE.find("greet"));
        assertEquals(1, validatorService.validate(SCRIPT).size());
        assertThrows(WorkflowRuntimeException.class, () -> SerializerFactory.toObject(SCRIPT, WorkflowDefinitionScript.class));
        // but a workflow saved with it can still be read
        final var script = SerializerFactory.toObjectKeepingUnknownCommands(SCRIPT, WorkflowDefinitionScript.class);
        assertInstanceOf(UnknownCommand.class, script.getCommands().getFirst());
        assertEquals(SCRIPT, SerializerFactory.toString(script, null));
    }

    @Test
    public void whenLibraryWasCompiledWithoutTheNamesOfItsParametersThenItsCommandIsLeftOut() throws Exception {
        // given a command whose inputs cannot be named, as its class does not keep the names of its parameters
        final var jarFile = createLibrary("-g:none");
        final var libraryConfig = new LibraryConfig();
        libraryConfig.setBaseDirectory(jarFile.getParent().toString());
        final var libraryService = mock(LibraryService.class);
        final var libraryServiceLoader = new LibraryServiceLoader(libraryConfig, libraryService);

        // when
        libraryServiceLoader.load();
        try {
            // then the library is loaded, without its command
            verify(libraryService).create(describe(jarFile, List.of()), false);
            assertEquals(Optional.empty(), CommandTypeHolder.INSTANCE.find("greet"));
        } finally {
            libraryServiceLoader.unload();
        }
    }

    @Test
    public void whenLoadedLibraryIsDeletedThenItsFileIsGoneAndItsCommandIsNotKnownAnymore() throws Exception {
        // given a library which is loaded and listed, with a service and a loader which are the real ones
        final var jarFile = createLibrary("-parameters");
        final var filePath = jarFile.toFile().getAbsolutePath();
        final var libraryConfig = new LibraryConfig();
        libraryConfig.setBaseDirectory(jarFile.getParent().toString());
        final var cache = new InternalCache(CacheNameEnum.LIBRARY.getName());
        final var applicationManager = mock(ApplicationManager.class);
        final var internalCacheManager = mock(InternalCacheManager.class);
        when(applicationManager.getBeanOfType(InternalCacheManager.class)).thenReturn(internalCacheManager);
        when(internalCacheManager.getCache(anyString())).thenReturn(cache);
        when(applicationManager.getBeanOfType(LibraryConfig.class)).thenReturn(libraryConfig);
        final var libraryService = new LibraryService(applicationManager);
        final var libraryServiceLoader = new LibraryServiceLoader(libraryConfig, libraryService);
        when(applicationManager.getBeanOfType(LibraryServiceLoader.class)).thenReturn(libraryServiceLoader);

        libraryServiceLoader.load();
        try {
            // and whose command was run, so its class was read from the very file which is about to be deleted
            final var workflowRun = new WorkflowRun();
            SerializerFactory.toObject(SCRIPT, WorkflowDefinitionScript.class).getCommands().getFirst().run(workflowRun);
            assertEquals("Hello, World!", workflowRun.getCache().get("greeting"));
            final var library = describe(jarFile, List.of("greet"));
            // what the cache annotation of the service does once Spring is around
            cache.put(filePath, library);

            // when
            libraryService.delete(library);

            // then the file is gone - even where a file which is in use cannot be deleted - and so is the library
            assertFalse(Files.exists(jarFile));
            assertNull(cache.get(filePath));
            assertEquals(Optional.empty(), CommandTypeHolder.INSTANCE.find("greet"));
        } finally {
            libraryServiceLoader.unload();
        }
    }

    private Library describe(Path jarFile, List<String> commands) throws Exception {
        return Library.of(jarFile.toFile().getAbsolutePath(), true, commands).withFileFacts(Files.size(jarFile),
            Files.getLastModifiedTime(jarFile).toInstant(), HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(jarFile))));
    }

    /**
     * Compiles a command against the classes of the application and packs it the way a library has to: its class and
     * the file which declares it as a command.
     *
     * @param compilerOption {@code -parameters} for a class which keeps the names of its parameters, as a command has to.
     */
    private Path createLibrary(String compilerOption) throws Exception {
        final var sourceFile = Files.createDirectories(tempDir.resolve("sources").resolve("org").resolve("sample")).resolve("GreetCommand.java");
        Files.writeString(sourceFile, COMMAND_SOURCE);
        final var classes = Files.createDirectories(tempDir.resolve("classes"));
        final var applicationClasses = Path.of(AbstractCommand.class.getProtectionDomain().getCodeSource().getLocation().toURI());
        assertEquals(0, ToolProvider.getSystemJavaCompiler().run(null, null, null, "-proc:none", compilerOption, "-d", classes.toString(),
            "-classpath", applicationClasses + File.pathSeparator + System.getProperty("java.class.path"), sourceFile.toString()));

        final var jarFile = Files.createDirectories(tempDir.resolve("libs")).resolve("greet-command.jar");
        try (var jarOutputStream = new JarOutputStream(Files.newOutputStream(jarFile))) {
            final var classFile = COMMAND_CLASS_NAME.replace('.', '/') + ".class";
            jarOutputStream.putNextEntry(new JarEntry(classFile));
            jarOutputStream.write(Files.readAllBytes(classes.resolve(classFile)));
            jarOutputStream.closeEntry();
            jarOutputStream.putNextEntry(new JarEntry("META-INF/services/" + AbstractCommand.class.getName()));
            jarOutputStream.write(COMMAND_CLASS_NAME.getBytes(UTF_8));
            jarOutputStream.closeEntry();
        }
        return jarFile;
    }

}
