package org.myworkflows.service.loader;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.config.LibraryConfig;
import org.myworkflows.domain.Library;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.holder.CommandTypeHolder;
import org.myworkflows.holder.ParentClassLoaderHolder;
import org.myworkflows.service.LibraryService;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.CodeSource;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Optional;

import static com.networknt.schema.utils.Strings.isBlank;
import static java.util.Optional.ofNullable;
import static org.myworkflows.config.LibraryConfig.JAR_EXTENSION;
import static org.myworkflows.exception.WorkflowRuntimeException.wrap;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public final class LibraryServiceLoader implements ServiceLoader {

    private static final int BUFFER_SIZE = 8192;

    private final LibraryConfig libraryConfig;
    private final LibraryService libraryService;

    private URLClassLoader urlClassLoader;

    // The libraries come before the workflow definitions, which may use the commands a library brings.
    @Order(5)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        final var jarUrls = new LinkedHashMap<File, URL>();
        listJarFiles().forEach(file -> toUrl(file).ifPresent(url -> jarUrls.put(file, url)));

        urlClassLoader = new URLClassLoader(jarUrls.values().toArray(URL[]::new), getClass().getClassLoader());
        ParentClassLoaderHolder.INSTANCE.setClassLoader(urlClassLoader);

        final var commandTypes = CommandTypeHolder.INSTANCE.loadPlugins(urlClassLoader);
        jarUrls.forEach((file, url) -> libraryService.create(describe(file, Library.of(file.getAbsolutePath(), true, findCommandsOf(url, commandTypes))),
            false));
    }

    @Override
    public void unload() {
        ofNullable(urlClassLoader).ifPresent(item -> wrap(() -> {
            CommandTypeHolder.INSTANCE.unloadPlugins();
            item.close();
            ParentClassLoaderHolder.INSTANCE.resetClassLoaderToDefault();
            return null;
        }));
    }

    private List<File> listJarFiles() {
        final var baseDirectory = libraryConfig.getBaseDirectory();
        if (isBlank(baseDirectory)) {
            log.info("No library base directory is configured, so no JAR file is loaded.");
            return List.of();
        }

        // listFiles returns null when the directory is missing or unreadable.
        final var files = new File(baseDirectory).listFiles((dir, name) -> name.endsWith(JAR_EXTENSION));
        if (files == null) {
            log.warn("Library base directory '{}' does not exist or cannot be read, so no JAR file is loaded.", baseDirectory);
            return List.of();
        }
        return List.of(files);
    }

    private Optional<URL> toUrl(File file) {
        try {
            final var url = file.toURI().toURL();
            log.info("JAR file ready to be loaded: {}", file.getAbsolutePath());
            return Optional.of(url);
        } catch (Exception exception) {
            // One broken JAR must not abort the whole startup: it is only reported as not loaded.
            log.warn("JAR file '{}' could not be loaded.", file.getAbsolutePath(), exception);
            libraryService.create(describe(file, Library.of(file.getAbsolutePath(), false)), false);
            return Optional.empty();
        }
    }

    /**
     * @return the library together with what its file says about itself now, as it is being loaded: its size, its
     *     date and its checksum. A file which cannot be read leaves them unknown, without keeping the library from
     *     being listed.
     */
    private Library describe(File file, Library library) {
        try {
            final var path = file.toPath();
            return library.withFileFacts(Files.size(path), Files.getLastModifiedTime(path).toInstant(), findChecksum(path));
        } catch (IOException | NoSuchAlgorithmException exception) {
            log.warn("JAR file '{}' could not be read, so its size, date and checksum are not known.", file.getAbsolutePath(), exception);
            return library;
        }
    }

    private String findChecksum(Path path) throws IOException, NoSuchAlgorithmException {
        final var digest = MessageDigest.getInstance("SHA-256");
        final var buffer = new byte[BUFFER_SIZE];
        try (var inputStream = Files.newInputStream(path)) {
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                digest.update(buffer, 0, bytesRead);
            }
        }
        return HexFormat.of().formatHex(digest.digest());
    }

    /**
     * @return the names of the command types whose class was read from the JAR file behind the given URL.
     */
    private List<String> findCommandsOf(URL jarUrl, List<CommandType> commandTypes) {
        return commandTypes.stream()
            .filter(commandType -> ofNullable(commandType.type().getProtectionDomain().getCodeSource())
                .map(CodeSource::getLocation)
                .filter(location -> location.toExternalForm().equals(jarUrl.toExternalForm()))
                .isPresent())
            .map(CommandType::name)
            .toList();
    }

}
