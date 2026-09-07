package org.myworkflows.service.loader;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.config.LibraryConfig;
import org.myworkflows.domain.Library;
import org.myworkflows.holder.ParentClassLoaderHolder;
import org.myworkflows.service.LibraryService;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.List;
import java.util.Optional;

import static com.networknt.schema.utils.StringUtils.isBlank;
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

    private final LibraryConfig libraryConfig;
    private final LibraryService libraryService;

    private URLClassLoader urlClassLoader;

    @Order(5)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        final var jarUrls = listJarFiles().stream()
            .flatMap(file -> toUrl(file).stream())
            .toArray(URL[]::new);

        urlClassLoader = new URLClassLoader(jarUrls, getClass().getClassLoader());
        ParentClassLoaderHolder.INSTANCE.setClassLoader(urlClassLoader);
    }

    @Override
    public void unload() {
        ofNullable(urlClassLoader).ifPresent(item -> wrap(() -> {
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
            libraryService.create(Library.of(file.getAbsolutePath(), true), false);
            return Optional.of(url);
        } catch (Exception exception) {
            // One broken JAR must not abort the whole startup: it is only reported as not loaded.
            log.warn("JAR file '{}' could not be loaded.", file.getAbsolutePath(), exception);
            libraryService.create(Library.of(file.getAbsolutePath(), false), false);
            return Optional.empty();
        }
    }

}
