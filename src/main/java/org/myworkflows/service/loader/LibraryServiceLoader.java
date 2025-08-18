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
import java.util.Arrays;
import java.util.Objects;

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
        final var jarUrls = Arrays.stream(Objects.requireNonNull(new File(libraryConfig.getBaseDirectory())
                .listFiles((dir, name) -> name.endsWith(JAR_EXTENSION))))
            .map(file -> wrap(() -> {
                final var url = file.toURI().toURL();
                log.info("JAR file ready to be loaded: {}", file.getAbsolutePath());
                libraryService.create(Library.of(file.getAbsolutePath(), true), false);
                return url;
            }, exception -> libraryService.create(Library.of(file.getAbsolutePath(), false), false)))
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

}
