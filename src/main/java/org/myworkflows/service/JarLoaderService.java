package org.myworkflows.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.config.LoaderConfig;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.ParentClassLoaderHolder;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.LinkedHashSet;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public final class JarLoaderService implements LoaderService {

    private final LoaderConfig loaderConfig;

    private URLClassLoader urlClassLoader;

    @Order(5)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        final var jarsAsSet = new LinkedHashSet<>(loaderConfig.getJars());
        final var jarUrls = jarsAsSet.stream()
            .map(File::new)
            .filter(File::exists)
            .map(file -> {
                try {
                    final var url = file.toURI().toURL();
                    log.info("JAR file ready to be loaded: {}", file.getAbsolutePath());
                    return url;
                } catch (MalformedURLException e) {
                    throw new WorkflowRuntimeException(e);
                }
            })
            .toArray(URL[]::new);

        urlClassLoader = new URLClassLoader(jarUrls, getClass().getClassLoader());
        ParentClassLoaderHolder.INSTANCE.setClassLoader(urlClassLoader);
    }

    @Override
    public void unload() {
        ofNullable(urlClassLoader).ifPresent(item -> {
            try {
                item.close();
                ParentClassLoaderHolder.INSTANCE.resetClassLoaderToDefault();
            } catch (IOException e) {
                throw new WorkflowRuntimeException(e);
            }
        });
    }

}
