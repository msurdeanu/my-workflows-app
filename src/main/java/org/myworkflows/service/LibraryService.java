package org.myworkflows.service;

import org.apache.commons.lang3.StringUtils;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.config.LibraryConfig;
import org.myworkflows.domain.Library;
import org.myworkflows.domain.filter.LibraryFilter;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.service.loader.LibraryServiceLoader;
import org.springframework.cache.annotation.CachePut;
import org.springframework.stereotype.Service;

import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Service
public class LibraryService extends CacheableDataService<Library, LibraryFilter> implements ServiceCreator<Library> {

    private static final int BUFFER_SIZE = 4096;

    public LibraryService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.LIBRARY);
    }

    @Override
    @CachePut(cacheNames = CacheNameEnum.LIBRARY_NAME, key = "#result.filePath")
    public Library create(Library library, boolean requiresPersistence) {
        return library;
    }

    public void upload(String fileName, InputStream inputStream) {
        final var libraryConfig = applicationManager.getBeanOfType(LibraryConfig.class);
        final var baseDirectory = Path.of(libraryConfig.getBaseDirectory()).toAbsolutePath().normalize();
        final var targetPath = baseDirectory.resolve(sanitizeFileName(fileName)).normalize();
        // A file name such as "../../evil.jar" must not be able to escape the configured base directory.
        if (!targetPath.startsWith(baseDirectory)) {
            throw new WorkflowRuntimeException("File name '" + fileName + "' resolves outside of the library base directory.");
        }

        try {
            Files.createDirectories(baseDirectory);
            try (OutputStream outputStream = Files.newOutputStream(targetPath)) {
                byte[] buffer = new byte[BUFFER_SIZE];
                int bytesRead;
                while ((bytesRead = inputStream.read(buffer)) != -1) {
                    outputStream.write(buffer, 0, bytesRead);
                }
            }
            reloadLibrariesIfNeeded(libraryConfig);
        } catch (Exception exception) {
            throw new WorkflowRuntimeException(exception);
        }
    }

    private String sanitizeFileName(String fileName) {
        final var name = ofNullable(fileName)
            .map(item -> item.substring(Math.max(item.lastIndexOf('/'), item.lastIndexOf('\\')) + 1))
            .orElse(StringUtils.EMPTY);
        if (StringUtils.isBlank(name) || !name.endsWith(LibraryConfig.JAR_EXTENSION)) {
            throw new WorkflowRuntimeException("Only '" + LibraryConfig.JAR_EXTENSION + "' files can be uploaded as libraries.");
        }
        return name;
    }

    @Override
    protected LibraryFilter createFilter() {
        return new LibraryFilter();
    }

    private void reloadLibrariesIfNeeded(LibraryConfig libraryConfig) {
        if (libraryConfig.isReloadAfterUpload()) {
            final var libraryServiceLoader = applicationManager.getBeanOfType(LibraryServiceLoader.class);
            libraryServiceLoader.unload();
            libraryServiceLoader.load();
        }
    }

}
