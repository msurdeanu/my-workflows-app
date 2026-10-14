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

import java.io.IOException;
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

    /**
     * Deletes the file of a library, which is not listed anymore from then on. The libraries are unloaded before and
     * loaded again afterwards, whatever {@code reload-after-upload} says: a JAR which is loaded cannot be deleted on
     * Windows, and anywhere else its classes would go missing one by one, as they are asked for.
     */
    public void delete(Library library) {
        final var libraryConfig = applicationManager.getBeanOfType(LibraryConfig.class);
        final var baseDirectory = Path.of(libraryConfig.getBaseDirectory()).toAbsolutePath().normalize();
        final var filePath = Path.of(library.filePath()).toAbsolutePath().normalize();
        // Whatever the page is told to delete, nothing but a library of the configured base directory goes away.
        if (!baseDirectory.equals(filePath.getParent()) || !filePath.getFileName().toString().endsWith(LibraryConfig.JAR_EXTENSION)) {
            throw new WorkflowRuntimeException("File '" + library.filePath() + "' is not a library of the base directory.");
        }

        final var libraryServiceLoader = applicationManager.getBeanOfType(LibraryServiceLoader.class);
        lock.lock();
        try {
            libraryServiceLoader.unload();
            deleteFile(library, filePath);
        } finally {
            // the other libraries come back even when this one could not be deleted
            try {
                libraryServiceLoader.load();
            } finally {
                lock.unlock();
            }
        }
    }

    private void deleteFile(Library library, Path filePath) {
        try {
            // a file which is already gone is what was asked for
            Files.deleteIfExists(filePath);
            cache.evict(library.filePath());
        } catch (IOException exception) {
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
            // one reload at a time, a delete included: the loader keeps a single class loader
            lock.lock();
            try {
                libraryServiceLoader.unload();
                libraryServiceLoader.load();
            } finally {
                lock.unlock();
            }
        }
    }

}
