package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.config.LibraryConfig;
import org.myworkflows.domain.Library;
import org.myworkflows.domain.filter.LibraryFilter;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.service.loader.LibraryServiceLoader;
import org.springframework.cache.annotation.CachePut;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public class LibraryService extends CacheableDataService<Library, LibraryFilter> implements ServiceCreator<Library> {

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

        try (OutputStream outputStream = new FileOutputStream(libraryConfig.getBaseDirectory() + File.separator + fileName)) {
            byte[] buffer = new byte[4096];
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
            }
            reloadLibrariesIfNeeded(libraryConfig);
        } catch (Exception exception) {
            throw new WorkflowRuntimeException(exception);
        }
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
