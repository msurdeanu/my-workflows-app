package org.myworkflows.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.config.LibraryConfig;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.io.ByteArrayInputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public final class LibraryServiceTest {

    private static final String CONTENT = "content";

    @TempDir
    private Path tempDir;

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private InternalCacheManager internalCacheManager;

    @Test
    public void whenFileNameIsPlainThenItIsStoredInsideTheBaseDirectory() throws Exception {
        // given
        final var libraryService = createLibraryService();

        // when
        libraryService.upload("my-lib.jar", new ByteArrayInputStream(CONTENT.getBytes(UTF_8)));

        // then
        assertArrayEquals(CONTENT.getBytes(UTF_8), Files.readAllBytes(baseDirectory().resolve("my-lib.jar")));
    }

    @Test
    public void whenFileNameEscapesTheBaseDirectoryThenItIsStrippedDownToTheBareName() {
        // given both separator flavours, because the name comes straight from a browser upload
        final var libraryService = createLibraryService();

        // when
        libraryService.upload("../../escaped.jar", new ByteArrayInputStream(CONTENT.getBytes(UTF_8)));
        libraryService.upload(windowsTraversingName(), new ByteArrayInputStream(CONTENT.getBytes(UTF_8)));

        // then nothing was written outside of the configured base directory
        assertFalse(Files.exists(tempDir.resolve("escaped.jar")));
        assertFalse(Files.exists(tempDir.resolve("nested").resolve("escaped.jar")));
        assertTrue(Files.exists(baseDirectory().resolve("escaped.jar")));
    }

    @Test
    public void whenFileIsNotAJarThenTheUploadIsRejected() {
        // given
        final var libraryService = createLibraryService();

        // when and then
        assertThrows(WorkflowRuntimeException.class,
            () -> libraryService.upload("evil.sh", new ByteArrayInputStream(CONTENT.getBytes(UTF_8))));
        assertThrows(WorkflowRuntimeException.class,
            () -> libraryService.upload(null, new ByteArrayInputStream(CONTENT.getBytes(UTF_8))));
    }

    private String windowsTraversingName() {
        final var separator = String.valueOf((char) 92);
        return ".." + separator + ".." + separator + "escaped.jar";
    }

    private Path baseDirectory() {
        // two levels deep, so that a "../.." traversal would still land inside the test's own temp directory
        return tempDir.resolve("nested").resolve("libs");
    }

    private LibraryService createLibraryService() {
        final var libraryConfig = new LibraryConfig();
        libraryConfig.setBaseDirectory(baseDirectory().toString());
        when(applicationManager.getBeanOfType(InternalCacheManager.class)).thenReturn(internalCacheManager);
        when(internalCacheManager.getCache(anyString()))
            .thenReturn(new InternalCache(CacheNameEnum.LIBRARY.getName()));
        final var libraryService = new LibraryService(applicationManager);
        when(applicationManager.getBeanOfType(LibraryConfig.class)).thenReturn(libraryConfig);
        return libraryService;
    }

}
