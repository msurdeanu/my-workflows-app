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
import org.myworkflows.domain.Library;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.service.loader.LibraryServiceLoader;

import java.io.ByteArrayInputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
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

    @Mock
    private LibraryServiceLoader libraryServiceLoader;

    private final InternalCache cache = new InternalCache(CacheNameEnum.LIBRARY.getName());

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

    @Test
    public void whenLibraryIsDeletedThenItsFileIsRemovedBetweenAnUnloadAndALoad() throws Exception {
        // given
        final var libraryService = createLibraryService();
        final var library = createLibrary(baseDirectory().resolve("my-lib.jar"));

        // when
        libraryService.delete(library);

        // then
        assertFalse(Files.exists(Path.of(library.filePath())));
        assertNull(cache.get(library.filePath()));
        final var inOrder = inOrder(libraryServiceLoader);
        inOrder.verify(libraryServiceLoader).unload();
        inOrder.verify(libraryServiceLoader).load();
    }

    @Test
    public void whenFileIsAlreadyGoneThenTheLibraryIsStillTakenOffTheList() throws Exception {
        // given a library whose file was removed by hand
        final var libraryService = createLibraryService();
        final var library = createLibrary(baseDirectory().resolve("my-lib.jar"));
        Files.delete(Path.of(library.filePath()));

        // when
        libraryService.delete(library);

        // then
        assertNull(cache.get(library.filePath()));
        verify(libraryServiceLoader).load();
    }

    @Test
    public void whenFileIsNotALibraryOfTheBaseDirectoryThenNothingIsDeleted() throws Exception {
        // given files next to the libraries, under them or among them without being one
        final var libraryService = createLibraryService();
        final var outside = createLibrary(tempDir.resolve("nested").resolve("outside.jar"));
        final var below = createLibrary(baseDirectory().resolve("below").resolve("below.jar"));
        final var notAJar = createLibrary(baseDirectory().resolve("notes.txt"));
        final var traversing = Library.of(baseDirectory().resolve("..").resolve("outside.jar").toString(), true);

        // when and then
        for (var library : List.of(outside, below, notAJar, traversing)) {
            assertThrows(WorkflowRuntimeException.class, () -> libraryService.delete(library));
        }
        assertTrue(Files.exists(Path.of(outside.filePath())));
        assertTrue(Files.exists(Path.of(below.filePath())));
        assertTrue(Files.exists(Path.of(notAJar.filePath())));
        assertNotNull(cache.get(outside.filePath()));
        verify(libraryServiceLoader, never()).unload();
        verify(libraryServiceLoader, never()).load();
    }

    @Test
    public void whenLibrariesCannotBeUnloadedThenTheFileIsKeptAndTheLibrariesAreLoadedAgain() throws Exception {
        // given
        final var libraryService = createLibraryService();
        final var library = createLibrary(baseDirectory().resolve("my-lib.jar"));
        doThrow(new WorkflowRuntimeException("still in use")).when(libraryServiceLoader).unload();

        // when and then
        assertThrows(WorkflowRuntimeException.class, () -> libraryService.delete(library));
        assertTrue(Files.exists(Path.of(library.filePath())));
        assertNotNull(cache.get(library.filePath()));
        verify(libraryServiceLoader).load();
        // and the next delete is not kept waiting by the one which failed
        doNothing().when(libraryServiceLoader).unload();
        libraryService.delete(library);
        assertFalse(Files.exists(Path.of(library.filePath())));
    }

    /**
     * @return a library which is listed, as it is once the libraries were loaded, with a file of its own.
     */
    private Library createLibrary(Path filePath) throws Exception {
        Files.createDirectories(filePath.getParent());
        Files.writeString(filePath, CONTENT);
        final var library = Library.of(filePath.toString(), true);
        cache.put(library.filePath(), library);
        return library;
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
        when(internalCacheManager.getCache(anyString())).thenReturn(cache);
        final var libraryService = new LibraryService(applicationManager);
        when(applicationManager.getBeanOfType(LibraryConfig.class)).thenReturn(libraryConfig);
        lenient().when(applicationManager.getBeanOfType(LibraryServiceLoader.class)).thenReturn(libraryServiceLoader);
        return libraryService;
    }

}
