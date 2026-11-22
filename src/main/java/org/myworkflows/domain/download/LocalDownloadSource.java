package org.myworkflows.domain.download;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

/**
 * A file or a directory of the server this application runs on. A link inside a directory is left out, so that what is
 * downloaded is what the directory holds and nothing it points to.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public record LocalDownloadSource(Path path) implements DownloadSource {

    /**
     * @throws NoSuchFileException when there is nothing under the path.
     */
    @Override
    public DownloadReader open() throws IOException {
        if (!Files.exists(path)) {
            throw new NoSuchFileException(path.toString(), null, "there is no file or directory here");
        }
        final var root = path.toRealPath();
        final var name = Optional.ofNullable(root.getFileName()).map(Path::toString).orElse("root");
        final var directory = Files.isDirectory(root);
        return new DownloadReader() {

            @Override
            public String name() {
                return name;
            }

            @Override
            public boolean isDirectory() {
                return directory;
            }

            @Override
            public List<DownloadEntry> entries() throws IOException {
                return directory ? walk(root, name) : List.of(new DownloadEntry(root.toString(), name, Files.size(root)));
            }

            @Override
            public void copy(DownloadEntry entry, OutputStream outputStream) throws IOException {
                try (var inputStream = Files.newInputStream(Path.of(entry.location()), LinkOption.NOFOLLOW_LINKS)) {
                    inputStream.transferTo(outputStream);
                }
            }
        };
    }

    /**
     * @return the regular files of the directory and of the directories in it, by their path.
     */
    private static List<DownloadEntry> walk(Path root, String name) throws IOException {
        // a walk does not follow the links, and neither does the test of a regular file here
        try (var paths = Files.walk(root)) {
            return paths
                .filter(file -> Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS))
                .sorted()
                .map(file -> new DownloadEntry(file.toString(), name + "/" + toZipPath(root.relativize(file)), sizeOf(file)))
                .toList();
        } catch (UncheckedIOException exception) {
            throw exception.getCause();
        }
    }

    private static String toZipPath(Path relativePath) {
        final var names = new StringBuilder();
        relativePath.forEach(part -> names.append(names.isEmpty() ? "" : "/").append(part));
        return names.toString();
    }

    private static long sizeOf(Path file) {
        try {
            return Files.size(file);
        } catch (IOException exception) {
            throw new UncheckedIOException(exception);
        }
    }

}
