package org.myworkflows.domain;

import java.nio.file.Path;
import java.time.Instant;
import java.util.List;

/**
 * A JAR file of the libraries directory, as it was found when the libraries were loaded: what is said here about the
 * file - its size, its date, its checksum - is what was loaded, even if the file was replaced since.
 *
 * @param commands the names of the command types the library brings, to be used as the {@code class} of a command.
 * @param size the size of the file in bytes, or {@link #UNKNOWN_SIZE} when the file could not be read.
 * @param modified when the file was last written, or nothing when the file could not be read.
 * @param checksum the SHA-256 digest of the file in hexadecimal, or nothing when the file could not be read.
 * @author Mihai Surdeanu
 * @since 1.0
 */
public record Library(String filePath, boolean isLoaded, List<String> commands, long size, Instant modified, String checksum) {

    public static final long UNKNOWN_SIZE = -1;

    public static Library of(String filePath, boolean isLoaded) {
        return of(filePath, isLoaded, List.of());
    }

    public static Library of(String filePath, boolean isLoaded, List<String> commands) {
        return new Library(filePath, isLoaded, commands, UNKNOWN_SIZE, null, null);
    }

    public Library withFileFacts(long newSize, Instant newModified, String newChecksum) {
        return new Library(filePath, isLoaded, commands, newSize, newModified, newChecksum);
    }

    /**
     * @return the name of the file, without the directory it is kept in.
     */
    public String fileName() {
        return Path.of(filePath).getFileName().toString();
    }

    public boolean hasFileFacts() {
        return size != UNKNOWN_SIZE && modified != null && checksum != null;
    }

}
