package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Path;
import java.util.Optional;
import java.util.regex.Pattern;

/**
 * The name of a file which a run writes - a report, a download - made out of a text the run chose, and a name which no
 * other file of the run has.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class FileNameUtil {

    /**
     * How many files of a run may share a name before the next one is refused.
     */
    public static final int MAX_FILES_WITH_THE_SAME_NAME = 1_000;

    // what a file name cannot hold on Windows or anywhere else
    private static final Pattern FORBIDDEN_CHARS = Pattern.compile("[\\\\/:*?\"<>|\\p{Cntrl}]");

    // names Windows keeps for its devices, whatever the extension
    private static final Pattern RESERVED_NAMES = Pattern.compile("(?i)(con|prn|aux|nul|com[1-9]|lpt[1-9])(\\..*)?");

    /**
     * @return the text as a file name can hold it: {@code Sales 2026/10?} is {@code Sales 2026-10-}, and a text which is
     *     left with nothing is the given fallback.
     */
    public static String toFileName(String text, String fallback) {
        // a line break is a space first, before what is left of the control characters is replaced
        var fileName = FORBIDDEN_CHARS.matcher(text.replaceAll("\\s+", " ")).replaceAll("-")
            // no hidden file, and nothing Windows would drop at the end of a name
            .replaceAll("^[.\\s]+|[.\\s]+$", "");
        if (fileName.isEmpty()) {
            fileName = fallback;
        }
        return RESERVED_NAMES.matcher(fileName).matches() ? fileName + "-" : fileName;
    }

    /**
     * @return the name a file gets when its run has one with this name already: {@code orders.csv} is
     *     {@code orders (2).csv} the second time, {@code orders (3).csv} the third.
     */
    public static String numbered(String baseName, String extension, int number) {
        return number == 1 ? baseName + extension : baseName + " (" + number + ")" + extension;
    }

    /**
     * Gives a file the first of {@code name}, {@code name (2)}, {@code name (3)}... which the directory does not hold:
     * a file is only ever created there, never overwritten.
     *
     * @param creator creates the file under the given path, and throws {@link FileAlreadyExistsException} when there is
     *     one already.
     * @return where the file was created, or nothing when every name is taken.
     */
    public static Optional<Path> createUnderFreeName(Path directory, String baseName, String extension, PathCreator creator)
        throws IOException {
        for (int number = 1; number <= MAX_FILES_WITH_THE_SAME_NAME; number++) {
            final var filePath = directory.resolve(numbered(baseName, extension, number));
            try {
                creator.create(filePath);
                return Optional.of(filePath);
            } catch (FileAlreadyExistsException alreadyExists) {
                // the next number is tried
            }
        }
        return Optional.empty();
    }

    /**
     * Creates a file under a path, unless there is one.
     */
    @FunctionalInterface
    public interface PathCreator {

        void create(Path filePath) throws IOException;

    }

}
