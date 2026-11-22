package org.myworkflows.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.config.DownloadConfig;
import org.myworkflows.domain.WorkflowDownload;
import org.myworkflows.domain.WorkflowDownloadCompression;
import org.myworkflows.domain.WorkflowDownloadRequest;
import org.myworkflows.domain.WorkflowDownloadWriter;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.download.DownloadEntry;
import org.myworkflows.domain.download.DownloadReader;
import org.myworkflows.domain.download.DownloadSource;
import org.myworkflows.domain.download.LimitedOutputStream;
import org.myworkflows.domain.download.LocalDownloadSource;
import org.myworkflows.domain.event.WorkflowDownloadOnWriteEvent;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.util.FileNameUtil;
import org.springframework.stereotype.Service;

import java.io.BufferedOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import static org.apache.commons.lang3.StringUtils.isBlank;

/**
 * The files the commands of the runs offer to download, which are files and nothing else, like the reports: a directory
 * per run under the downloads directory. What a command offers is copied there when it runs - from a text, from this
 * server or from another one - so that it can still be downloaded once the source changed or is gone, and so that
 * nothing but that directory is ever served. A file which is large enough, and every directory, is zipped on the way.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class WorkflowDownloadService implements WorkflowDownloadWriter {

    /**
     * The name of the file, which a file system keeps to 255 characters: what is left goes to the number of a name which
     * is taken already.
     */
    public static final int MAX_FILE_NAME_LENGTH = 200;

    private static final int DEFAULT_MAX_SIZE_IN_MB = 500;

    private static final long BYTES_IN_MB = 1024L * 1024L;

    private static final String DEFAULT_FILE_NAME = "download";

    private static final String ZIP_EXTENSION = ".zip";

    // a file being written is hidden until it is complete, so that nobody lists a part of it
    private static final String HIDDEN_PREFIX = ".";

    private static final String PART_SUFFIX = ".part";

    // what zipping makes hardly any smaller
    private static final Set<String> COMPRESSED_EXTENSIONS = Set.of("zip", "gz", "tgz", "bz2", "tbz2", "xz", "txz", "zst", "7z", "rar", "jar",
        "war", "ear", "apk", "png", "jpg", "jpeg", "gif", "webp", "mp3", "mp4", "mkv", "mov", "avi", "pdf", "docx", "xlsx", "pptx", "odt", "ods");

    private final ApplicationManager applicationManager;

    /**
     * Copies what the command offers into the directory of the run, under its name or under its name followed by a
     * number when the run has a file with this name already: a file is only ever created, never overwritten.
     */
    @Override
    public WorkflowDownload write(WorkflowRun workflowRun, WorkflowDownloadRequest request) {
        requireAllowed(request.source());
        final var maxSizeInMb = maxSizeInMb();
        try (var reader = request.source().open()) {
            final var entries = requireEntries(reader);
            final var zip = shouldZip(reader, entries, request);
            final var fileName = toFileName(request.fileName(), reader.name(), zip);
            if (!zip && entries.getFirst().size() > maxSizeInMb * BYTES_IN_MB) {
                throw tooLarge(fileName, maxSizeInMb);
            }
            final var filePath = writeFile(runDirectoryOf(workflowRun.getId()), fileName, reader, entries, zip, maxSizeInMb);
            final var workflowDownload = describe(filePath).orElseThrow(() ->
                new WorkflowRuntimeException("Download file '" + filePath + "' was written, but it cannot be read."));
            applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(new WorkflowDownloadOnWriteEvent(workflowDownload));
            return workflowDownload;
        } catch (IOException exception) {
            // a run shows the root cause of its failure, which would leave out where the source was: it is logged instead
            log.warn("Run '{}' could not offer a file to download.", workflowRun.getId(), exception);
            throw new WorkflowRuntimeException("Nothing could be offered to download: " + exception.getMessage());
        }
    }

    /**
     * @return the files the run offers, the oldest first.
     */
    public List<WorkflowDownload> findAllOfRun(UUID workflowRunId) {
        final var runDirectory = runDirectoryOf(workflowRunId);
        if (!Files.isDirectory(runDirectory)) {
            return List.of();
        }
        try (var files = Files.list(runDirectory)) {
            return files
                .flatMap(file -> describe(file).stream())
                .sorted(Comparator.comparing(WorkflowDownload::created).thenComparing(WorkflowDownload::fileName))
                .toList();
        } catch (IOException exception) {
            log.warn("Download directory '{}' could not be read, so its files are not listed.", runDirectory, exception);
            return List.of();
        }
    }

    /**
     * @return the file the run offers under this name. It is found among the ones which are listed, never by reading
     *     the path it is asked for from the disk.
     */
    public Optional<WorkflowDownload> find(UUID workflowRunId, String fileName) {
        return findAllOfRun(workflowRunId).stream()
            .filter(workflowDownload -> workflowDownload.fileName().equals(fileName))
            .findFirst();
    }

    /**
     * @return what the file holds, as a stream the caller closes.
     */
    public InputStream openStream(WorkflowDownload workflowDownload) throws IOException {
        return Files.newInputStream(filePathOf(workflowDownload));
    }

    /**
     * Deletes the files of the given runs, which are deleted themselves: the directory of each run, with whatever it
     * holds.
     */
    public void deleteAllOf(Collection<UUID> workflowRunIds) {
        workflowRunIds.forEach(runId -> deleteDirectory(runDirectoryOf(runId)));
    }

    public Path baseDirectory() {
        return Path.of(applicationManager.getBeanOfType(DownloadConfig.class).getBaseDirectory()).toAbsolutePath().normalize();
    }

    /**
     * @return whether a file with this name is compressed already, by its extension.
     */
    static boolean isCompressedAlready(String fileName) {
        final var dot = fileName.lastIndexOf('.');
        return dot >= 0 && COMPRESSED_EXTENSIONS.contains(fileName.substring(dot + 1).toLowerCase(Locale.ROOT));
    }

    /**
     * @return the name without its extension and the extension, {@code .tar.gz} being one.
     */
    static String[] splitExtension(String fileName) {
        var dot = fileName.lastIndexOf('.');
        if (dot <= 0) {
            return new String[] {fileName, ""};
        }
        if (fileName.substring(0, dot).toLowerCase(Locale.ROOT).endsWith(".tar") && dot > 4) {
            dot -= 4;
        }
        return new String[] {fileName.substring(0, dot), fileName.substring(dot)};
    }

    private Path filePathOf(WorkflowDownload workflowDownload) {
        final var runDirectory = runDirectoryOf(workflowDownload.workflowRunId());
        final var filePath = runDirectory.resolve(workflowDownload.fileName()).normalize();
        // the name is written by this service or found in the directory, but it is checked all the same
        if (!runDirectory.equals(filePath.getParent())) {
            throw new WorkflowRuntimeException("File '" + workflowDownload.fileName() + "' is not inside the directory of its run.");
        }
        return filePath;
    }

    /**
     * A file of this server is read only from the directories which are allowed, when some are: by where it really is,
     * so that a link cannot lead out of them.
     */
    private void requireAllowed(DownloadSource source) {
        final var allowedDirectories = applicationManager.getBeanOfType(DownloadConfig.class).getAllowedDirectories();
        if (!(source instanceof LocalDownloadSource(var path)) || allowedDirectories.isEmpty() || !Files.exists(path)) {
            return;
        }
        final var realPath = toRealPath(path);
        final var allowed = allowedDirectories.stream()
            .map(Path::of)
            .filter(Files::isDirectory)
            .map(this::toRealPath)
            .anyMatch(realPath::startsWith);
        if (!allowed) {
            throw new WorkflowRuntimeException("'" + path + "' is not in a directory a download may read from. An admin lists them in "
                + "'my-workflows.config.download.allowed-directories'.");
        }
    }

    private Path toRealPath(Path path) {
        try {
            return path.toRealPath();
        } catch (IOException exception) {
            throw new WorkflowRuntimeException("'" + path + "' cannot be read: " + exception.getMessage(), exception);
        }
    }

    private List<DownloadEntry> requireEntries(DownloadReader reader) throws IOException {
        final var entries = reader.entries();
        if (entries.isEmpty()) {
            throw new WorkflowRuntimeException("Directory '" + reader.name() + "' has no file to offer.");
        }
        return entries;
    }

    private boolean shouldZip(DownloadReader reader, List<DownloadEntry> entries, WorkflowDownloadRequest request) {
        if (reader.isDirectory()) {
            if (request.compression() == WorkflowDownloadCompression.NEVER) {
                throw new WorkflowRuntimeException("'" + reader.name() + "' is a directory, which is offered as a zip archive: "
                    + "'download.compress' cannot be 'never' for it.");
            }
            return true;
        }
        return switch (request.compression()) {
            case ALWAYS -> true;
            case NEVER -> false;
            case AUTO -> !isCompressedAlready(reader.name()) && entries.getFirst().size() > request.compressAbove();
        };
    }

    private String toFileName(String requestedFileName, String sourceName, boolean zip) {
        final var wantedName = isBlank(requestedFileName) ? sourceName : requestedFileName.strip();
        if (wantedName.length() > MAX_FILE_NAME_LENGTH) {
            throw new WorkflowRuntimeException("The name of a file to download has at most " + MAX_FILE_NAME_LENGTH + " characters.");
        }
        final var fileName = FileNameUtil.toFileName(wantedName, DEFAULT_FILE_NAME);
        return zip && !fileName.toLowerCase(Locale.ROOT).endsWith(ZIP_EXTENSION) ? fileName + ZIP_EXTENSION : fileName;
    }

    /**
     * The file is written under a hidden name first, and given its own once it is complete: what fails or is cancelled
     * on the way leaves nothing behind.
     */
    private Path writeFile(Path runDirectory, String fileName, DownloadReader reader, List<DownloadEntry> entries, boolean zip,
                           long maxSizeInMb) throws IOException {
        Files.createDirectories(runDirectory);
        final var partPath = runDirectory.resolve(HIDDEN_PREFIX + UUID.randomUUID() + PART_SUFFIX);
        final var outputStream = new LimitedOutputStream(new BufferedOutputStream(Files.newOutputStream(partPath, StandardOpenOption.CREATE_NEW,
            StandardOpenOption.WRITE)), maxSizeInMb * BYTES_IN_MB);
        try {
            if (zip) {
                writeZip(outputStream, reader, entries);
            } else {
                writeOne(outputStream, reader, entries.getFirst());
            }
            final var nameParts = splitExtension(fileName);
            return FileNameUtil.createUnderFreeName(runDirectory, nameParts[0], nameParts[1], filePath -> Files.move(partPath, filePath))
                .orElseThrow(() -> new WorkflowRuntimeException("The run offers too many files named '" + fileName + "' already."));
        } catch (IOException exception) {
            // whoever read the source may have wrapped what stopped the stream, which says itself why it stopped
            if (outputStream.isInterrupted()) {
                throw new WorkflowRuntimeException("Download '" + fileName + "' was stopped, as the run was cancelled.", exception);
            }
            if (outputStream.isLimitExceeded()) {
                throw tooLarge(fileName, maxSizeInMb);
            }
            throw exception;
        } finally {
            Files.deleteIfExists(partPath);
        }
    }

    private static void writeOne(LimitedOutputStream outputStream, DownloadReader reader, DownloadEntry entry) throws IOException {
        try (outputStream) {
            reader.copy(entry, new ShieldedOutputStream(outputStream));
        }
    }

    private static void writeZip(LimitedOutputStream outputStream, DownloadReader reader, List<DownloadEntry> entries) throws IOException {
        try (var zipOutputStream = new ZipOutputStream(outputStream)) {
            final var shielded = new ShieldedOutputStream(zipOutputStream);
            for (var entry : entries) {
                zipOutputStream.putNextEntry(new ZipEntry(entry.path()));
                reader.copy(entry, shielded);
                zipOutputStream.closeEntry();
            }
        }
    }

    private static WorkflowRuntimeException tooLarge(String fileName, long maxSizeInMb) {
        return new WorkflowRuntimeException("Download '" + fileName + "' is larger than the " + maxSizeInMb
            + " MB a run may offer. An admin can change it with the setting 'Download - Max size'.");
    }

    /**
     * @return the file the given path is, as the file says now, or nothing when it is not one: it is not in the
     *     directory of a run, it is hidden - being written -, or it cannot be read.
     */
    private Optional<WorkflowDownload> describe(Path filePath) {
        final var fileName = filePath.getFileName().toString();
        final var workflowRunId = toRunId(filePath.getParent());
        if (fileName.startsWith(HIDDEN_PREFIX) || workflowRunId.isEmpty() || !Files.isRegularFile(filePath)) {
            return Optional.empty();
        }
        try {
            final var size = Files.size(filePath);
            final var created = Files.getLastModifiedTime(filePath).toInstant();
            if (!fileName.toLowerCase(Locale.ROOT).endsWith(ZIP_EXTENSION)) {
                return Optional.of(new WorkflowDownload(workflowRunId.get(), fileName, size, created, null, null));
            }
            return Optional.of(describeZip(filePath, new WorkflowDownload(workflowRunId.get(), fileName, size, created, null, null)));
        } catch (IOException exception) {
            log.warn("Download file '{}' could not be read, so it is not listed.", filePath, exception);
            return Optional.empty();
        }
    }

    /**
     * @return the file with what its zip archive holds, read from its list of files alone, or as it is when it cannot be
     *     read as a zip archive.
     */
    private static WorkflowDownload describeZip(Path filePath, WorkflowDownload workflowDownload) {
        try (var zipFile = new ZipFile(filePath.toFile())) {
            final var files = zipFile.stream().filter(entry -> !entry.isDirectory()).toList();
            final var contentSize = files.stream().mapToLong(entry -> Math.max(entry.getSize(), 0)).sum();
            return new WorkflowDownload(workflowDownload.workflowRunId(), workflowDownload.fileName(), workflowDownload.size(),
                workflowDownload.created(), files.size(), contentSize);
        } catch (IOException notAZip) {
            return workflowDownload;
        }
    }

    private Optional<UUID> toRunId(Path directory) {
        if (directory == null || !baseDirectory().equals(directory.toAbsolutePath().normalize().getParent())) {
            return Optional.empty();
        }
        try {
            return Optional.of(UUID.fromString(directory.getFileName().toString()));
        } catch (IllegalArgumentException notARun) {
            return Optional.empty();
        }
    }

    private Path runDirectoryOf(UUID workflowRunId) {
        return baseDirectory().resolve(workflowRunId.toString()).normalize();
    }

    private long maxSizeInMb() {
        return applicationManager.getBeanOfType(SettingProvider.class).getOrDefault("downloadMaxSize", DEFAULT_MAX_SIZE_IN_MB);
    }

    private void deleteDirectory(Path directory) {
        if (!Files.isDirectory(directory)) {
            return;
        }
        try (var paths = Files.walk(directory)) {
            paths.sorted(Comparator.reverseOrder()).forEach(this::deleteQuietly);
        } catch (IOException exception) {
            log.warn("Download directory '{}' could not be deleted.", directory, exception);
        }
    }

    private void deleteQuietly(Path filePath) {
        try {
            Files.deleteIfExists(filePath);
        } catch (IOException exception) {
            log.warn("Download file '{}' could not be deleted.", filePath, exception);
        }
    }

    /**
     * Lets whoever reads a source write to the stream, but not close it: a zip archive has an entry after this one.
     */
    private static final class ShieldedOutputStream extends FilterOutputStream {

        ShieldedOutputStream(OutputStream outputStream) {
            super(outputStream);
        }

        @Override
        public void write(byte[] bytes, int offset, int length) throws IOException {
            out.write(bytes, offset, length);
        }

        @Override
        public void close() throws IOException {
            flush();
        }

    }

}
