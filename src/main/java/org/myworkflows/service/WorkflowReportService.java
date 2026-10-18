package org.myworkflows.service;

import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.config.ReportConfig;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowReportFormat;
import org.myworkflows.domain.WorkflowReportWriter;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.event.WorkflowReportOnChangeEvent;
import org.myworkflows.domain.filter.WorkflowReportFilter;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.provider.SettingProvider;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * The reports the commands of the runs wrote, which are files and nothing else: a directory per run under the reports
 * directory, a file per report, named after its title. Like the libraries, they are read from the directory when the
 * application starts (see {@code WorkflowReportServiceLoader}) and kept in a cache, which the reports page reads.
 *
 * @author Mihai Surdeanu
 * @since 2.3
 */
@Slf4j
@Service
public class WorkflowReportService extends CacheableDataService<WorkflowReport, WorkflowReportFilter>
    implements ServiceCreator<WorkflowReport>, WorkflowReportWriter {

    /**
     * The title is the name of the file, which a file system keeps to 255 characters: what is left goes to the
     * extension and to the number of a title which is taken already.
     */
    public static final int MAX_TITLE_LENGTH = 150;

    private static final int DEFAULT_MAX_SIZE_IN_MB = 5;

    private static final int MAX_FILES_WITH_THE_SAME_TITLE = 1_000;

    private static final String DEFAULT_TITLE = "report";

    // what a file name cannot hold on Windows or anywhere else
    private static final Pattern FORBIDDEN_CHARS = Pattern.compile("[\\\\/:*?\"<>|\\p{Cntrl}]");

    // names Windows keeps for its devices, whatever the extension
    private static final Pattern RESERVED_NAMES = Pattern.compile("(?i)(con|prn|aux|nul|com[1-9]|lpt[1-9])(\\..*)?");

    private final InternalCache templateCache;

    public WorkflowReportService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_REPORT);
        templateCache = (InternalCache) applicationManager.getBeanOfType(InternalCacheManager.class)
            .getCache(CacheNameEnum.WORKFLOW_TEMPLATE.getName());
    }

    /**
     * Lists a report whose file is there already. It is put into the cache here rather than by an annotation, as
     * {@link #write} calls it on this object, where an annotation would do nothing.
     */
    @Override
    public WorkflowReport create(WorkflowReport workflowReport, boolean requiresPersistence) {
        cache.put(workflowReport.id(), workflowReport);
        return workflowReport;
    }

    /**
     * Writes the file under the title, or under the title followed by a number when the run has a report with this
     * title already: a file is only ever created, never overwritten, so that a report which is listed keeps its text.
     */
    @Override
    public WorkflowReport write(WorkflowRun workflowRun, String title, WorkflowReportFormat format, String content) {
        final var fileTitle = toFileTitle(requireTitle(title));
        final var bytes = requireContent(content).getBytes(StandardCharsets.UTF_8);
        final var maxSizeInMb = applicationManager.getBeanOfType(SettingProvider.class).getOrDefault("reportMaxSize", DEFAULT_MAX_SIZE_IN_MB);
        if (bytes.length > maxSizeInMb * 1024L * 1024L) {
            throw new WorkflowRuntimeException("Report '" + fileTitle + "' has " + bytes.length + " bytes, more than the " + maxSizeInMb
                + " MB a report may have. An admin can change it with the setting 'Report - Max size'.");
        }

        final var filePath = writeFile(runDirectoryOf(workflowRun.getId()), fileTitle, format.getExtension(), bytes);
        final var workflowReport = describe(filePath, workflowRun.getWorkflowTemplateId(), workflowRun.getUserId()).orElseThrow(() ->
            new WorkflowRuntimeException("Report file '" + filePath + "' was written, but it cannot be read."));
        create(workflowReport, false);
        broadcast(List.of(workflowReport), false);
        return workflowReport;
    }

    /**
     * @return the report the given file is, as the file says now, or nothing when it is not one: it is not in the
     *     directory of a run, it has no extension of a report, or it cannot be read.
     */
    public Optional<WorkflowReport> describe(Path filePath, Integer workflowTemplateId, Integer userId) {
        final var fileName = filePath.getFileName().toString();
        final var format = WorkflowReportFormat.ofFileName(fileName);
        final var workflowRunId = toRunId(filePath.getParent());
        if (format.isEmpty() || workflowRunId.isEmpty() || !Files.isRegularFile(filePath)) {
            return Optional.empty();
        }
        try {
            return Optional.of(new WorkflowReport(workflowRunId.get(), fileName, format.get(), workflowTemplateId, userId, Files.size(filePath),
                Files.getLastModifiedTime(filePath).toInstant()));
        } catch (IOException exception) {
            log.warn("Report file '{}' could not be read, so it is not listed.", filePath, exception);
            return Optional.empty();
        }
    }

    public Optional<WorkflowReport> findById(String id) {
        return Optional.ofNullable(cache.get(id, WorkflowReport.class));
    }

    public Stream<WorkflowReport> findAllOfRun(UUID workflowRunId) {
        return getAllItems().filter(workflowReport -> workflowRunId.equals(workflowReport.workflowRunId()));
    }

    /**
     * @return the template of the run of the report, unless the run had none or the template was deleted since.
     */
    public Optional<WorkflowTemplate> findWorkflowTemplate(WorkflowReport workflowReport) {
        return Optional.ofNullable(workflowReport.workflowTemplateId())
            .map(id -> templateCache.get(id, WorkflowTemplate.class));
    }

    /**
     * @return the text of the report, or nothing when its file is gone: it was deleted by hand.
     */
    public Optional<String> findContent(WorkflowReport workflowReport) {
        final var filePath = filePathOf(workflowReport);
        if (!Files.isRegularFile(filePath)) {
            return Optional.empty();
        }
        try {
            return Optional.of(Files.readString(filePath, StandardCharsets.UTF_8));
        } catch (IOException exception) {
            throw new WorkflowRuntimeException("Report '" + workflowReport.title() + "' could not be read.", exception);
        }
    }

    public void delete(WorkflowReport workflowReport) {
        final var filePath = filePathOf(workflowReport);
        try {
            // a file which is already gone is what was asked for
            Files.deleteIfExists(filePath);
        } catch (IOException exception) {
            throw new WorkflowRuntimeException("Report '" + workflowReport.title() + "' could not be deleted.", exception);
        }
        cache.evict(workflowReport.id());
        deleteDirectoryIfEmpty(filePath.getParent());
        broadcast(List.of(workflowReport), true);
    }

    /**
     * Deletes the reports of the given runs, which are deleted themselves: the directory of each run, with whatever it
     * holds.
     */
    public void deleteAllOf(Collection<UUID> workflowRunIds) {
        if (workflowRunIds.isEmpty()) {
            return;
        }
        final var runIds = new HashSet<>(workflowRunIds);
        final var deletedReports = getAllItems().filter(workflowReport -> runIds.contains(workflowReport.workflowRunId())).toList();
        deletedReports.forEach(workflowReport -> cache.evict(workflowReport.id()));
        runIds.forEach(runId -> deleteDirectory(runDirectoryOf(runId)));
        if (!deletedReports.isEmpty()) {
            broadcast(deletedReports, true);
        }
    }

    public Path filePathOf(WorkflowReport workflowReport) {
        final var runDirectory = runDirectoryOf(workflowReport.workflowRunId());
        final var filePath = runDirectory.resolve(workflowReport.fileName()).normalize();
        // the name is written by this service or found in the directory, but it is checked all the same
        if (!runDirectory.equals(filePath.getParent())) {
            throw new WorkflowRuntimeException("File '" + workflowReport.fileName() + "' is not inside the directory of its run.");
        }
        return filePath;
    }

    public Path baseDirectory() {
        return Path.of(applicationManager.getBeanOfType(ReportConfig.class).getBaseDirectory()).toAbsolutePath().normalize();
    }

    /**
     * @return the title as a file name can hold it: {@code Sales 2026/10?} is {@code Sales 2026-10-}. It is what the
     *     reports page shows, as the file name is all there is.
     */
    static String toFileTitle(String title) {
        // a line break is a space first, before what is left of the control characters is replaced
        var fileTitle = FORBIDDEN_CHARS.matcher(title.replaceAll("\\s+", " ")).replaceAll("-")
            // no hidden file, and nothing Windows would drop at the end of a name
            .replaceAll("^[.\\s]+|[.\\s]+$", "");
        if (fileTitle.isEmpty()) {
            fileTitle = DEFAULT_TITLE;
        }
        return RESERVED_NAMES.matcher(fileTitle).matches() ? fileTitle + "-" : fileTitle;
    }

    @Override
    protected WorkflowReportFilter createFilter() {
        return new WorkflowReportFilter();
    }

    private String requireTitle(String title) {
        final var reportTitle = title == null ? "" : title.strip();
        if (reportTitle.isEmpty()) {
            throw new WorkflowRuntimeException("A report needs a title: 'report.title' is empty.");
        }
        if (reportTitle.length() > MAX_TITLE_LENGTH) {
            throw new WorkflowRuntimeException("The title of a report has at most " + MAX_TITLE_LENGTH + " characters.");
        }
        return reportTitle;
    }

    private String requireContent(String content) {
        if (content == null) {
            throw new WorkflowRuntimeException("A report needs a content: 'report.content' is null.");
        }
        return content;
    }

    /**
     * Two reports of a run may have the same title: the second one is {@code title (2)}, the third one {@code title (3)}.
     */
    private Path writeFile(Path runDirectory, String fileTitle, String extension, byte[] bytes) {
        try {
            Files.createDirectories(runDirectory);
            for (int number = 1; number <= MAX_FILES_WITH_THE_SAME_TITLE; number++) {
                final var filePath = runDirectory.resolve(number == 1 ? fileTitle + extension : fileTitle + " (" + number + ")" + extension);
                try {
                    Files.write(filePath, bytes, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
                    return filePath;
                } catch (FileAlreadyExistsException alreadyExists) {
                    // the next number is tried
                }
            }
        } catch (IOException exception) {
            throw new WorkflowRuntimeException("Report file '" + fileTitle + extension + "' could not be written.", exception);
        }
        throw new WorkflowRuntimeException("The run has too many reports titled '" + fileTitle + "' already.");
    }

    /**
     * @return the run the given directory belongs to, when it is a directory of the reports directory named after a run.
     */
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

    private void broadcast(List<WorkflowReport> workflowReports, boolean deleted) {
        applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(new WorkflowReportOnChangeEvent(workflowReports, deleted));
    }

    private void deleteQuietly(Path filePath) {
        try {
            Files.deleteIfExists(filePath);
        } catch (IOException exception) {
            log.warn("Report file '{}' could not be deleted.", filePath, exception);
        }
    }

    private void deleteDirectoryIfEmpty(Path directory) {
        try (var entries = Files.list(directory)) {
            if (entries.findAny().isEmpty()) {
                Files.deleteIfExists(directory);
            }
        } catch (IOException exception) {
            // a directory which is gone already, or still used, is left as it is
            log.debug("Report directory '{}' was not deleted.", directory, exception);
        }
    }

    private void deleteDirectory(Path directory) {
        if (!Files.isDirectory(directory)) {
            return;
        }
        try (var paths = Files.walk(directory)) {
            paths.sorted(Comparator.reverseOrder()).forEach(this::deleteQuietly);
        } catch (IOException exception) {
            log.warn("Report directory '{}' could not be deleted.", directory, exception);
        }
    }

}
