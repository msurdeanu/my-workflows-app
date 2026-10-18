package org.myworkflows.service.loader;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.config.ReportConfig;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.service.WorkflowReportService;
import org.myworkflows.service.WorkflowRunService;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.3
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowReportServiceLoaderTest {

    @TempDir
    private Path tempDir;

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private InternalCacheManager internalCacheManager;

    @Mock
    private WorkflowRunService workflowRunService;

    private final InternalCache reportCache = new InternalCache(CacheNameEnum.WORKFLOW_REPORT.getName(), Integer.MAX_VALUE,
        InternalCache.InternalCacheOrder.LIFO);

    private WorkflowReportService workflowReportService;

    @BeforeEach
    public void init() {
        final var reportConfig = new ReportConfig();
        reportConfig.setBaseDirectory(tempDir.resolve("reports").toString());
        when(applicationManager.getBeanOfType(InternalCacheManager.class)).thenReturn(internalCacheManager);
        when(internalCacheManager.getCache(CacheNameEnum.WORKFLOW_REPORT.getName())).thenReturn(reportCache);
        when(internalCacheManager.getCache(CacheNameEnum.WORKFLOW_TEMPLATE.getName()))
            .thenReturn(new InternalCache(CacheNameEnum.WORKFLOW_TEMPLATE.getName()));
        when(applicationManager.getBeanOfType(ReportConfig.class)).thenReturn(reportConfig);
        workflowReportService = new WorkflowReportService(applicationManager);
        when(applicationManager.getBeanOfType(WorkflowReportService.class)).thenReturn(workflowReportService);
    }

    @Test
    public void whenReportsDirectoryHoldsReportsThenTheyAreListedNewestFirstWithWhatTheirRunsSay() throws Exception {
        // given a run which is known, and one which is not anymore
        final var knownRun = new WorkflowRun(9, null, 2);
        final var goneRunId = UUID.randomUUID();
        final var older = write(knownRun.getId(), "Weekly sales.md", Instant.parse("2026-10-01T10:00:00Z"));
        final var newer = write(goneRunId, "Health.html", Instant.parse("2026-10-02T10:00:00Z"));
        write(knownRun.getId(), "notes.txt", Instant.parse("2026-10-03T10:00:00Z"));
        Files.createDirectories(tempDir.resolve("reports").resolve("not-a-run"));
        Files.writeString(tempDir.resolve("reports").resolve("not-a-run").resolve("Stray.md"), "text");
        when(applicationManager.getBeanOfType(WorkflowRunService.class)).thenReturn(workflowRunService);
        when(workflowRunService.getAllItems()).thenReturn(Stream.of(knownRun));

        // when
        new WorkflowReportServiceLoader(applicationManager).load();

        // then
        final var workflowReports = workflowReportService.getAll().toList();
        assertEquals(List.of(newer, older), workflowReports.stream().map(WorkflowReport::fileName).toList());
        assertEquals(9, workflowReports.get(1).workflowTemplateId());
        assertEquals(2, workflowReports.get(1).userId());
        assertNull(workflowReports.get(0).workflowTemplateId());
        assertNull(workflowReports.get(0).userId());
    }

    @Test
    public void whenReportsDirectoryDoesNotExistThenNothingIsListed() {
        // when
        new WorkflowReportServiceLoader(applicationManager).load();

        // then
        assertEquals(0, workflowReportService.getAllSize());
    }

    private String write(UUID runId, String fileName, Instant modified) throws Exception {
        final var directory = Files.createDirectories(tempDir.resolve("reports").resolve(runId.toString()));
        final var file = Files.writeString(directory.resolve(fileName), "text");
        Files.setLastModifiedTime(file, FileTime.from(modified));
        return fileName;
    }

}
