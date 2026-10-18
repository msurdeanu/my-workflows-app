package org.myworkflows.service.loader;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.service.WorkflowReportService;
import org.myworkflows.service.WorkflowRunService;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Reads the reports from the reports directory, the way the libraries are read from theirs: every Markdown or HTML file
 * of a directory named after a run is a report, the ones copied there by hand included. It runs once the runs are loaded,
 * and the oldest of them trimmed together with their reports, so that a report finds the template and the user of its
 * run. A report whose run is not known anymore is listed all the same.
 *
 * @author Mihai Surdeanu
 * @since 2.3
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class WorkflowReportServiceLoader implements ServiceLoader {

    private final ApplicationManager applicationManager;

    @Order(110)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        final var service = applicationManager.getBeanOfType(WorkflowReportService.class);
        final var baseDirectory = service.baseDirectory();
        if (!Files.isDirectory(baseDirectory)) {
            log.info("Report base directory '{}' does not exist yet, so no report is listed.", baseDirectory);
            return;
        }

        final Map<UUID, WorkflowRun> runs = applicationManager.getBeanOfType(WorkflowRunService.class).getAllItems()
            .collect(Collectors.toMap(WorkflowRun::getId, Function.identity(), (first, _) -> first));
        final var workflowReports = new ArrayList<WorkflowReport>();
        try (var directories = Files.list(baseDirectory)) {
            directories.filter(Files::isDirectory).forEach(directory -> workflowReports.addAll(readDirectory(service, directory, runs)));
        } catch (IOException exception) {
            log.warn("Report base directory '{}' could not be read, so no report is listed.", baseDirectory, exception);
            return;
        }
        // the oldest first, so that the newest one ends up first in the cache
        workflowReports.stream()
            .sorted(Comparator.comparing(WorkflowReport::created))
            .forEach(workflowReport -> service.create(workflowReport, false));
        log.info("{} report(s) listed from '{}'.", workflowReports.size(), baseDirectory);
    }

    private List<WorkflowReport> readDirectory(WorkflowReportService service, Path directory, Map<UUID, WorkflowRun> runs) {
        final var run = parseRunId(directory).map(runs::get);
        try (var files = Files.list(directory)) {
            return files
                .flatMap(file -> service.describe(file, run.map(WorkflowRun::getWorkflowTemplateId).orElse(null),
                    run.map(WorkflowRun::getUserId).orElse(null)).stream())
                .toList();
        } catch (IOException exception) {
            log.warn("Report directory '{}' could not be read, so its reports are not listed.", directory, exception);
            return List.of();
        }
    }

    private Optional<UUID> parseRunId(Path directory) {
        try {
            return Optional.of(UUID.fromString(directory.getFileName().toString()));
        } catch (IllegalArgumentException notARun) {
            return Optional.empty();
        }
    }

}
