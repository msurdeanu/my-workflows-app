package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.Arrays;
import java.util.Locale;
import java.util.Optional;

/**
 * What the text of a report is written in. It gives the extension of its file, and how the reports page shows it.
 *
 * @author Mihai Surdeanu
 * @since 2.3
 */
@Getter
@RequiredArgsConstructor
public enum WorkflowReportFormat {
    MARKDOWN("markdown", ".md", "text/markdown"),
    HTML("html", ".html", "text/html");

    /**
     * How a workflow names the format, as the value of {@code report.format}.
     */
    private final String label;

    private final String extension;

    private final String mediaType;

    /**
     * @return the format a workflow names, whatever the case it is written in.
     * @throws WorkflowRuntimeException when the name is none of the formats.
     */
    public static WorkflowReportFormat of(String label) {
        final var normalizedLabel = label == null ? null : label.strip().toLowerCase(Locale.ROOT);
        return Arrays.stream(values())
            .filter(format -> format.label.equals(normalizedLabel))
            .findFirst()
            .orElseThrow(() -> new WorkflowRuntimeException("Report format '" + label + "' is not known: it is either 'markdown' or 'html'."));
    }

    /**
     * @return the format of a report file, told by its extension in any case, or nothing for a file which is no report.
     */
    public static Optional<WorkflowReportFormat> ofFileName(String fileName) {
        final var lowerCaseName = fileName.toLowerCase(Locale.ROOT);
        return Arrays.stream(values())
            // a file which is all extension is not a report
            .filter(format -> lowerCaseName.endsWith(format.extension) && lowerCaseName.length() > format.extension.length())
            .findFirst();
    }

}
