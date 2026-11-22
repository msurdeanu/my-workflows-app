package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.Arrays;
import java.util.Locale;
import java.util.stream.Collectors;

/**
 * Whether a file is zipped before it is offered to download. A directory always is: a download is one file.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@Getter
@RequiredArgsConstructor
public enum WorkflowDownloadCompression {

    /**
     * Zipped when it is larger than the threshold of the command, unless it is compressed already: a {@code .gz}, a
     * {@code .zip}, a picture...
     */
    AUTO("auto"),
    ALWAYS("always"),
    NEVER("never");

    private final String label;

    /**
     * @throws WorkflowRuntimeException when no compression has this label.
     */
    public static WorkflowDownloadCompression of(String label) {
        final var wanted = label == null ? "" : label.strip().toLowerCase(Locale.ROOT);
        return Arrays.stream(values())
            .filter(compression -> compression.label.equals(wanted))
            .findFirst()
            .orElseThrow(() -> new WorkflowRuntimeException("Compression '" + label + "' is not known: 'download.compress' is one of "
                + Arrays.stream(values()).map(WorkflowDownloadCompression::getLabel).collect(Collectors.joining(", ")) + "."));
    }

}
