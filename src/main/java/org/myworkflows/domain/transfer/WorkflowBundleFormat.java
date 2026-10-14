package org.myworkflows.domain.transfer;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * The two files an export can write. Both hold the same bundle: one text which is read at once, or an archive with a
 * file per definition and per template, which is what a repository would keep.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Getter
@RequiredArgsConstructor
public enum WorkflowBundleFormat {
    YAML(".yaml", "application/yaml"),
    ZIP(".zip", "application/zip");

    private final String extension;

    private final String contentType;

}
