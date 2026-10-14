package org.myworkflows.domain.transfer;

/**
 * An export, ready to be downloaded.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record WorkflowExportFile(String name, String contentType, byte[] content) {

}
