package org.myworkflows.domain.transfer;

/**
 * What an export writes to a file and an import reads from it.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public enum WorkflowTransferKind {
    TEMPLATE, DEFINITION, PARAMETER, PLACEHOLDER
}
