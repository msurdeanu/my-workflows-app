package org.myworkflows.domain.handler;

import org.myworkflows.domain.WorkflowPlaceholder;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public interface WorkflowPlaceholderEventHandler {

    WorkflowPlaceholder onCreate(String name);

    void onDelete(WorkflowPlaceholder workflowPlaceholder);

    void onUpdate(WorkflowPlaceholder workflowPlaceholder);

}
