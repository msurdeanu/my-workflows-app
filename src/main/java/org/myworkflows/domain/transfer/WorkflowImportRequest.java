package org.myworkflows.domain.transfer;

import org.myworkflows.domain.transfer.WorkflowImportItem.Action;

import java.util.Map;

/**
 * What a user chose for the items of a file, each named by its {@link WorkflowImportItem#key() key}.
 *
 * @param actions what to do with an item. An item which is not named is imported.
 * @param values the value typed for a new item whose value the file left out.
 * @param pausingNewTemplates whether a template the import creates stays unticked, whatever the file says, so that its
 *     cron fires only once somebody has looked at it.
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record WorkflowImportRequest(Map<String, Action> actions, Map<String, String> values, boolean pausingNewTemplates) {

    public WorkflowImportRequest {
        actions = Map.copyOf(actions);
        values = Map.copyOf(values);
    }

}
