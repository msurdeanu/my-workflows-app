package org.myworkflows.domain;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface DocPageEventHandler {

    void onDelete(DocPage docPage);

    void onValueUpdated(DocPage docPage, String newValue);

}
