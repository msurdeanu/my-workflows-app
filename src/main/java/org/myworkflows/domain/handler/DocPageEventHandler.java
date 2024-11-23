package org.myworkflows.domain.handler;

import org.myworkflows.domain.DocPage;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface DocPageEventHandler {

    void onCreate(String name);

    void onDelete(DocPage docPage);

    void onUpdate(DocPage docPage, String newValue);

}
