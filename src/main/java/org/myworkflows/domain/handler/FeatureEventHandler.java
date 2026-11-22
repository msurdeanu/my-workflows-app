package org.myworkflows.domain.handler;

import org.myworkflows.domain.Feature;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
public interface FeatureEventHandler {

    void onEnabledChanged(Feature feature, boolean enabled);

    void onTestClaude();

}
