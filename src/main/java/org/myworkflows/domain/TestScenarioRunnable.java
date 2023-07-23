package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class TestScenarioRunnable implements Runnable {

    private final ApplicationManager applicationManager;

    @Getter
    private final TestScenario testScenario;

    @Override
    public void run() {
        // Nothing to do
    }

}
