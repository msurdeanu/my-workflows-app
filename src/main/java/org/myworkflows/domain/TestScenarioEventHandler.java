package org.myworkflows.domain;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface TestScenarioEventHandler {

    void onActivationChanged(final TestScenario testScenario);

    void onCronExpressionChanged(final TestScenario testScenario, final String newCronExpression);

    void onDelete(final TestScenario testScenario);

    void onNameChanged(final TestScenario testScenario, final String newName);

}
