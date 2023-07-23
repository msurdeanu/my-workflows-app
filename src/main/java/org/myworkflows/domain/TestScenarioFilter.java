package org.myworkflows.domain;

import lombok.Getter;
import org.apache.commons.lang3.StringUtils;

import java.util.Set;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
public final class TestScenarioFilter {

    private TestScenarioType byTypeCriteria = TestScenarioType.ALL;

    private String byNameCriteria = StringUtils.EMPTY;

    private Set<String> byTagCriteria = Set.of();

    public TestScenarioFilter setByTypeCriteria(final TestScenarioType byTypeCriteria) {
        this.byTypeCriteria = byTypeCriteria;

        return this;
    }

    public TestScenarioFilter setByNameCriteria(final String byNameCriteria) {
        this.byNameCriteria = byNameCriteria;

        return this;
    }

    public TestScenarioFilter setByTagCriteria(final Set<String> byTagCriteria) {
        this.byTagCriteria = byTagCriteria;

        return this;
    }

}
