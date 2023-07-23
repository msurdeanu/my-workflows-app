package org.myworkflows.domain;

import lombok.Getter;
import lombok.Setter;

import java.time.Instant;
import java.util.Objects;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public class TestScenario {

    @Getter
    private Integer id;

    @Getter
    @Setter
    private boolean enabled;

    @Getter
    private String name;

    @Getter
    private String cron;


    @Setter
    private Instant lastRunTime;

    @Setter
    private Boolean failed;

    @Getter
    @Setter
    private boolean editable = false;

    @Override
    public boolean equals(final Object other) {
        if (this == other) {
            return true;
        }
        if (other == null || getClass() != other.getClass()) {
            return false;
        }
        final var testScenario = (TestScenario) other;
        return Objects.equals(id, testScenario.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    public boolean setCron(final String cron) {
        if (cron.equals(this.cron)) {
            return false;
        }

        this.cron = cron;
        return true;
    }

    public boolean setName(final String name) {
        if (name.equals(this.name)) {
            return false;
        }

        this.name = name;
        return true;
    }

    public void toggleOnEnabling() {
        enabled = !enabled;
    }

    public void toggleOnEditing() {
        editable = !editable;
    }

}
