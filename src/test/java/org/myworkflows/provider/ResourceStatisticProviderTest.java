package org.myworkflows.provider;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ResourceStatisticProviderTest {

    @Test
    public void whenResourceStatisticProviderIsCalledThenEverythingWorksAsExpected() {
        // given
        final var provider = new ResourceStatisticProvider();

        // when and then
        final var statisticItemGroup = provider.getStatisticItemGroup();
        assertNotNull(statisticItemGroup);
        assertNotNull(statisticItemGroup.getRoot());
        assertEquals(3, statisticItemGroup.getLeafs().size());
    }

}
