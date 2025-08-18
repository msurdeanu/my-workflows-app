package org.myworkflows.provider;

import org.myworkflows.domain.StatisticItem;
import org.myworkflows.domain.StatisticItemGroup;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Component
public final class ResourceStatisticProvider implements StatisticProvider {

    private static final float ONE_MB = 1e6f;
    private static final String FILE_ICON = "vaadin:file-o";

    @Override
    public StatisticItemGroup getStatisticItemGroup() {
        final var runtime = Runtime.getRuntime();

        return StatisticItemGroup.builder()
            .root(StatisticItem.builder()
                .name("statistics.resource-usage.group")
                .icon("vaadin:folder-o")
                .build())
            .leafs(List.of(
                StatisticItem.builder()
                    .name("statistics.resource-usage.group.used-memory.name")
                    .icon(FILE_ICON)
                    .value(String.format("~%.0f MB", (runtime.totalMemory() - runtime.freeMemory()) / ONE_MB))
                    .description("statistics.resource-usage.group.used-memory.description")
                    .build(),
                StatisticItem.builder()
                    .name("statistics.resource-usage.group.free-memory.name")
                    .icon(FILE_ICON)
                    .value(String.format("~%.0f MB", runtime.freeMemory() / ONE_MB))
                    .description("statistics.resource-usage.group.free-memory.description")
                    .build(),
                StatisticItem.builder()
                    .name("statistics.resource-usage.group.max-memory.name")
                    .icon(FILE_ICON)
                    .value(String.format("~%.0f MB", runtime.maxMemory() / ONE_MB))
                    .description("statistics.resource-usage.group.max-memory.description")
                    .build()
            ))
            .build();
    }

}
