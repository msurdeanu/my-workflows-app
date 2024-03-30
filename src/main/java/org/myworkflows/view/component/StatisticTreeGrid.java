package org.myworkflows.view.component;

import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.treegrid.TreeGrid;
import com.vaadin.flow.data.renderer.LitRenderer;
import org.myworkflows.domain.StatisticItem;
import org.myworkflows.domain.StatisticItemGroup;
import org.myworkflows.provider.StatisticProvider;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class StatisticTreeGrid extends Composite<VerticalLayout> {

    private final Map<StatisticItem, List<StatisticItem>> statisticItemListMap;

    public StatisticTreeGrid(final List<StatisticProvider> statisticProviders) {
        statisticItemListMap = buildAllStats(statisticProviders);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();

        layout.setSizeFull();

        final var treeGrid = new TreeGrid<StatisticItem>();
        treeGrid.setItems(statisticItemListMap.keySet(), parent -> statisticItemListMap.getOrDefault(parent, List.of()));
        treeGrid.addColumn(LitRenderer.<StatisticItem>of("<vaadin-grid-tree-toggle @click=${onClick} "
                                + ".leaf=${item.leaf} .expanded=${model.expanded} .level=${model.level}>"
                                + "<vaadin-icon icon='${item.icon}'></vaadin-icon>&nbsp;"
                                + "${item.name}"
                                + "</vaadin-grid-tree-toggle>")
                        .withProperty("leaf", item -> !treeGrid.getDataCommunicator().hasChildren(item))
                        .withProperty("icon", StatisticItem::getIcon)
                        .withProperty("name", prop -> getTranslation(prop.getName()))
                        .withFunction("onClick", item -> {
                            if (treeGrid.getDataCommunicator().hasChildren(item)) {
                                if (treeGrid.isExpanded(item)) {
                                    treeGrid.collapse(List.of(item));
                                } else {
                                    treeGrid.expand(List.of(item));
                                }
                            }
                        }))
                .setHeader(getTranslation("statistics.property.column"))
                .setWidth("30%");
        treeGrid.addColumn(this::getComputedValue)
                .setHeader(getTranslation("statistics.value.column"))
                .setWidth("20%");
        treeGrid.addColumn(item -> getTranslation(item.getDescription()))
                .setHeader(getTranslation("statistics.description.column"))
                .setWidth("50%");
        treeGrid.expandRecursively(statisticItemListMap.keySet(), 2);
        treeGrid.addThemeVariants(GridVariant.LUMO_NO_ROW_BORDERS, GridVariant.LUMO_WRAP_CELL_CONTENT);

        layout.add(treeGrid);
        return layout;
    }

    private Map<StatisticItem, List<StatisticItem>> buildAllStats(final List<StatisticProvider> statisticProviders) {
        return statisticProviders.stream()
                .map(StatisticProvider::getStatisticItemGroup)
                .collect(Collectors.toMap(StatisticItemGroup::getRoot, StatisticItemGroup::getLeafs, (it1, it2) -> it1));
    }

    private Object getComputedValue(final StatisticItem statisticItem) {
        return ofNullable(statisticItem.getValueTranslationKey())
                .map(key -> (Object) getTranslation(key, statisticItem.getValue()))
                .orElse(statisticItem.getValue());
    }

}
