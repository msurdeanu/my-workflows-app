package org.myworkflows.domain;

import lombok.Builder;
import lombok.Getter;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Getter
public final class StatisticItemGroup {

    private StatisticItem root;

    private List<StatisticItem> leafs;

}
