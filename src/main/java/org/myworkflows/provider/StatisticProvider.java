package org.myworkflows.provider;

import org.myworkflows.domain.StatisticItemGroup;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public interface StatisticProvider {

    StatisticItemGroup getStatisticItemGroup();

}
