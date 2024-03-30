package org.myworkflows.domain;

import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@Setter
@Builder
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public final class StatisticItem {

    @EqualsAndHashCode.Include
    private String name;

    private String icon;

    private Object value;

    private String valueTranslationKey;

    private String description;

}
