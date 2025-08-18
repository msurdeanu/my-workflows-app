package org.myworkflows.domain;

import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0
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

    public String getValueAsString() {
        return ofNullable(value).map(Object::toString).orElse(StringUtils.EMPTY);
    }

}
