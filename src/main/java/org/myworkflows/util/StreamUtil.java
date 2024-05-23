package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class StreamUtil {

    public static <T> List<T> substract(List<T> list1, List<T> list2) {
        return list1.stream()
            .filter(element -> !list2.contains(element))
            .collect(Collectors.toList());
    }

}
