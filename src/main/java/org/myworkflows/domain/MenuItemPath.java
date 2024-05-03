package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@RequiredArgsConstructor
public final class MenuItemPath<T> {

    private final T value;

}
