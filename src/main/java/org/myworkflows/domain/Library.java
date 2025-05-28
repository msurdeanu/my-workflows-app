package org.myworkflows.domain;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * @author Mihai Surdeanu
 * @since 1.2.0
 */
@Getter
@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class Library {

    private final String filePath;
    private final boolean loaded;

    public static Library of(String filePath, boolean loaded) {
        return new Library(filePath, loaded);
    }

}

