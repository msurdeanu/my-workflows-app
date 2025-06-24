package org.myworkflows.domain;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public record Library(String filePath, boolean isLoaded) {

    public static Library of(String filePath, boolean isLoaded) {
        return new Library(filePath, isLoaded);
    }

}
