package org.myworkflows.view.component;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public interface HasResizeableWidth {

    default void onBigWidth() {
    }

    default void onSmallWidth() {
    }

    default void adjustByWidth(int width) {
        if (width > 900) {
            onBigWidth();
        } else {
            onSmallWidth();
        }
    }

}
