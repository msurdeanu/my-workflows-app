package org.myworkflows.view.component;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public interface HasResizeableWidth {

    /**
     * Called on every component below a {@link ResponsiveLayout} (or a {@link ResizableComposite}) when the screen
     * moves to another {@link Breakpoint}, never on intermediate widths.
     */
    default void onWidthChange(Breakpoint breakpoint) {
    }

}
