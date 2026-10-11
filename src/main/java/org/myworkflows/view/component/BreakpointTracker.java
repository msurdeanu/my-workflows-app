package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.shared.Registration;

import static java.util.Optional.ofNullable;

/**
 * Turns browser widths into a {@link Breakpoint} and tells a component tree about it. Shared by
 * {@link ResponsiveLayout} (a whole page) and {@link ResizableComposite} (a component that can also live in a dialog,
 * which attaches to the UI overlay rather than below a page).
 *
 * @author Mihai Surdeanu
 * @since 2.0
 */
final class BreakpointTracker {

    private final Component root;

    private Registration registration;
    private Breakpoint breakpoint;

    BreakpointTracker(Component root) {
        this.root = root;
    }

    void attach(UI ui) {
        final var page = ui.getPage();
        registration = page.addBrowserWindowResizeListener(event -> adjustByWidth(event.getWidth()));
        final var clientDetails = page.getExtendedClientDetails();
        if (clientDetails.getScreenWidth() == -1) {
            clientDetails.refresh(details -> adjustByWidth(details.getBodyClientWidth()));
        } else {
            adjustByWidth(clientDetails.getBodyClientWidth());
        }
    }

    void detach() {
        ofNullable(registration).ifPresent(Registration::remove);
        registration = null;
        breakpoint = null;
    }

    /**
     * Resize events fire continuously while a window is dragged, and on a phone every time the address bar
     * hides or the on-screen keyboard opens, so listeners are only told when the tier itself changes.
     */
    private void adjustByWidth(int width) {
        final var currentBreakpoint = Breakpoint.of(width);
        if (currentBreakpoint == breakpoint) {
            return;
        }

        breakpoint = currentBreakpoint;
        if (root instanceof HasResizeableWidth resizeable) {
            resizeable.onWidthChange(currentBreakpoint);
        }
        notifyChildren(root, currentBreakpoint);
    }

    private static void notifyChildren(Component parent, Breakpoint breakpoint) {
        parent.getChildren()
            // a nested resizable composite tracks the width on its own
            .filter(child -> !(child instanceof ResizableComposite<?>))
            .forEach(child -> {
                if (child instanceof HasResizeableWidth resizeable) {
                    resizeable.onWidthChange(breakpoint);
                }
                notifyChildren(child, breakpoint);
            });
    }

}
