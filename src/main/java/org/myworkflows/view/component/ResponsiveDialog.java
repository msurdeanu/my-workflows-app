package org.myworkflows.view.component;

import com.vaadin.flow.component.dialog.Dialog;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public abstract class ResponsiveDialog extends Dialog {

    // half of the screen, but never less than 900x450 and never more than the screen itself
    private static final String WIDTH = "min(max(900px, 50vw), 96vw)";
    private static final String HEIGHT = "min(max(450px, 50vh), 96vh)";

    public ResponsiveDialog(String id) {
        setId(id);

        makeDialogResizableAndDraggable();
        makeDialogResponsive();
    }

    private void makeDialogResizableAndDraggable() {
        setDraggable(true);
        setResizable(true);
    }

    /**
     * A dialog attaches to the UI overlay rather than below a {@link ResponsiveLayout}, so it never receives a
     * breakpoint and is sized in viewport terms instead.
     */
    private void makeDialogResponsive() {
        setWidth(WIDTH);
        setHeight(HEIGHT);
    }

}
