package org.myworkflows.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dialog.Dialog;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public abstract class ResponsiveDialog extends Dialog {

    private static final int MAX_WIDTH = 900;
    private static final int MAX_HEIGHT = 450;

    private static final String PIXELS = "px";

    public ResponsiveDialog(final String id) {
        setId(id);

        makeDialogResizableAndDraggable();
        makeDialogResponsive();
    }

    private void makeDialogResizableAndDraggable() {
        setDraggable(true);
        setResizable(true);

    }

    private void makeDialogResponsive() {
        UI.getCurrent().getPage().retrieveExtendedClientDetails(details -> {
            setWidth(Math.max(MAX_WIDTH, details.getScreenWidth() / 2) + PIXELS);
            setHeight(Math.max(MAX_HEIGHT, details.getScreenHeight() / 2) + PIXELS);
        });

        UI.getCurrent().getPage().addBrowserWindowResizeListener(details -> {
            setWidth(Math.max(MAX_WIDTH, details.getWidth() / 2) + PIXELS);
            setHeight(Math.max(MAX_HEIGHT, details.getHeight() / 2) + PIXELS);
        });
    }

}
