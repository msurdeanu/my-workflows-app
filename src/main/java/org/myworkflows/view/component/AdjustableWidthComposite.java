package org.myworkflows.view.component;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.DetachEvent;
import com.vaadin.flow.shared.Registration;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public abstract class AdjustableWidthComposite<T extends Component> extends Composite<T> {

    protected static final int THRESHOLD_WIDTH = 900;

    private Registration registration;

    @Override
    protected void onAttach(final AttachEvent attachEvent) {
        super.onAttach(attachEvent);
        final var page = attachEvent.getUI().getPage();
        registration = page.addBrowserWindowResizeListener(event -> adjustByWidth(event.getWidth()));
        page.retrieveExtendedClientDetails(event -> adjustByWidth(event.getBodyClientWidth()));
    }

    @Override
    protected void onDetach(final DetachEvent detachEvent) {
        registration.remove();
        super.onDetach(detachEvent);
    }

    protected abstract void adjustByWidth(final int width);

}
