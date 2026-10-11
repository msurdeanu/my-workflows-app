package org.myworkflows.view.component;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.DetachEvent;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public abstract class ResizableComposite<T extends Component> extends Composite<T> implements HasResizeableWidth {

    private final BreakpointTracker breakpointTracker = new BreakpointTracker(this);

    @Override
    protected void onAttach(AttachEvent attachEvent) {
        super.onAttach(attachEvent);
        breakpointTracker.attach(attachEvent.getUI());
    }

    @Override
    protected void onDetach(DetachEvent detachEvent) {
        breakpointTracker.detach();
        super.onDetach(detachEvent);
    }

}
