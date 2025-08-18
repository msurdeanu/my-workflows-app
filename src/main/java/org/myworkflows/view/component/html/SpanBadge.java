package org.myworkflows.view.component.html;

import com.vaadin.flow.component.html.Span;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public final class SpanBadge extends Span {

    public SpanBadge(String text) {
        setText(text);
        getElement().getThemeList().add("badge");
    }

    public SpanBadge(String text, String theme) {
        setText(text);
        getElement().getThemeList().add("badge " + theme);
    }

}
