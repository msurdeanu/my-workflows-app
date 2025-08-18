package org.myworkflows.view.util;

import com.vaadin.flow.dom.Element;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ClipboardUtil {

    public static void copyTo(Element element, String text) {
        element.executeJs("navigator.clipboard.writeText($0)", text);
    }

}
