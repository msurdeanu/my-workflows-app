package org.myworkflows.view.util;

import com.vaadin.flow.dom.DomEvent;
import com.vaadin.flow.dom.Element;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.function.Consumer;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class KeyboardUtil {

    public static void addKeydownEventListener(Element element, Consumer<DomEvent> eventConsumer, String eventKey) {
        element.addEventListener("keydown", event -> {
            if (eventKey.equals(event.getEventData().getString("event.key"))) {
                eventConsumer.accept(event);
            }
        }).addEventData("event.key");
    }

}
