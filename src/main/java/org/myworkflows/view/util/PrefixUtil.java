package org.myworkflows.view.util;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.dom.Element;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.stream.Stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class PrefixUtil {

    public static void setPrefixComponent(Component target, Component component) {
        clearSlot(target, "prefix");

        if (component != null) {
            component.getElement().setAttribute("slot", "prefix");
            target.getElement().appendChild(component.getElement());
        }
    }

    public static Component getPrefixComponent(Component target) {
        return getChildInSlot(target, "prefix");
    }

    private static void clearSlot(Component target, String slot) {
        getElementsInSlot(target, slot).toList().forEach(target.getElement()::removeChild);
    }

    private static Component getChildInSlot(HasElement target, String slot) {
        return getElementsInSlot(target, slot).findFirst().map(value -> value.getComponent().get()).orElse(null);
    }

    private static Stream<Element> getElementsInSlot(HasElement target, String slot) {
        return target.getElement().getChildren().filter(child -> slot.equals(child.getAttribute("slot")));
    }

}
