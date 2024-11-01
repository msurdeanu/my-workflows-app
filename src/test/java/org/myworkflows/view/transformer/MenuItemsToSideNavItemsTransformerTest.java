package org.myworkflows.view.transformer;

import com.vaadin.flow.component.sidenav.SideNavItem;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.MenuItem;
import org.myworkflows.domain.MenuItemPath;
import org.myworkflows.domain.UserRole;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class MenuItemsToSideNavItemsTransformerTest {

    @Test
    public void whenPathIsMissingThenNoItemIsReturned() {
        // given
        final var transformer = new MenuItemsToSideNavItemsTransformer();
        final var menuItems = List.of(createMenuItem("Book", "book", null, UserRole.GUEST, 1));

        // when & then
        final var sideNavItems = transformer.transform(menuItems);
        assertNotNull(sideNavItems);
        assertEquals(0, sideNavItems.size());
    }

    @Test
    public void whenPathIsStringThenItemIsReturned() {
        // given
        final var transformer = new MenuItemsToSideNavItemsTransformer();
        final var menuItems = List.of(createMenuItem("Globe", "globe", new MenuItemPath<>("https://mydomain"), UserRole.GUEST, 2));

        // when & then
        final var sideNavItems = transformer.transform(menuItems);
        assertNotNull(sideNavItems);
        assertEquals(1, sideNavItems.size());
        final var sideNavItem = sideNavItems.getFirst();
        assertEquals("https://mydomain", sideNavItem.getPath());
    }

    private MenuItem createMenuItem(String label, String icon, MenuItemPath<?> path, UserRole role, int position) {
        final var menuItem = new MenuItem();
        menuItem.setLabel(label);
        menuItem.setIcon(icon);
        menuItem.setPath(path);
        menuItem.setRole(role);
        menuItem.setPosition(position);
        return menuItem;
    }

}
