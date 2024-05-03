package org.myworkflows.transformer;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.sidenav.SideNavItem;
import org.myworkflows.domain.MenuItem;

import java.util.List;
import java.util.stream.Collectors;

import static com.vaadin.flow.component.icon.VaadinIcon.valueOf;
import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class MenuItemsToSideNavItemsTransformer implements Transformer<List<MenuItem>, List<SideNavItem>> {

    @Override
    public List<SideNavItem> transform(List<MenuItem> menuItems) {
        return ofNullable(menuItems)
            .orElse(List.of())
            .stream()
            .filter(menuItem -> nonNull(menuItem.getPath()))
            .filter(menuItem -> menuItem.getRole().validate())
            .map(this::createRouterLink)
            .collect(Collectors.toList());
    }

    private SideNavItem createRouterLink(MenuItem menuItem) {
        final var menuItemPath = menuItem.getPath();
        final var value = menuItemPath.getValue();
        final SideNavItem sideNavItem;
        if (value instanceof String path) {
            sideNavItem = new SideNavItem(menuItem.getLabel(), path,
                valueOf(menuItem.getIcon().toUpperCase()).create());
            sideNavItem.setTarget("_blank");
        } else {
            sideNavItem = new SideNavItem(menuItem.getLabel(), (Class<? extends Component>) value,
                valueOf(menuItem.getIcon().toUpperCase()).create());
        }
        sideNavItem.setLabel(sideNavItem.getTranslation(menuItem.getLabel()));
        return sideNavItem;
    }

}
