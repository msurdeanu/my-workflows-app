package org.myworkflows.domain;

import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import org.myworkflows.converter.MenuItemPathToStringConverter;
import org.myworkflows.converter.UserRoleToStringConverter;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Entity
@Getter
@Table(name = "menu_items")
public class MenuItem {

    @Id
    private String label;

    private String icon;

    @Convert(converter = MenuItemPathToStringConverter.class)
    private MenuItemPath<?> path;

    @Convert(converter = UserRoleToStringConverter.class)
    private UserRole role;

    private int position;

}

