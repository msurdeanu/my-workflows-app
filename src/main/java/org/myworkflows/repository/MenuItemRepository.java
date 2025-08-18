package org.myworkflows.repository;

import org.myworkflows.domain.MenuItem;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public interface MenuItemRepository extends JpaRepository<MenuItem, String> {

    @Cacheable(cacheNames = "menuItem")
    List<MenuItem> findByOrderByPosition();

}
