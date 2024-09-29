package org.myworkflows.repository;

import org.myworkflows.domain.Placeholder;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface PlaceholderRepository extends JpaRepository<Placeholder, String> {

    @Cacheable(cacheNames = "placeholder")
    default Map<String, String> getAllAsMap() {
        return findAll().stream().collect(Collectors.toMap(Placeholder::getName, Placeholder::getValue));
    }

}

