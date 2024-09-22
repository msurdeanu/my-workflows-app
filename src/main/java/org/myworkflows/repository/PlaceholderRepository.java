package org.myworkflows.repository;

import org.myworkflows.domain.Placeholder;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface PlaceholderRepository extends JpaRepository<Placeholder, String> {

}

