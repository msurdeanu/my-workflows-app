package org.myworkflows.repository;

import org.myworkflows.domain.DocPage;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface DocPageRepository extends JpaRepository<DocPage, String> {

}
