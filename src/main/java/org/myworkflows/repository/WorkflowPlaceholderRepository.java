package org.myworkflows.repository;

import org.myworkflows.domain.WorkflowPlaceholder;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowPlaceholderRepository extends JpaRepository<WorkflowPlaceholder, String> {

}

