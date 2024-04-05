package org.myworkflows.repository;

import org.myworkflows.domain.WorkflowTemplate;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowRepository extends JpaRepository<WorkflowTemplate, Integer> {

}
