package org.myworkflows.repository;

import org.myworkflows.domain.WorkflowDefinition;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowDefinitionRepository extends JpaRepository<WorkflowDefinition, Integer> {

}