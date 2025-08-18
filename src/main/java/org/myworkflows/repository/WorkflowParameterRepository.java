package org.myworkflows.repository;

import org.myworkflows.domain.WorkflowParameter;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public interface WorkflowParameterRepository extends JpaRepository<WorkflowParameter, String> {

}
