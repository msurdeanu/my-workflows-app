package org.myworkflows.repository;

import org.myworkflows.domain.WorkflowRun;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowRunRepository extends JpaRepository<WorkflowRun, Integer> {

    List<WorkflowRun> findByOrderByCreatedDesc(Pageable pageable);

}
