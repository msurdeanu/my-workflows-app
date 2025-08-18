package org.myworkflows.repository;

import org.myworkflows.domain.WorkflowRun;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public interface WorkflowRunRepository extends JpaRepository<WorkflowRun, UUID> {

    List<WorkflowRun> findByOrderByCreatedDesc(Pageable pageable);

    @Query("SELECT run.created FROM WorkflowRun run ORDER BY run.created DESC")
    List<Instant> findOldestCutoffDate(Pageable pageable);

    @Query("SELECT run.id FROM WorkflowRun run WHERE run.created <= :cutoff")
    List<UUID> findOldestEntriesByCutoff(@Param("cutoff") Instant cutoff);

}
