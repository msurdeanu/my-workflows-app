package org.myworkflows.repository;

import org.myworkflows.domain.WorkflowRun;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowRunRepository extends JpaRepository<WorkflowRun, UUID> {

    List<WorkflowRun> findByOrderByCreatedDesc(Pageable pageable);

    @Modifying
    @Query(value = """
        DELETE FROM workflow_runs
        WHERE ROWID NOT IN (
            SELECT ROWID
            FROM workflow_runs
            ORDER BY created DESC
            LIMIT :cacheSize
        )
        """, nativeQuery = true)
    int deleteOldEntries(@Param("cacheSize") int cacheSize);

}
