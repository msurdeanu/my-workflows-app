package org.myworkflows.repository;

import org.myworkflows.domain.WorkflowDefinition;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public interface WorkflowDefinitionRepository extends JpaRepository<WorkflowDefinition, Integer> {

    @Modifying
    @Query(value = """
        DELETE FROM workflow_definitions
        WHERE id = :id AND
            NOT EXISTS (SELECT 1 FROM workflow_templates_workflow_definitions WHERE workflow_definition_id = :id)
        """, nativeQuery = true)
    int deleteIfNotUsed(@Param("id") int id);

}