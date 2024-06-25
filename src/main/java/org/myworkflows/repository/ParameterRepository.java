package org.myworkflows.repository;

import org.myworkflows.domain.Parameter;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface ParameterRepository extends JpaRepository<Parameter, String> {

}
