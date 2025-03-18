package org.myworkflows.repository;

import org.myworkflows.domain.User;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface UserRepository extends JpaRepository<User, Integer> {

    Optional<User> findByToken(String token);

    Optional<User> findByUsername(String username);

}

