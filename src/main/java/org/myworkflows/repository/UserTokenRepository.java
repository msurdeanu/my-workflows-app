package org.myworkflows.repository;

import org.myworkflows.domain.UserToken;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public interface UserTokenRepository extends JpaRepository<UserToken, Integer> {

    Optional<UserToken> findByTokenHash(String tokenHash);

    Optional<UserToken> findByIdAndUserId(int id, int userId);

    List<UserToken> findByUserIdOrderByCreatedDescIdDesc(int userId);

    boolean existsByUserIdAndNameIgnoreCase(int userId, String name);

}
