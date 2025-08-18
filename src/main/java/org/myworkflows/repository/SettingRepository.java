package org.myworkflows.repository;

import org.myworkflows.domain.Setting;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.1
 */
public interface SettingRepository extends JpaRepository<Setting, String> {

    List<Setting> findAllByOrderByPosition();

}
