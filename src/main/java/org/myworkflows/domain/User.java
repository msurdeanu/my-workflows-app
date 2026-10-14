package org.myworkflows.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Entity
@Table(name = "users")
public class User {

    @Id
    @Getter
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int id;

    @Getter
    @Setter
    private boolean enabled;

    @Getter
    @Setter
    private String username;

    @Getter
    @Setter
    private String password;

    @Getter
    @Setter
    private boolean admin;

    /**
     * Set while the password is one the user did not choose: the one an account is created with, or the one an admin
     * replaced it with.
     */
    @Getter
    @Setter
    @Column(name = "password_change_required")
    private boolean passwordChangeRequired;

    /**
     * @return an enabled user without the admin role, who is asked for a password of their own at first login.
     */
    public static User of(String username, String password) {
        final var user = new User();
        user.enabled = true;
        user.username = username;
        user.password = password;
        user.passwordChangeRequired = true;
        return user;
    }

}
