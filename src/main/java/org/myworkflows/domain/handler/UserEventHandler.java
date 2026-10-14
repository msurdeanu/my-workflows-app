package org.myworkflows.domain.handler;

import org.myworkflows.domain.User;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public interface UserEventHandler {

    void onCreate(String username);

    void onEnabledChanged(User user, boolean enabled);

    void onAdminChanged(User user, boolean admin);

    void onPasswordReset(User user);

}
