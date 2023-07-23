package org.myworkflows;

import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
public final class SecuredPasswordGenerator {

    public static void main(String[] args) {
        log.info(encode("user"));
    }

    private static String encode(String password) {
        return new BCryptPasswordEncoder().encode(password);
    }

}
