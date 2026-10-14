package org.myworkflows.domain;

/**
 * A password generated for a user, as it has to be handed over to them. It exists only here: what is saved is its hash,
 * so it cannot be shown a second time.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record UserTemporaryPassword(User user, String password) {

}
