package org.myworkflows.domain;

/**
 * An API token which has just been created, as it has to be handed over to its user. It exists only here: what is
 * saved is its digest, so it cannot be shown a second time.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record UserTokenSecret(UserToken userToken, String secret) {

}
