package org.myworkflows.service;

import com.vaadin.flow.data.provider.Query;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.User;
import org.myworkflows.domain.UserTemporaryPassword;
import org.myworkflows.domain.filter.UserFilter;
import org.myworkflows.exception.UserException;
import org.myworkflows.exception.UserException.Reason;
import org.myworkflows.repository.UserRepository;
import org.myworkflows.repository.UserTokenRepository;
import org.myworkflows.util.SecurityUtil;
import org.springframework.data.domain.Sort;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.security.SecureRandom;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * The users are few and what is read here decides who gets in, so nothing is cached: every call goes to the database.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Service
@RequiredArgsConstructor
public class UserService {

    public static final int MIN_PASSWORD_LENGTH = 8;

    public static final String USERNAME_ALLOWED_CHARS = "[a-zA-Z0-9._-]";

    private static final Pattern USERNAME_PATTERN = Pattern.compile(USERNAME_ALLOWED_CHARS + "{3,32}");

    // neither 0 and O nor 1, l and I: a temporary password is read by one person and typed by another
    private static final String TEMPORARY_PASSWORD_CHARS = "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz23456789";

    private static final int TEMPORARY_PASSWORD_LENGTH = 16;

    private final SecureRandom secureRandom = new SecureRandom();

    private final UserRepository userRepository;

    private final UserTokenRepository userTokenRepository;

    private final WorkflowRunService workflowRunService;

    private final PasswordEncoder passwordEncoder;

    public Stream<User> findBy(Query<User, UserFilter> query) {
        return findAll(query)
            .skip(query.getOffset())
            .limit(query.getLimit());
    }

    public int countBy(Query<User, UserFilter> query) {
        return (int) findAll(query).count();
    }

    public Optional<User> findById(int id) {
        return userRepository.findById(id);
    }

    /**
     * Has to be called on the thread serving the user, see {@link SecurityUtil#findCurrentUserId()}.
     */
    public Optional<User> findCurrentUser() {
        return SecurityUtil.findCurrentUserId().flatMap(userRepository::findById);
    }

    /**
     * Read from the database, like {@link #getCurrentAdmin()}, so that an admin who was demoted a moment ago is one no more.
     */
    public boolean isCurrentUserAnAdmin() {
        return findCurrentUser().filter(user -> user.isEnabled() && user.isAdmin()).isPresent();
    }

    /**
     * Creates a user who is not an admin, with a temporary password. Their API tokens are theirs to create.
     */
    @Transactional
    public UserTemporaryPassword create(String username) {
        getCurrentAdmin();
        if (username == null || !USERNAME_PATTERN.matcher(username).matches()) {
            throw new UserException(Reason.USERNAME_INVALID);
        }
        // names which differ only by their case would be told apart by the login but not by the people reading them
        if (userRepository.existsByUsernameIgnoreCase(username)) {
            throw new UserException(Reason.USERNAME_TAKEN);
        }
        final var temporaryPassword = generateTemporaryPassword();
        final var user = userRepository.save(User.of(username, passwordEncoder.encode(temporaryPassword)));
        return new UserTemporaryPassword(user, temporaryPassword);
    }

    /**
     * A disabled user can neither log in nor use their API tokens.
     */
    @Transactional
    public User setEnabled(int userId, boolean enabled) {
        final var user = getAnotherUser(userId);
        user.setEnabled(enabled);
        return userRepository.save(user);
    }

    @Transactional
    public User setAdmin(int userId, boolean admin) {
        final var user = getAnotherUser(userId);
        user.setAdmin(admin);
        return userRepository.save(user);
    }

    /**
     * Replaces the password of a user who lost theirs with a temporary one.
     */
    @Transactional
    public UserTemporaryPassword resetPassword(int userId) {
        final var user = getAnotherUser(userId);
        final var temporaryPassword = generateTemporaryPassword();
        user.setPassword(passwordEncoder.encode(temporaryPassword));
        user.setPasswordChangeRequired(true);
        return new UserTemporaryPassword(userRepository.save(user), temporaryPassword);
    }

    /**
     * Deletes a user and their API tokens, which go first: SQLite does not enforce the foreign keys here, so nothing
     * cascades. The runs they started stay, as the runs of a deleted user - the ids of the users only grow, so no
     * user created later takes them over. A session of the user ends with the next page they open.
     */
    @Transactional
    public void delete(int userId) {
        final var user = getAnotherUser(userId);
        userTokenRepository.deleteByUserId(user.getId());
        userRepository.delete(user);
        workflowRunService.forgetUsername(user.getId());
    }

    /**
     * Changes the password of the current user, who proves to be that user with the current one. Every other session
     * of the user ends, as it was opened with a password which is no longer theirs.
     */
    @Transactional
    public void changeOwnPassword(String currentPassword, String newPassword) {
        final var user = getCurrentUser();
        if (currentPassword == null || !passwordEncoder.matches(currentPassword, user.getPassword())) {
            throw new UserException(Reason.CURRENT_PASSWORD_WRONG);
        }
        if (newPassword == null || newPassword.length() < MIN_PASSWORD_LENGTH) {
            throw new UserException(Reason.PASSWORD_TOO_SHORT);
        }
        if (passwordEncoder.matches(newPassword, user.getPassword())) {
            throw new UserException(Reason.PASSWORD_UNCHANGED);
        }
        final var encodedPassword = passwordEncoder.encode(newPassword);
        user.setPassword(encodedPassword);
        user.setPasswordChangeRequired(false);
        userRepository.save(user);
        SecurityUtil.findCurrentAccount().ifPresent(account -> account.onPasswordChanged(encodedPassword));
    }

    private Stream<User> findAll(Query<User, UserFilter> query) {
        final var users = userRepository.findAll(Sort.by(Sort.Order.asc("username").ignoreCase())).stream();
        return query.getFilter()
            .map(filter -> users.filter(filter.getFilterPredicate()))
            .orElse(users);
    }

    private User getCurrentUser() {
        return findCurrentUser().orElseThrow(() -> new UserException(Reason.USER_UNKNOWN));
    }

    /**
     * The role is read from the database and not from the session, which may be older than the last change of it.
     */
    private User getCurrentAdmin() {
        final var user = getCurrentUser();
        if (!user.isEnabled() || !user.isAdmin()) {
            throw new UserException(Reason.NOT_ALLOWED);
        }
        return user;
    }

    /**
     * An admin changes the other accounts only. Their own password is changed with the current one at hand, and an
     * admin who could disable or demote themselves could leave the application without anybody to manage it - which
     * cannot happen this way, as whoever changes an account is an enabled admin and stays one.
     */
    private User getAnotherUser(int userId) {
        if (getCurrentAdmin().getId() == userId) {
            throw new UserException(Reason.OWN_ACCOUNT);
        }
        return userRepository.findById(userId).orElseThrow(() -> new UserException(Reason.USER_UNKNOWN));
    }

    private String generateTemporaryPassword() {
        final var password = new StringBuilder(TEMPORARY_PASSWORD_LENGTH);
        for (int index = 0; index < TEMPORARY_PASSWORD_LENGTH; index++) {
            password.append(TEMPORARY_PASSWORD_CHARS.charAt(secureRandom.nextInt(TEMPORARY_PASSWORD_CHARS.length())));
        }
        return password.toString();
    }

}
