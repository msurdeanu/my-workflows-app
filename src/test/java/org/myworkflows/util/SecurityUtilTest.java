package org.myworkflows.util;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.User;
import org.myworkflows.domain.UserAccountDetails;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.0
 */
public final class SecurityUtilTest {

    @AfterEach
    public void clearContext() {
        SecurityContextHolder.clearContext();
    }

    @Test
    public void whenNobodyIsAuthenticatedThenNoUserIsFound() {
        assertEquals(Optional.empty(), SecurityUtil.findCurrentUserId());
    }

    @Test
    public void whenTheUserIsAnonymousThenNoUserIsFound() {
        // given
        SecurityContextHolder.getContext().setAuthentication(new AnonymousAuthenticationToken("key", "anonymousUser",
            List.of(new SimpleGrantedAuthority("ROLE_ANONYMOUS"))));

        // when and then
        assertEquals(Optional.empty(), SecurityUtil.findCurrentUserId());
    }

    @Test
    public void whenAPageIsServedThenTheUserIsFoundBehindTheAccountDetails() {
        // given
        final var userAccountDetails = new UserAccountDetails(createUser(7));
        SecurityContextHolder.getContext().setAuthentication(new UsernamePasswordAuthenticationToken(userAccountDetails, null, List.of()));

        // when and then
        assertEquals(Optional.of(7), SecurityUtil.findCurrentUserId());
    }

    @Test
    public void whenARestApiIsCalledThenTheUserItselfIsFound() {
        // given
        SecurityContextHolder.getContext().setAuthentication(new UsernamePasswordAuthenticationToken(createUser(9), null, List.of()));

        // when and then
        assertEquals(Optional.of(9), SecurityUtil.findCurrentUserId());
    }

    private User createUser(int id) {
        final var user = mock(User.class);
        when(user.getId()).thenReturn(id);
        return user;
    }

}
