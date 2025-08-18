package org.myworkflows.view.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

import java.util.Optional;

import static com.vaadin.flow.server.VaadinServletService.getCurrentServletRequest;
import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class RequestUtil {

    public static Optional<String> getFullUrl(String path) {
        return ofNullable(getCurrentServletRequest()).map(request -> {
            final var serverPort = request.getServerPort();
            return request.getScheme() + "://" + request.getServerName() + (serverPort != 80 && serverPort != 443 ? ":" + serverPort : StringUtils.EMPTY)
                + request.getContextPath() + ofNullable(path).map(item -> "/" + item).orElse(StringUtils.EMPTY);
        });
    }

}
