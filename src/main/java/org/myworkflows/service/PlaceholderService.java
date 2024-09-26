package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.domain.Placeholder;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class PlaceholderService {

    private final InternalCache<String, Placeholder> cache;

    @SuppressWarnings("unchecked")
    public PlaceholderService(ApplicationManager applicationManager) {
        cache = applicationManager.getBeanOfTypeAndName(InternalCache.class, "placeholderCache");
    }

    public void addToCache(Placeholder placeholder) {
        cache.putFirst(placeholder.getName(), placeholder);
    }

    public Map<String, String> getAllAsMap() {
        return cache.getAllValues().stream().collect(Collectors.toMap(Placeholder::getName, Placeholder::getValue));
    }

}
