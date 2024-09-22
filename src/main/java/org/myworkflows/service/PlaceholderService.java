package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.domain.Placeholder;
import org.myworkflows.repository.PlaceholderRepository;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class PlaceholderService {

    private final AtomicBoolean cacheState = new AtomicBoolean();

    private final PlaceholderRepository placeholderRepository;

    private final InternalCache<String, Placeholder> cache;

    @SuppressWarnings("unchecked")
    public PlaceholderService(ApplicationManager applicationManager) {
        placeholderRepository = applicationManager.getBeanOfType(PlaceholderRepository.class);
        cache = applicationManager.getBeanOfTypeAndName(InternalCache.class, "placeholderCache");
    }

    public Map<String, String> getAllAsMap() {
        Collection<Placeholder> retrievedPlaceholders;
        if (cacheState.getAndSet(true)) {
            retrievedPlaceholders = cache.getAllValues();
        } else {
            retrievedPlaceholders = placeholderRepository.findAll();
            retrievedPlaceholders.forEach(placeholder -> cache.put(placeholder.getName(), placeholder));
        }
        return retrievedPlaceholders.stream().collect(Collectors.toMap(Placeholder::getName, Placeholder::getValue));
    }

}
