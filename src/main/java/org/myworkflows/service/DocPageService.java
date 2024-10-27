package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.domain.CacheableEntry;
import org.myworkflows.domain.DocPage;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.Set;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class DocPageService {

    private final InternalCache docPageCache;

    public DocPageService(ApplicationManager applicationManager) {
        docPageCache = (InternalCache) applicationManager.getBeanOfType(InternalCacheManager.class)
            .getCache(InternalCacheManager.CacheNameEnum.DOC_PAGE.getName());
    }

    public void addToCache(CacheableEntry entry) {
        docPageCache.put(entry.getCacheableKey(), entry);
    }

    public Optional<DocPage> findByName(String name) {
        return ofNullable(docPageCache.get(name, DocPage.class));
    }

    public Set<String> getAllNames() {
        return docPageCache.getAllKeys(String.class);
    }

}
