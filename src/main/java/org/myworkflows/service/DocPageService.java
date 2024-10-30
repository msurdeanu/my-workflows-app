package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.domain.CacheableEntry;
import org.myworkflows.domain.DocPage;
import org.myworkflows.repository.DocPageRepository;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class DocPageService {

    private final ReentrantLock lock = new ReentrantLock();

    private final ApplicationManager applicationManager;
    private final InternalCache docPageCache;

    public DocPageService(ApplicationManager applicationManager) {
        this.applicationManager = applicationManager;
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

    public void delete(DocPage docPage) {
        lock.lock();
        try {
            docPageCache.evict(docPage.getName());
            applicationManager.getBeanOfType(DocPageRepository.class).delete(docPage);
        } finally {
            lock.unlock();
        }
    }

    public void updateValue(DocPage docPage, String newValue) {
        lock.lock();
        try {
            docPage.setValue(newValue);
            applicationManager.getBeanOfType(DocPageRepository.class).save(docPage);
        } finally {
            lock.unlock();
        }
    }

}
