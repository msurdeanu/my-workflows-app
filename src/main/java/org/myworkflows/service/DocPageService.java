package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.domain.DocPage;
import org.myworkflows.repository.DocPageRepository;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

import static java.util.Optional.ofNullable;
import static org.myworkflows.cache.CacheNameEnum.DOC_PAGE_NAME;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public class DocPageService implements ServiceCreator<DocPage> {

    private final ReentrantLock lock = new ReentrantLock();

    private final ApplicationManager applicationManager;
    private final InternalCache cache;

    public DocPageService(ApplicationManager applicationManager) {
        this.applicationManager = applicationManager;
        cache = (InternalCache) applicationManager.getBeanOfType(InternalCacheManager.class)
            .getCache(CacheNameEnum.DOC_PAGE.getName());
    }

    public Optional<DocPage> findByName(String name) {
        return ofNullable(cache.get(name, DocPage.class));
    }

    public Set<String> getAllNames() {
        return cache.getAllKeys(String.class);
    }

    @Override
    @CachePut(cacheNames = DOC_PAGE_NAME, key = "#result.name")
    public DocPage create(DocPage docPage, boolean requiresPersistence) {
        if (requiresPersistence) {
            lock.lock();
            try {
                applicationManager.getBeanOfType(DocPageRepository.class).save(docPage);
            } finally {
                lock.unlock();
            }
        }

        return docPage;
    }

    @CacheEvict(cacheNames = DOC_PAGE_NAME, key = "#docPage.name")
    public void delete(DocPage docPage) {
        lock.lock();
        try {
            applicationManager.getBeanOfType(DocPageRepository.class).delete(docPage);
        } finally {
            lock.unlock();
        }
    }

    public void update(DocPage docPage, String newValue) {
        lock.lock();
        try {
            docPage.setValue(newValue);
            applicationManager.getBeanOfType(DocPageRepository.class).save(docPage);
        } finally {
            lock.unlock();
        }
    }

}
