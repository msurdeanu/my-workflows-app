package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.domain.CacheableEntry;
import org.myworkflows.domain.DocPage;
import org.myworkflows.repository.DocPageRepository;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
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
public final class DocPageService implements LoaderService {

    private final ReentrantLock lock = new ReentrantLock();

    private final ApplicationManager applicationManager;
    private final InternalCache cache;

    public DocPageService(ApplicationManager applicationManager) {
        this.applicationManager = applicationManager;
        cache = (InternalCache) applicationManager.getBeanOfType(InternalCacheManager.class)
            .getCache(InternalCacheManager.CacheNameEnum.DOC_PAGE.getName());
    }

    @Order(200)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        applicationManager.getBeanOfType(DocPageRepository.class)
            .findAll()
            .forEach(this::addToCache);
    }

    public void addToCache(CacheableEntry entry) {
        cache.put(entry.getCacheableKey(), entry);
    }

    public Optional<DocPage> findByName(String name) {
        return ofNullable(cache.get(name, DocPage.class));
    }

    public Set<String> getAllNames() {
        return cache.getAllKeys(String.class);
    }

    public void create(String name) {
        lock.lock();
        try {
            final var docPage = DocPage.of(name);
            cache.put(applicationManager.getBeanOfType(DocPageRepository.class).save(docPage).getName(), docPage);
        } finally {
            lock.unlock();
        }
    }

    public void delete(DocPage docPage) {
        lock.lock();
        try {
            cache.evict(docPage.getName());
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
