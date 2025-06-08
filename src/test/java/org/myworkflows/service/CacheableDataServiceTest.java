package org.myworkflows.service;

import com.vaadin.flow.data.provider.Query;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.domain.filter.Filter;

import java.util.function.Predicate;

import static org.apache.commons.lang3.StringUtils.contains;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 1.2.0
 */
@ExtendWith(MockitoExtension.class)
public final class CacheableDataServiceTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private InternalCacheManager internalCacheManager;

    @Test
    public void testCacheableDataService() {
        // given
        final var internalCache = new InternalCache(CacheNameEnum.LIBRARY.getName(), 10, InternalCache.InternalCacheOrder.FIFO);
        internalCache.put("abc", new TestModel("abc", "def"));
        internalCache.put("1234", new TestModel("1234", "5678"));
        when(applicationManager.getBeanOfType(InternalCacheManager.class)).thenReturn(internalCacheManager);
        when(internalCacheManager.getCache(anyString())).thenReturn(internalCache);

        // when
        final var cacheableDataService = new CacheableDataService<TestModel, TestFilter>(applicationManager, CacheNameEnum.LIBRARY) {

            @Override
            protected TestFilter createFilter() {
                return new TestFilter();
            }

        };

        // then
        assertEquals(2, cacheableDataService.getAllSize());
        final var selectedDataSet = cacheableDataService.getAll(new TestFilter(), 1, 1).toList();
        assertEquals(1, selectedDataSet.size());
        assertTrue(selectedDataSet.contains(new TestModel("1234", "5678")));
        assertEquals(2, cacheableDataService.getAllItems().count());
        assertEquals(1, cacheableDataService.findBy(new Query<>(new TestFilter().keyCriteria("23"))).count());
        assertEquals(0, cacheableDataService.countBy(new Query<>(new TestFilter().keyCriteria("ef"))));
    }

    private record TestModel(String key, String value) {

    }

    @Setter
    @Accessors(fluent = true)
    private static class TestFilter implements Filter<TestModel> {

        private String keyCriteria = StringUtils.EMPTY;

        @Override
        public Predicate<TestModel> getFilterPredicate() {
            return isNotEmpty(keyCriteria)
                ? item -> contains(item.key(), keyCriteria)
                : item -> true;
        }

    }

}
