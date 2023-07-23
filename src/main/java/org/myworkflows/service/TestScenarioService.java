package org.myworkflows.service;

import com.vaadin.flow.data.provider.Query;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.TestScenario;
import org.myworkflows.domain.TestScenarioFilter;
import org.myworkflows.domain.TestScenarioRunnable;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Predicate;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
@RequiredArgsConstructor
public class TestScenarioService {

    private static final Predicate<TestScenario> ALWAYS_TRUE_PREDICATE = testScenario -> true;

    private static final Map<Integer, TestScenarioRunnable> ALL_SCENARIOS = new HashMap<>();

    private final Lock lock = new ReentrantLock();

    private final ApplicationManager applicationManager;

    private final ScheduleTestScenarioService scheduleTestScenarioService;

    public Stream<TestScenario> getAll() {
        return getAll(new TestScenarioFilter(), 0, Long.MAX_VALUE);
    }

    public Stream<TestScenario> getAll(final TestScenarioFilter filter, final long offset, final long limit) {
        return ALL_SCENARIOS.values().stream()
            .map(TestScenarioRunnable::getTestScenario)
            .filter(filter.getByTypeCriteria().getFilter())
            .filter(getPredicateByNameCriteria(filter.getByNameCriteria()))
            .skip(offset)
            .limit(limit);
    }

    public long getAllSize() {
        return getAll().count();
    }

    public long getAllSize(final TestScenarioFilter filter) {
        return getAll(filter, 0, Long.MAX_VALUE).count();
    }

    public Optional<TestScenario> findBy(final int id) {
        return ofNullable(ALL_SCENARIOS.get(id)).map(TestScenarioRunnable::getTestScenario);
    }

    public Stream<TestScenario> findBy(final Query<TestScenario, TestScenarioFilter> query) {
        return query.getFilter()
            .map(filter -> getAll(filter, query.getOffset(), query.getLimit()))
            .orElseGet(this::getAll);
    }

    public int countBy(final Query<TestScenario, TestScenarioFilter> query) {
        return query.getFilter()
            .map(this::getAllSize)
            .orElseGet(this::getAllSize)
            .intValue();
    }

    public void createAndSchedule(@NonNull final TestScenario testScenario) {
        lock.lock();
        try {
            final var testScenarioRunnable = new TestScenarioRunnable(applicationManager, testScenario);
            ofNullable(ALL_SCENARIOS.put(testScenario.getId(), testScenarioRunnable))
                .filter(oldTestScenarioRunnable -> oldTestScenarioRunnable.getTestScenario().isEnabled())
                .ifPresent(scheduleTestScenarioService::unschedule);
            if (testScenario.isEnabled()) {
                scheduleTestScenarioService.schedule(testScenarioRunnable);
            }
        } finally {
            lock.unlock();
        }
    }

    public void changeActivation(@NonNull final TestScenario testScenario) {
        lock.lock();
        try {
            testScenario.toggleOnEnabling();

            ofNullable(ALL_SCENARIOS.get(testScenario.getId())).ifPresent(testScenarioRunnable -> {
                if (testScenarioRunnable.getTestScenario().isEnabled()) {
                    scheduleTestScenarioService.unschedule(testScenarioRunnable);
                } else {
                    scheduleTestScenarioService.schedule(testScenarioRunnable);
                }
            });
        } finally {
            lock.unlock();
        }
    }

    public boolean changeCronExpression(final TestScenario testScenario, final String newCronExpression) {
        var isOperationPerformed = false;

        lock.lock();
        try {
            final var testScenarioRunnable = ALL_SCENARIOS.get(testScenario.getId());
            if (testScenario.isEnabled()) {
                scheduleTestScenarioService.unschedule(testScenarioRunnable);
            }
            isOperationPerformed = testScenario.setCron(newCronExpression);
            if (testScenario.isEnabled()) {
                scheduleTestScenarioService.schedule(testScenarioRunnable);
            }
        } finally {
            lock.unlock();
        }

        return isOperationPerformed;
    }

    public boolean changeName(final TestScenario testScenario, final String newName) {
        var isOperationPerformed = false;

        lock.lock();
        try {
            isOperationPerformed = testScenario.setName(newName);
        } finally {
            lock.unlock();
        }

        return isOperationPerformed;
    }

    public void delete(final TestScenario testScenario) {
        lock.lock();
        try {
            final var testScenarioRunnable = ALL_SCENARIOS.remove(testScenario.getId());
            if (testScenario.isEnabled()) {
                scheduleTestScenarioService.unschedule(testScenarioRunnable);
            }
        } finally {
            lock.unlock();
        }
    }

    private Predicate<TestScenario> getPredicateByNameCriteria(final String byNameCriteria) {
        return StringUtils.isNotEmpty(byNameCriteria)
            ? testScenario -> StringUtils.containsIgnoreCase(testScenario.getName(), byNameCriteria)
            : ALWAYS_TRUE_PREDICATE;
    }

}
