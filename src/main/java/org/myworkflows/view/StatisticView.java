package org.myworkflows.view;

import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.PermitAll;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.provider.StatisticProvider;
import org.myworkflows.view.component.StatisticTreeGrid;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@PermitAll
@Route(value = StatisticView.ROUTE, layout = BaseLayout.class)
public class StatisticView extends ResponsiveLayout implements HasDynamicTitle {

    public static final String ROUTE = "statistics";

    public StatisticView(final List<StatisticProvider> statisticProviders) {
        add(createHeader(getTranslation("statistics.page.title")),
                createContent(new StatisticTreeGrid(statisticProviders)),
                createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("statistics.page.title"));
    }

}
