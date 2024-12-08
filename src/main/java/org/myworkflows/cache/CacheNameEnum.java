package org.myworkflows.cache;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.Arrays;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@RequiredArgsConstructor
public enum CacheNameEnum {
    WORKFLOW_PLACEHOLDER("workflowPlaceholder"),
    WORKFLOW_PARAMETER("workflowParameter"),
    WORKFLOW_DEFINITION("workflowDefinition"),
    WORKFLOW_TEMPLATE("workflowTemplate"),
    WORKFLOW_RUN("workflowRun"),
    MENU_ITEM("menuItem"),
    DOC_PAGE("docPage"),
    DEFAULT("default");

    public static final String WORKFLOW_PLACEHOLDER_NAME = "workflowPlaceholder";
    public static final String WORKFLOW_PARAMETER_NAME = "workflowParameter";
    public static final String WORKFLOW_DEFINITION_NAME = "workflowDefinition";
    public static final String WORKFLOW_TEMPLATE_NAME = "workflowTemplate";
    public static final String WORKFLOW_RUN_NAME = "workflowRun";
    public static final String DOC_PAGE_NAME = "docPage";

    private final String name;

    public static CacheNameEnum of(String name) {
        return Arrays.stream(CacheNameEnum.values())
            .filter(cacheName -> cacheName.name.equals(name))
            .findFirst().orElse(DEFAULT);
    }
}
