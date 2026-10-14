package org.myworkflows.domain.transfer;

import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.serializer.SerializerFactory;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.spy;

/**
 * What the tests of the export and of the import are made of. A definition and a template get their id from the
 * database, so the ones made here are told theirs.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowTransferFixture {

    public static final String SLEEP_SCRIPT = """
        commands:
        - class: sleep
          name: Wait a little
          inputs:
          - name: sleep.time
            value: 10
        """;

    private WorkflowTransferFixture() {
    }

    public static WorkflowDefinitionScript script(String yaml) {
        return SerializerFactory.toObjectKeepingUnknownCommands(yaml, WorkflowDefinitionScript.class);
    }

    /**
     * @return a script which asks the given URL, where a placeholder can be named.
     */
    public static String requestScript(String url) {
        return """
            commands:
            - class: httpRequest
              name: Ask
              inputs:
              - name: httpRequest.url
                value: %s
            """.formatted(url);
    }

    public static WorkflowDefinition definition(int id, String name, String yaml) {
        final var definition = spy(WorkflowDefinition.of(name, script(yaml)));
        lenient().doReturn(id).when(definition).getId();
        return definition;
    }

    public static WorkflowTemplate template(int id, String name, String cron, boolean enabled, List<WorkflowDefinition> definitions,
                                            List<WorkflowParameter> parameters) {
        final var template = spy(WorkflowTemplate.of(name, new ArrayList<>(definitions), new ArrayList<>(parameters)));
        template.setCron(cron);
        if (enabled) {
            template.toggleOnEnabling();
        }
        lenient().doReturn(id).when(template).getId();
        return template;
    }

    public static WorkflowParameter parameter(String name, WorkflowParameterType type, String value) {
        return WorkflowParameter.of(name, type, value);
    }

    public static WorkflowPlaceholder placeholder(String name, String value) {
        return WorkflowPlaceholder.of(name, value);
    }

    public static WorkflowTransferContent content(List<WorkflowTemplate> templates, List<WorkflowDefinition> definitions,
                                                  List<WorkflowParameter> parameters, List<WorkflowPlaceholder> placeholders) {
        return new WorkflowTransferContent(templates, definitions, parameters, placeholders);
    }

}
