package org.myworkflows.view.component;

import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.view.transformer.WorkflowParameterToComponentSupplierObjectTransformer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowDevParamGrid extends AbstractParameterGrid {

    private static final WorkflowParameterToComponentSupplierObjectTransformer WORKFLOW_PARAMETER_TO_COMPONENT_SUPPLIER_OBJECT_TRANSFORMER =
        new WorkflowParameterToComponentSupplierObjectTransformer();

    private final List<WorkflowParameter> workflowParameters = new ArrayList<>();

    private final Map<String, WorkflowParameterToComponentSupplierObjectTransformer.ComponentValueSupplier> workflowParameterFields = new HashMap<>();

    public WorkflowDevParamGrid() {
        super("workflow-dev-param");
        setRenderValueFunction(workflowParameter -> {
            final var componentSupplierObject = WORKFLOW_PARAMETER_TO_COMPONENT_SUPPLIER_OBJECT_TRANSFORMER.transform(workflowParameter);
            workflowParameterFields.put(workflowParameter.getName(), componentSupplierObject.getComponentValueSupplier());
            return componentSupplierObject.getComponent();
        });
        setCreateFunction(value -> {
            final var workflowParameter = WorkflowParameter.of(value);
            addParameters(List.of(workflowParameter));
            return workflowParameter;
        });
        setDeleteConsumer(workflowParameter -> {
            workflowParameters.remove(workflowParameter);
            refreshPage();
        });
        setItems(workflowParameters);
    }

    public void addParameters(Collection<WorkflowParameter> parameters) {
        workflowParameters.addAll(parameters);
        refreshPage();
    }

    public Map<String, List<String>> getParametersForQuery() {
        final var names = new ArrayList<String>(workflowParameters.size());
        final var types = new ArrayList<String>(workflowParameters.size());
        final var values = new ArrayList<String>(workflowParameters.size());
        workflowParameters.forEach(workflowParameter -> {
            names.add(workflowParameter.getName());
            types.add(workflowParameter.getType().getValue());
            values.add(ofNullable(workflowParameterFields.get(workflowParameter.getName()))
                .map(WorkflowParameterToComponentSupplierObjectTransformer.ComponentValueSupplier::getValueAsStringSupplier)
                .orElseGet(() -> workflowParameter::getValue)
                .get());
        });
        final var parameters = new HashMap<String, List<String>>(3);
        parameters.put("n", names);
        parameters.put("t", types);
        parameters.put("v", values);
        return parameters;
    }

    public Map<String, Object> getParametersAsMap() {
        return workflowParameters.stream().collect(Collectors.toMap(WorkflowParameter::getName,
            workflowParameter -> ofNullable(workflowParameterFields.get(workflowParameter.getName()))
                .map(WorkflowParameterToComponentSupplierObjectTransformer.ComponentValueSupplier::getValueSupplier)
                .orElseGet(() -> workflowParameter::getComputedValue)
                .get(), (it1, it2) -> it2));
    }

}
