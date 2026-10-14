package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.ListItem;
import com.vaadin.flow.component.html.OrderedList;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.html.UnorderedList;
import com.vaadin.flow.component.icon.VaadinIcon;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.holder.CommandTypeHolder;
import org.myworkflows.util.LangUtil;
import org.myworkflows.view.transformer.WorkflowPipeline;
import org.myworkflows.view.transformer.WorkflowPipeline.Status;
import org.myworkflows.view.transformer.WorkflowPipeline.Step;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.IntStream;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.springframework.web.util.HtmlUtils.htmlEscape;

/**
 * The commands of a workflow drawn as the stops of a line, in the order they run: what each of them does, in a few
 * words, and - once a run was started - which of them the run has passed, which one it is at and where it failed. The
 * final commands end the line, apart from the others.
 *
 * @author Mihai Surdeanu
 * @since 2.0
 */
public final class WorkflowPipelinePanel extends Div {

    private final Div intro = new Div();
    private final OrderedList stopList = new OrderedList();
    private final List<Stop> stops = new ArrayList<>();
    private final List<Stop> finallyStops = new ArrayList<>();
    private final ListItem finallyHeading = new ListItem();

    private WorkflowPipeline pipeline = new WorkflowPipeline(List.of(), List.of());
    private UUID workflowRunId;

    public WorkflowPipelinePanel() {
        addClassName("workflow-pipeline");
        intro.addClassName("workflow-pipeline-intro");
        stopList.addClassName("workflow-pipeline-stops");
        finallyHeading.addClassName("workflow-pipeline-heading");
        finallyHeading.add(createRail(null), new Div(createDiv("workflow-pipeline-heading-title", getTranslation("workflow-pipeline.finally.title")),
            createDiv("workflow-pipeline-text", getTranslation("workflow-pipeline.finally.text"))));
        add(intro, stopList);
    }

    /**
     * Draws the pipeline of another workflow definition. The run which was followed until now is forgotten.
     */
    public void setPipeline(String definitionName, WorkflowPipeline pipeline) {
        this.pipeline = pipeline;
        workflowRunId = null;
        intro.getElement().setProperty("innerHTML", describe(definitionName));

        stopList.removeAll();
        stops.clear();
        finallyStops.clear();
        final var commands = pipeline.commands();
        final var finallyCommands = pipeline.finallyCommands();
        IntStream.range(0, commands.size())
            .mapToObj(index -> new Stop(commands.get(index), String.valueOf(index + 1)))
            .forEach(stops::add);
        finallyCommands.forEach(step -> finallyStops.add(new Stop(step, null)));
        if (!stops.isEmpty()) {
            stops.getLast().item.addClassName(finallyStops.isEmpty() ? "terminal" : "last");
        }
        if (!finallyStops.isEmpty()) {
            finallyStops.getLast().item.addClassName("terminal");
        }

        stops.forEach(stop -> stopList.add(stop.item));
        if (!finallyStops.isEmpty()) {
            stopList.add(finallyHeading);
        }
        finallyStops.forEach(stop -> stopList.add(stop.item));
        show(pipeline.noProgress());
    }

    /**
     * Follows the given run from now on, starting with every command still to run.
     */
    public void start(UUID workflowRunId) {
        this.workflowRunId = workflowRunId;
        show(pipeline.noProgress());
    }

    /**
     * Moves the line to where the given run has got to, provided it is the run which is followed.
     *
     * @return where the run is, in words, while one of its commands is running.
     */
    public Optional<String> setProgress(WorkflowRun workflowRun) {
        if (!workflowRun.getId().equals(workflowRunId)) {
            return empty();
        }

        final var progress = pipeline.progressOf(workflowRun);
        show(progress);
        final var runningCommand = progress.commands().indexOf(Status.RUNNING);
        if (runningCommand >= 0) {
            return of(getTranslation("workflow-pipeline.position.command", runningCommand + 1, stops.size(),
                pipeline.commands().get(runningCommand).name()));
        }
        final var runningFinallyCommand = progress.finallyCommands().indexOf(Status.RUNNING);
        return runningFinallyCommand < 0 ? empty() : of(getTranslation("workflow-pipeline.position.finally-command",
            pipeline.finallyCommands().get(runningFinallyCommand).name()));
    }

    private void show(WorkflowPipeline.Progress progress) {
        final var finallyStarted = progress.finallyCommands().stream()
            .anyMatch(status -> status == Status.RUNNING || status == Status.DONE || status == Status.FAILED);
        IntStream.range(0, stops.size()).forEach(index -> {
            final var status = progress.commands().get(index);
            // the line leaves the last command for the final ones even when that command was not reached
            stops.get(index).show(status, index == stops.size() - 1 ? finallyStarted : status == Status.DONE);
        });
        finallyHeading.setClassName("travelled", finallyStarted);
        IntStream.range(0, finallyStops.size()).forEach(index -> {
            final var status = progress.finallyCommands().get(index);
            finallyStops.get(index).show(status, status == Status.DONE);
        });
    }

    private String describe(String definitionName) {
        final var name = htmlEscape(definitionName);
        final var commands = pipeline.commands().size();
        final var text = new StringBuilder();
        if (commands == 0) {
            text.append(getTranslation("workflow-pipeline.intro.empty", name));
        } else {
            text.append(getTranslation(commands == 1 ? "workflow-pipeline.intro.one" : "workflow-pipeline.intro.many", name,
                pluralize("workflow-pipeline.command", commands)));
        }
        final var finallyCommands = pipeline.finallyCommands().size();
        if (finallyCommands > 0) {
            text.append(' ').append(getTranslation(finallyCommands == 1 ? "workflow-pipeline.intro.finally.one" : "workflow-pipeline.intro.finally.many",
                pluralize("workflow-pipeline.finally-command", finallyCommands)));
        }
        return text.toString();
    }

    private String pluralize(String wordKey, int count) {
        return LangUtil.pluralize(getTranslation(wordKey), count).orElse(StringUtils.EMPTY);
    }

    private Div createRail(Component dot) {
        final var line = new Span();
        line.addClassName("workflow-pipeline-line");
        final var rail = new Div();
        rail.addClassName("workflow-pipeline-rail");
        if (dot != null) {
            rail.add(dot);
        }
        rail.add(line);
        return rail;
    }

    private Div createDiv(String className, String text) {
        final var div = new Div(text);
        div.addClassName(className);
        return div;
    }

    private Span createSpan(String className, String text) {
        final var span = new Span(text);
        span.addClassName(className);
        return span;
    }

    private Badge createBadge(String text, BadgeVariant... variants) {
        final var badge = new Badge(text);
        badge.addThemeVariants(BadgeVariant.SMALL);
        badge.addThemeVariants(variants);
        return badge;
    }

    private String findTypeLabel(Step step) {
        // a command brought by a library has no translation: it goes by the name its definition gives it
        return isBuiltIn(step) ? getTranslation("workflow-pipeline.command." + step.type() + ".label") : step.type();
    }

    private String findTypeText(Step step) {
        if (isBuiltIn(step)) {
            return getTranslation("workflow-pipeline.command." + step.type() + ".text");
        }
        return getTranslation(CommandTypeHolder.INSTANCE.find(step.type()).isPresent()
            ? "workflow-pipeline.plugin-command.text"
            : "workflow-pipeline.unknown-command.text");
    }

    private boolean isBuiltIn(Step step) {
        return CommandTypeHolder.INSTANCE.find(step.type()).filter(CommandType::builtIn).isPresent();
    }

    /**
     * A command on the line: a dot which tells its status, next to what the command does.
     */
    private final class Stop {

        private final ListItem item = new ListItem();
        private final Span dot = new Span();
        private final Badge runningBadge = createBadge(getTranslation("workflow-pipeline.state.running"), BadgeVariant.WARNING);
        private final Badge failedBadge = createBadge(getTranslation("workflow-pipeline.state.failed"), BadgeVariant.ERROR);
        private final Badge skippedBadge = createBadge(getTranslation("workflow-pipeline.state.skipped"));

        /**
         * The number of the command, or nothing for a final command.
         */
        private final String label;

        private Status status;

        private Stop(Step step, String label) {
            this.label = label;
            item.addClassName("workflow-pipeline-stop");
            if (label == null) {
                item.addClassName("final");
            }
            dot.addClassName("workflow-pipeline-dot");
            dot.getElement().setAttribute("role", "img");

            final var title = new Div(createSpan("workflow-pipeline-name", step.name()), createBadge(findTypeLabel(step)),
                runningBadge, failedBadge, skippedBadge);
            title.addClassName("workflow-pipeline-title");
            final var body = new Div(title, createText(step));
            body.addClassName("workflow-pipeline-body");
            if (!step.subSteps().isEmpty()) {
                body.add(createSubSteps(step.subSteps()));
            }
            if (!step.stores().isEmpty()) {
                body.add(createMeta("workflow-pipeline.stores", step.stores(), "workflow-pipeline-code"));
            }
            if (!step.conditions().isEmpty()) {
                body.add(createMeta("workflow-pipeline.only-if", step.conditions(), "workflow-pipeline-condition"));
            }
            item.add(createRail(dot), body);
        }

        private void show(Status newStatus, boolean travelled) {
            item.setClassName("travelled", travelled);
            if (newStatus == status) {
                return;
            }
            if (status != null) {
                item.removeClassName(status.name().toLowerCase());
            }
            status = newStatus;
            item.addClassName(status.name().toLowerCase());

            dot.removeAll();
            dot.getElement().setAttribute("aria-label", getTranslation("workflow-pipeline.status." + status.name().toLowerCase()));
            switch (status) {
                case DONE -> dot.add(VaadinIcon.CHECK.create());
                case FAILED -> dot.add(VaadinIcon.CLOSE_SMALL.create());
                default -> dot.add(label == null ? VaadinIcon.FLAG.create() : new Span(label));
            }
            runningBadge.setVisible(status == Status.RUNNING);
            failedBadge.setVisible(status == Status.FAILED);
            skippedBadge.setVisible(status == Status.SKIPPED);
            if (status == Status.RUNNING) {
                // a long pipeline scrolls inside its panel: the command which is running is kept in sight
                item.getElement().executeJs("this.scrollIntoView({block: 'nearest'})");
            }
        }

        private Div createText(Step step) {
            if (step.detail().isEmpty()) {
                return createDiv("workflow-pipeline-text", findTypeText(step));
            }
            final var text = new Div(new Span(getTranslation("workflow-pipeline.command." + step.type() + ".detail")),
                createSpan("workflow-pipeline-code", step.detail()));
            text.addClassName("workflow-pipeline-text");
            return text;
        }

        private UnorderedList createSubSteps(List<Step> subSteps) {
            final var list = new UnorderedList();
            list.addClassName("workflow-pipeline-subs");
            subSteps.forEach(subStep -> {
                final var title = new Div(createSpan("workflow-pipeline-sub-name", subStep.name()), createBadge(findTypeLabel(subStep)));
                title.addClassName("workflow-pipeline-title");
                final var subItem = new ListItem(title);
                if (!subStep.subSteps().isEmpty()) {
                    subItem.add(createSubSteps(subStep.subSteps()));
                }
                list.add(subItem);
            });
            return list;
        }

        private Div createMeta(String labelKey, List<String> values, String valueClassName) {
            final var meta = new Div(new Span(getTranslation(labelKey)));
            meta.addClassName("workflow-pipeline-meta");
            values.forEach(value -> meta.add(createSpan(valueClassName, value)));
            return meta;
        }

    }

}
