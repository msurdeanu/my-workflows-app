package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.NativeDetails;
import com.vaadin.flow.component.html.NativeTable;
import com.vaadin.flow.component.html.NativeTableBody;
import com.vaadin.flow.component.html.Pre;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.tabs.Tab;
import com.vaadin.flow.component.tabs.Tabs;
import com.vaadin.flow.component.tabs.TabsVariant;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.util.LangUtil;
import org.myworkflows.view.transformer.WorkflowPrintValue;
import org.myworkflows.view.transformer.WorkflowPrintValue.JsonLine;
import org.myworkflows.view.transformer.WorkflowPrintValue.Node;
import org.myworkflows.view.transformer.WorkflowPrintValue.Type;
import org.myworkflows.view.transformer.WorkflowPrintValue.View;
import org.myworkflows.view.util.ClipboardUtil;

import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.joining;
import static org.springframework.web.util.HtmlUtils.htmlEscape;

/**
 * A printed value under its row. A value which is more than a text can be looked at in more than one way - as a tree,
 * as JSON, as a table - and says so with a tab per way. A tree and a table start with their first entries and show
 * the next ones when they are asked for, so that a large value costs nothing until somebody wants to see it.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowPrintViewer extends Div {

    static final int PAGE_SIZE = 100;

    /**
     * A tree opens its first level by itself, unless that level is long enough to fill the screen on its own.
     */
    private static final int MAX_OPEN_BRANCHES = 10;

    private static final int MAX_CELL_LENGTH = 400;

    /**
     * JSON which is longer than this is offered as a download, like a long text is.
     */
    private static final int MAX_JSON_LENGTH = 262_144;

    private final Map<View, Component> components = new EnumMap<>(View.class);
    private final Div content = new Div();
    private final Span info = new Span();

    private String name;
    private String type;
    private WorkflowPrintValue value;
    private View view;
    private int jsonLineCount;

    public WorkflowPrintViewer() {
        addClassName("workflow-print-viewer");
        info.addClassName("workflow-print-info");
    }

    /**
     * @param printName the name the value was printed under.
     * @param printType the type of the value, as it is named next to its text.
     */
    public void setPrint(String printName, String printType, WorkflowPrintValue printValue) {
        name = printName;
        type = printType;
        value = printValue;
        components.clear();
        removeAll();

        final var views = printValue.views();
        if (views.size() > 1) {
            add(createToolbar(views));
        }
        add(content);
        show(printValue.preferredView());
    }

    private Component createToolbar(List<View> views) {
        final var tabs = new Tabs();
        tabs.addThemeVariants(TabsVariant.SMALL);
        views.forEach(item -> tabs.add(new Tab(getTranslation("workflow-print.viewer.view." + item.name().toLowerCase()))));
        tabs.setSelectedIndex(views.indexOf(value.preferredView()));
        tabs.addSelectedChangeListener(_ -> show(views.get(tabs.getSelectedIndex())));

        final var copyButton = new Button(getTranslation("workflow-print.viewer.copy"), VaadinIcon.COPY.create());
        copyButton.addThemeVariants(ButtonVariant.SMALL, ButtonVariant.TERTIARY);
        copyButton.setTooltipText(getTranslation("workflow-print.viewer.copy.tooltip"));
        copyButton.addClickListener(_ -> {
            // what is copied is what is on display: a tree is copied as the JSON it stands for, a table as CSV
            ClipboardUtil.copyTo(getElement(), view == View.TREE || view == View.JSON ? value.json() : value.text());
            Notification.show(getTranslation("workflow-print.viewer.copied.message"));
        });

        final var actions = new Div(info, copyButton);
        actions.addClassName("workflow-print-actions");
        final var toolbar = new Div(tabs, actions);
        toolbar.addClassName("workflow-print-toolbar");
        return toolbar;
    }

    private void show(View selectedView) {
        view = selectedView;
        content.removeAll();
        // a view is built once, so that a tree is found again as it was left
        content.add(components.computeIfAbsent(selectedView, this::createView));
        info.setText(describe(selectedView));
    }

    private Component createView(View selectedView) {
        return switch (selectedView) {
            case TREE -> createTree();
            case JSON -> createJson();
            case TABLE -> createTable();
            case TEXT -> createText(value.text());
        };
    }

    private String describe(View selectedView) {
        return switch (selectedView) {
            case TREE -> count(value.root().type() == Type.LIST ? "item" : "key", value.root().children().size());
            case JSON -> count("line", jsonLineCount);
            case TABLE -> describeTable();
            case TEXT -> StringUtils.EMPTY;
        };
    }

    private String describeTable() {
        final var rows = value.root().children().size();
        final var columns = count("column", value.columns().size());
        return rows < value.totalRows()
            ? getTranslation("workflow-print.viewer.table.partial", rows, count("row", value.totalRows()), columns)
            : getTranslation("workflow-print.viewer.table.all", count("row", rows), columns);
    }

    private String count(String word, int count) {
        final var translatedWord = getTranslation("workflow-print.count." + word);
        return LangUtil.pluralize(translatedWord, count).orElseGet(() -> getTranslation("workflow-print.count.none", translatedWord));
    }

    private Component createText(String text) {
        final var layout = new WorkflowPrintDetailsFormLayout();
        layout.setText(name, type, text);
        return layout;
    }

    private Component createTree() {
        final var tree = new Div();
        tree.addClassName("workflow-print-tree");
        final var root = value.root();
        if (root.children().isEmpty()) {
            tree.add(createValue(root));
        }
        addNodes(tree, root, 0, root.children().size() <= MAX_OPEN_BRANCHES);
        return tree;
    }

    private void addNodes(Div parent, Node node, int from, boolean open) {
        final var children = node.children();
        final var to = Math.min(from + PAGE_SIZE, children.size());
        children.subList(from, to).forEach(child -> parent.add(createNode(child, node.type() == Type.LIST, open)));
        if (to < children.size()) {
            final var moreButton = createMoreButton(children.size() - to);
            moreButton.addClickListener(_ -> {
                parent.remove(moreButton);
                addNodes(parent, node, to, false);
            });
            parent.add(moreButton);
        }
    }

    private Component createNode(Node node, boolean indexed, boolean open) {
        final var key = new Span(node.key());
        key.addClassName(indexed ? "workflow-print-index" : "workflow-print-key");
        if (!node.isBranch() || node.children().isEmpty()) {
            final var colon = new Span(": ");
            colon.addClassName(classOf(Type.MAP));
            final var leaf = new Div(key, colon, createValue(node));
            leaf.addClassName("workflow-print-leaf");
            return leaf;
        }

        final var list = node.type() == Type.LIST;
        final var size = new Span(getTranslation(list ? "workflow-print.viewer.branch.list" : "workflow-print.viewer.branch.map", node.children().size()));
        size.addClassName("workflow-print-size");
        final var children = new Div();
        children.addClassName("workflow-print-children");
        final var branch = new NativeDetails();
        branch.addClassName("workflow-print-branch");
        branch.setSummary(key, size);
        branch.setContent(children);
        // what a branch holds is drawn when it is opened for the first time
        branch.addToggleListener(event -> {
            if (event.isOpened() && children.getComponentCount() == 0) {
                addNodes(children, node, 0, false);
            }
        });
        if (open) {
            addNodes(children, node, 0, false);
            branch.setOpen(true);
        }
        return branch;
    }

    /**
     * @return the value of a node which holds nothing else - a map and a list without entries too - in the colour of
     *     its type. A text is in quotes, so that it is told apart from a number and from a literal which read the same.
     */
    private Span createValue(Node node) {
        final var text = switch (node.type()) {
            case MAP -> "{}";
            case LIST -> "[]";
            case STRING -> node.jsonText();
            default -> node.text();
        };
        final var span = new Span(StringUtils.abbreviate(text, MAX_CELL_LENGTH));
        span.addClassName(classOf(node.type()));
        return span;
    }

    private Component createJson() {
        final var lines = value.jsonLines();
        jsonLineCount = lines.size();
        if (lines.stream().mapToInt(line -> line.text().length() + 1).sum() > MAX_JSON_LENGTH) {
            return createText(value.json());
        }
        final var pre = new Pre();
        // every text which comes from the value is escaped on its way into the markup
        pre.getElement().setProperty("innerHTML", lines.stream().map(this::toHtml).collect(joining("\n")));
        final var json = new Div(pre);
        json.addClassName("workflow-print-json");
        return json;
    }

    private String toHtml(JsonLine line) {
        final var html = new StringBuilder("<span class=\"workflow-print-line\">").append(line.indent());
        if (line.key() != null) {
            html.append(toHtml("workflow-print-key", line.key())).append(toHtml(classOf(Type.MAP), ": "));
        }
        // whatever is neither a number nor a literal is written as a text in JSON, hence it is drawn as one
        html.append(toHtml(line.type() == Type.OTHER ? classOf(Type.STRING) : classOf(line.type()), line.value()));
        if (line.comma()) {
            html.append(toHtml(classOf(Type.MAP), ","));
        }
        return html.append("</span>").toString();
    }

    private String toHtml(String className, String text) {
        return "<span class=\"" + className + "\">" + htmlEscape(text) + "</span>";
    }

    private Component createTable() {
        final var table = new NativeTable();
        final var rows = value.root().children();
        final var header = table.getHead().addRow();
        final var position = header.addHeaderCell();
        position.setText("#");
        position.addClassName("workflow-print-index");
        for (int column = 0; column < value.columns().size(); column++) {
            final var headerCell = header.addHeaderCell();
            headerCell.setText(value.columns().get(column));
            // a column is headed the way its first row is written
            if (!rows.isEmpty() && rows.getFirst().children().get(column).type() == Type.NUMBER) {
                headerCell.addClassName(classOf(Type.NUMBER));
            }
        }
        final var box = new Div(table);
        box.addClassName("workflow-print-table");
        addRows(box, table.getBody(), 0);
        return box;
    }

    private void addRows(Div box, NativeTableBody body, int from) {
        final var rows = value.root().children();
        final var to = Math.min(from + PAGE_SIZE, rows.size());
        for (int index = from; index < to; index++) {
            final var row = body.addRow();
            final var position = row.addDataCell();
            position.setText(String.valueOf(index + 1));
            position.addClassName("workflow-print-index");
            rows.get(index).children().forEach(cell -> {
                final var dataCell = row.addDataCell();
                dataCell.setText(StringUtils.abbreviate(cell.text(), MAX_CELL_LENGTH));
                dataCell.addClassName(classOf(cell.type()));
            });
        }
        if (to < rows.size()) {
            final var moreButton = createMoreButton(rows.size() - to);
            moreButton.addClickListener(_ -> {
                box.remove(moreButton);
                addRows(box, body, to);
            });
            box.add(moreButton);
        }
    }

    private Button createMoreButton(int left) {
        final var moreButton = new Button(getTranslation("workflow-print.viewer.more", left));
        moreButton.addThemeVariants(ButtonVariant.SMALL, ButtonVariant.TERTIARY);
        moreButton.addClassName("workflow-print-more");
        return moreButton;
    }

    private String classOf(Type nodeType) {
        return switch (nodeType) {
            case MAP, LIST -> "workflow-print-mark";
            case STRING -> "workflow-print-string";
            case NUMBER -> "workflow-print-number";
            case LITERAL -> "workflow-print-literal";
            case MASKED -> "workflow-print-masked";
            case OTHER -> "workflow-print-other";
        };
    }

}
