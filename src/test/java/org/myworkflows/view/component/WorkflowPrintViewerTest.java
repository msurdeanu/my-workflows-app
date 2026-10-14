package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.html.NativeDetails;
import com.vaadin.flow.component.html.NativeTableRow;
import com.vaadin.flow.component.html.Pre;
import com.vaadin.flow.component.tabs.Tab;
import com.vaadin.flow.component.tabs.Tabs;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRunPrint;
import org.myworkflows.view.transformer.WorkflowPrintValue;
import org.myworkflows.view.transformer.WorkflowPrintValue.Node;
import org.myworkflows.view.transformer.WorkflowPrintValue.Source;
import org.myworkflows.view.transformer.WorkflowPrintValue.Type;
import org.myworkflows.view.transformer.WorkflowRunPrintToValueTransformer;

import java.lang.ref.Reference;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowPrintViewerTest {

    @Test
    public void whenValueIsATextThenItIsShownAsBeforeWithoutTabs() {
        // when
        final var viewer = createViewer("greeting", "Hello, Ana!");

        // then
        assertEquals(0, find(viewer, Tabs.class).size());
        assertEquals(1, find(viewer, WorkflowPrintDetailsFormLayout.class).size());
    }

    @Test
    public void whenValueIsAMapThenItOpensAsATreeWhoseFirstLevelIsOpen() {
        // given
        final var order = new LinkedHashMap<String, Object>();
        order.put("id", 10482);
        order.put("customer", Map.of("address", Map.of("city", "Cluj")));
        order.put("none", Map.of());

        // when
        final var viewer = createViewer("order", order);

        // then
        assertEquals(3, find(viewer, Tab.class).size());
        assertEquals(0, find(viewer, Tabs.class).getFirst().getSelectedIndex());
        final var branches = find(viewer, NativeDetails.class);
        // a map without entries is no branch, and what the address holds is not drawn before somebody opens it
        assertEquals(2, branches.size());
        assertTrue(branches.getFirst().isOpen());
        assertFalse(branches.getLast().isOpen());
        assertEquals(List.of("id: 10482", "none: {}"), textsOf(viewer, "workflow-print-leaf"));
    }

    @Test
    public void whenALevelHasManyEntriesThenTheyAreShownAHundredAtATime() {
        // given
        final var viewer = createViewer("numbers", IntStream.range(0, 250).boxed().toList());

        // when and then
        assertEquals(WorkflowPrintViewer.PAGE_SIZE, textsOf(viewer, "workflow-print-leaf").size());
        find(viewer, Button.class).getLast().click();
        assertEquals(2 * WorkflowPrintViewer.PAGE_SIZE, textsOf(viewer, "workflow-print-leaf").size());
        find(viewer, Button.class).getLast().click();
        assertEquals(250, textsOf(viewer, "workflow-print-leaf").size());
        assertEquals("249: 249", textsOf(viewer, "workflow-print-leaf").getLast());

        // and nothing is left to ask for, but the button which copies
        assertEquals(1, find(viewer, Button.class).size());
    }

    @Test
    public void whenTheFirstLevelIsLongThenItsBranchesStayClosed() {
        // when
        final var viewer = createViewer("hits", IntStream.range(0, 11).mapToObj(index -> Map.of("id", index)).toList());

        // then
        final var branches = find(viewer, NativeDetails.class);
        assertEquals(11, branches.size());
        assertTrue(branches.stream().noneMatch(NativeDetails::isOpen));
        assertEquals(List.of(), textsOf(viewer, "workflow-print-leaf"));
    }

    @Test
    public void whenATextHoldsJsonThenItOpensAsJsonAndWhatItHoldsIsEscaped() {
        // when
        final var viewer = createViewer("response", "{\"<b>key</b>\": \"<img src=x onerror=alert(1)> & more\", \"count\": 2}");

        // then
        assertEquals(1, find(viewer, Tabs.class).getFirst().getSelectedIndex());
        final var html = find(viewer, Pre.class).getFirst().getElement().getProperty("innerHTML");
        assertFalse(html.contains("<img"));
        assertFalse(html.contains("<b>"));
        assertTrue(html.contains("&quot;&lt;b&gt;key&lt;/b&gt;&quot;"));
        assertTrue(html.contains("&lt;img src=x onerror=alert(1)&gt; &amp; more"));
        assertEquals(4, html.lines().count());
        assertTrue(html.lines().toList().get(2).endsWith(
            "<span class=\"workflow-print-key\">&quot;count&quot;</span><span class=\"workflow-print-mark\">: </span>"
                + "<span class=\"workflow-print-number\">2</span></span>"));
    }

    @Test
    public void whenAnotherTabIsChosenThenItsViewIsShownAndATreeIsFoundAsItWasLeft() {
        // given a tree whose branch was opened by hand
        final var viewer = createViewer("hits", IntStream.range(0, 11).mapToObj(index -> Map.of("id", index)).toList());
        final var tabs = find(viewer, Tabs.class).getFirst();
        final var branch = find(viewer, NativeDetails.class).getFirst();

        // when
        tabs.setSelectedIndex(1);

        // then
        assertEquals(1, find(viewer, Pre.class).size());
        assertEquals(0, find(viewer, NativeDetails.class).size());

        // when
        tabs.setSelectedIndex(2);

        // then
        assertEquals(0, find(viewer, Pre.class).size());
        assertEquals(1, find(viewer, WorkflowPrintDetailsFormLayout.class).size());

        // when
        tabs.setSelectedIndex(0);

        // then
        assertEquals(branch, find(viewer, NativeDetails.class).getFirst());
    }

    @Test
    public void whenJsonIsTooLongThenItIsShownLikeALongText() {
        // given entries which are few enough for a tree and long enough for a download
        final var texts = IntStream.range(0, 300).mapToObj(index -> "x".repeat(1_000)).toList();
        final var viewer = createViewer("texts", texts);
        // the link of a download is written for the page it is on, which is remembered by a weak reference only
        final var page = new UI();
        UI.setCurrent(page);

        try {
            // when
            find(viewer, Tabs.class).getFirst().setSelectedIndex(1);

            // then
            assertEquals(0, find(viewer, Pre.class).size());
            assertEquals(1, find(viewer, Anchor.class).size());
        } finally {
            UI.setCurrent(null);
            Reference.reachabilityFence(page);
        }
    }

    @Test
    public void whenValueIsATableThenItsRowsAreShownAHundredAtATime() {
        // given
        final var rows = IntStream.range(0, 150)
            .mapToObj(index -> Node.branch(String.valueOf(index), Type.MAP, List.of(
                Node.leaf("name", Type.STRING, "row " + index), Node.leaf("runs", Type.NUMBER, String.valueOf(index)))))
            .toList();
        final var viewer = new WorkflowPrintViewer();

        // when
        viewer.setPrint("runs", "cachedrowsetimpl", new WorkflowPrintValue(Source.TABLE, "name,runs", Node.branch(null, Type.LIST, rows), List.of("name", "runs"), 150));

        // then the header and the first rows
        assertEquals(2, find(viewer, Tab.class).size());
        assertEquals(1 + WorkflowPrintViewer.PAGE_SIZE, find(viewer, NativeTableRow.class).size());
        final var header = find(viewer, NativeTableRow.class).getFirst();
        assertEquals(List.of("#", "name", "runs"), header.getHeaderCells().stream().map(cell -> cell.getText()).toList());
        assertFalse(header.getHeaderCells().get(1).hasClassName("workflow-print-number"));
        assertTrue(header.getHeaderCells().get(2).hasClassName("workflow-print-number"));

        // when
        find(viewer, Button.class).getLast().click();

        // then
        final var tableRows = find(viewer, NativeTableRow.class);
        assertEquals(151, tableRows.size());
        assertEquals(List.of("150", "row 149", "149"), tableRows.getLast().getDataCells().stream().map(cell -> cell.getText()).toList());
        assertEquals(1, find(viewer, Button.class).size());
    }

    private WorkflowPrintViewer createViewer(String name, Object value) {
        final var print = new WorkflowRunPrint(name, value);
        final var viewer = new WorkflowPrintViewer();
        viewer.setPrint(print.name(), print.type(), new WorkflowRunPrintToValueTransformer().transform(print));
        return viewer;
    }

    private List<String> textsOf(Component root, String className) {
        return find(root, Component.class).stream()
            .filter(component -> component.getElement().getClassList().contains(className))
            .map(component -> component.getChildren().map(child -> child.getElement().getText()).reduce("", String::concat))
            .toList();
    }

    private <T extends Component> List<T> find(Component root, Class<T> type) {
        return descendantsOf(root).filter(type::isInstance).map(type::cast).toList();
    }

    private Stream<Component> descendantsOf(Component root) {
        return root.getChildren().flatMap(child -> Stream.concat(Stream.of(child), descendantsOf(child)));
    }

}
