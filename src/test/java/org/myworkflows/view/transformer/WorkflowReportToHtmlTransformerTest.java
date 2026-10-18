package org.myworkflows.view.transformer;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowReportFormat;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.3
 */
public final class WorkflowReportToHtmlTransformerTest {

    private final WorkflowReportToHtmlTransformer transformer = new WorkflowReportToHtmlTransformer();

    @Test
    public void whenReportIsHtmlThenItIsShownAsItWasWritten() {
        final var html = "<html><body><h1>Status</h1></body></html>";
        assertEquals(html, transformer.transform(WorkflowReportFormat.HTML, html));
    }

    @Test
    public void whenReportIsMarkdownThenItIsAPageWithItsHeadingsAndTables() {
        // when
        final var html = transformer.transform(WorkflowReportFormat.MARKDOWN, """
            # Weekly sales

            | Region | Orders |
            |---|--:|
            | North | 1,284 |
            """);

        // then
        assertTrue(html.startsWith("<!doctype html>"));
        assertTrue(html.contains("<base target=\"_blank\">"));
        assertTrue(html.contains("<h1>Weekly sales</h1>"));
        assertTrue(html.contains("<th>Region</th>"));
        assertTrue(html.contains("<td align=\"right\">1,284</td>"));
    }

    @Test
    public void whenMarkdownHasAScriptLinkThenTheLinkIsDropped() {
        // when
        final var html = transformer.transform(WorkflowReportFormat.MARKDOWN, "[click](javascript:alert(1))");

        // then
        assertFalse(html.contains("javascript:"));
    }

}
