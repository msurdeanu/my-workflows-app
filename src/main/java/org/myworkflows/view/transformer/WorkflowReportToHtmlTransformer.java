package org.myworkflows.view.transformer;

import org.commonmark.ext.gfm.tables.TablesExtension;
import org.commonmark.parser.Parser;
import org.commonmark.renderer.html.HtmlRenderer;
import org.myworkflows.domain.WorkflowReportFormat;

import java.util.List;

/**
 * Turns the text of a report into the page the reports page shows in its frame. An HTML report is that page already. A
 * Markdown one is rendered here, on the server, and not by the Markdown viewer of the documentation, which would put any
 * HTML written in it into the page of the application: whatever a report holds is shown in a frame which runs no script.
 *
 * @author Mihai Surdeanu
 * @since 2.3
 */
public final class WorkflowReportToHtmlTransformer {

    /**
     * The look of a Markdown report: light, like the application whatever the mode of the system, and a link opens in a
     * tab of its own, as the frame cannot be left.
     */
    private static final String MARKDOWN_HEAD = """
        <!doctype html>
        <html>
        <head>
        <meta charset="utf-8">
        <base target="_blank">
        <style>
        :root { color-scheme: light; }
        body { margin: 0; padding: 16px 20px; background: Canvas; color: CanvasText; line-height: 1.55;
               font: 15px/1.55 system-ui, -apple-system, "Segoe UI", Roboto, sans-serif; overflow-wrap: anywhere; }
        h1, h2, h3, h4 { line-height: 1.25; margin: 1.2em 0 .5em; }
        h1:first-child, h2:first-child, h3:first-child { margin-top: 0; }
        h1 { font-size: 1.6em; } h2 { font-size: 1.3em; } h3 { font-size: 1.1em; }
        table { border-collapse: collapse; margin: .8em 0; font-variant-numeric: tabular-nums; display: block; overflow-x: auto; }
        th, td { border: 1px solid color-mix(in srgb, CanvasText 20%, transparent); padding: 4px 10px; }
        th { background: color-mix(in srgb, CanvasText 6%, transparent); }
        code, pre { font-family: ui-monospace, "Cascadia Mono", Consolas, monospace; font-size: .9em; }
        code { background: color-mix(in srgb, CanvasText 8%, transparent); padding: 1px 4px; border-radius: 4px; }
        pre { background: color-mix(in srgb, CanvasText 6%, transparent); padding: 10px 12px; border-radius: 6px; overflow-x: auto; }
        pre code { background: none; padding: 0; }
        blockquote { margin: .8em 0; padding: 0 1em; border-left: 3px solid color-mix(in srgb, CanvasText 25%, transparent); }
        img { max-width: 100%; }
        hr { border: 0; border-top: 1px solid color-mix(in srgb, CanvasText 20%, transparent); }
        </style>
        </head>
        <body>
        """;

    private static final String MARKDOWN_TAIL = """
        </body>
        </html>
        """;

    private final Parser parser;

    private final HtmlRenderer renderer;

    public WorkflowReportToHtmlTransformer() {
        final var extensions = List.of(TablesExtension.create());
        parser = Parser.builder().extensions(extensions).build();
        renderer = HtmlRenderer.builder().extensions(extensions).sanitizeUrls(true).build();
    }

    public String transform(WorkflowReportFormat format, String content) {
        if (format == WorkflowReportFormat.HTML) {
            return content;
        }
        return MARKDOWN_HEAD + renderer.render(parser.parse(content)) + MARKDOWN_TAIL;
    }

}
