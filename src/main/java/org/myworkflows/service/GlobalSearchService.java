package org.myworkflows.service;

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.domain.search.GlobalSearchResult;
import org.myworkflows.domain.search.GlobalSearchResult.Kind;
import org.myworkflows.domain.transfer.WorkflowBundle;
import org.myworkflows.holder.CommandTypeHolder;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static org.apache.commons.lang3.StringUtils.containsIgnoreCase;
import static org.apache.commons.lang3.StringUtils.startsWithIgnoreCase;
import static org.myworkflows.serializer.SerializerFactory.toPrettyString;

/**
 * The search of the header: definitions by their name and their script, templates by their name, the documentation
 * pages by their name and their text and the commands by their name, for everybody - a user who is no admin reads all of
 * them on their pages; parameters and placeholders by their name and their value, for admins only, as only an admin may
 * open their pages. The value of a secret is never searched, so that what a search finds does not tell it either.
 *
 * @author Mihai Surdeanu
 * @since 2.3
 */
@Service
@RequiredArgsConstructor
public class GlobalSearchService {

    private static final int EXCERPT_LENGTH = 80;

    private static final int EXACT_NAME = 0;
    private static final int NAME_PREFIX = 1;
    private static final int NAME_PART = 2;
    private static final int CONTENT = 3;

    private final WorkflowDefinitionService workflowDefinitionService;
    private final WorkflowTemplateService workflowTemplateService;
    private final WorkflowParameterService workflowParameterService;
    private final WorkflowPlaceholderService workflowPlaceholderService;
    private final DocPageService docPageService;
    private final UserService userService;

    /**
     * Has to be called on the thread serving the user, who is told an admin or not by the database (see
     * {@link UserService#isCurrentUserAnAdmin()}). Exact names come first, then names starting with the text, then names
     * holding it and last what holds it elsewhere.
     */
    public List<GlobalSearchResult> search(String text, int limit) {
        final var term = StringUtils.strip(text);
        if (StringUtils.isEmpty(term)) {
            return List.of();
        }

        var matches = Stream.of(findDefinitions(term), findTemplates(term), findDocPages(term), findCommands(term))
            .flatMap(stream -> stream);
        if (userService.isCurrentUserAnAdmin()) {
            matches = Stream.concat(matches, Stream.concat(findParameters(term), findPlaceholders(term)));
        }
        return matches
            .sorted(Comparator.comparingInt(Match::rank)
                .thenComparing(match -> match.result().kind())
                .thenComparing(match -> match.result().name(), String.CASE_INSENSITIVE_ORDER))
            .limit(limit)
            .map(Match::result)
            .toList();
    }

    private Stream<Match> findDefinitions(String term) {
        return workflowDefinitionService.getAllItems().flatMap(definition -> match(Kind.DEFINITION, definition.getId(),
            definition.getName(), toPrettyString(definition.getScript(), StringUtils.EMPTY), term).stream());
    }

    private Stream<Match> findTemplates(String term) {
        return workflowTemplateService.getAllItems().flatMap(template -> match(Kind.TEMPLATE, template.getId(),
            template.getName(), null, term).stream());
    }

    private Stream<Match> findDocPages(String term) {
        return docPageService.getAllNames().stream()
            .flatMap(name -> docPageService.findByName(name).stream())
            .flatMap(docPage -> match(Kind.DOC_PAGE, null, docPage.getName(), docPage.getValue(), term).stream());
    }

    private Stream<Match> findCommands(String term) {
        return CommandTypeHolder.INSTANCE.getAll().stream()
            .map(CommandType::name)
            .flatMap(name -> match(Kind.COMMAND, null, name, null, term).stream());
    }

    private Stream<Match> findParameters(String term) {
        return workflowParameterService.getAllItems().flatMap(parameter -> match(Kind.PARAMETER, null, parameter.getName(),
            isSecret(parameter) ? null : parameter.getValue(), term).stream());
    }

    private Stream<Match> findPlaceholders(String term) {
        return workflowPlaceholderService.getAllItems().flatMap(placeholder -> match(Kind.PLACEHOLDER, null, placeholder.getName(),
            WorkflowBundle.namesSecret(placeholder.getName()) ? null : placeholder.getValue(), term).stream());
    }

    private static boolean isSecret(WorkflowParameter parameter) {
        return parameter.getType() == WorkflowParameterType.PASS || WorkflowBundle.namesSecret(parameter.getName());
    }

    private static Optional<Match> match(Kind kind, Integer id, String name, String content, String term) {
        if (StringUtils.equalsIgnoreCase(name, term)) {
            return Optional.of(new Match(EXACT_NAME, new GlobalSearchResult(kind, id, name, null)));
        } else if (startsWithIgnoreCase(name, term)) {
            return Optional.of(new Match(NAME_PREFIX, new GlobalSearchResult(kind, id, name, null)));
        } else if (containsIgnoreCase(name, term)) {
            return Optional.of(new Match(NAME_PART, new GlobalSearchResult(kind, id, name, null)));
        }
        return findExcerpt(content, term).map(excerpt -> new Match(CONTENT, new GlobalSearchResult(kind, id, name, excerpt)));
    }

    /**
     * The first line which holds the text, cut around it when it is longer than {@link #EXCERPT_LENGTH}.
     */
    static Optional<String> findExcerpt(String content, String term) {
        if (!containsIgnoreCase(content, term)) {
            return Optional.empty();
        }
        return content.lines()
            .filter(line -> containsIgnoreCase(line, term))
            .findFirst()
            .map(String::strip)
            .map(line -> cut(line, term));
    }

    private static String cut(String line, String term) {
        if (line.length() <= EXCERPT_LENGTH) {
            return line;
        }
        final var at = StringUtils.indexOfIgnoreCase(line, term);
        final var start = Math.max(0, Math.min(at - (EXCERPT_LENGTH - term.length()) / 2, line.length() - EXCERPT_LENGTH));
        final var end = Math.min(line.length(), start + EXCERPT_LENGTH);
        return (start > 0 ? "…" : StringUtils.EMPTY) + line.substring(start, end).strip() + (end < line.length() ? "…" : StringUtils.EMPTY);
    }

    private record Match(int rank, GlobalSearchResult result) {

    }

}
