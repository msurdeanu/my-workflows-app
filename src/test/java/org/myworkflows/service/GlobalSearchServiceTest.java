package org.myworkflows.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.domain.DocPage;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.search.GlobalSearchResult;
import org.myworkflows.domain.search.GlobalSearchResult.Kind;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.definition;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.parameter;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.placeholder;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.requestScript;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.template;

/**
 * @author Mihai Surdeanu
 * @since 2.3
 */
@ExtendWith(MockitoExtension.class)
public final class GlobalSearchServiceTest {

    @Mock
    private WorkflowDefinitionService workflowDefinitionService;

    @Mock
    private WorkflowTemplateService workflowTemplateService;

    @Mock
    private WorkflowParameterService workflowParameterService;

    @Mock
    private WorkflowPlaceholderService workflowPlaceholderService;

    @Mock
    private DocPageService docPageService;

    @Mock
    private UserService userService;

    private GlobalSearchService globalSearchService;

    @BeforeEach
    public void init() {
        final var docPage = DocPage.of("Backups");
        docPage.setValue("""
            # Backups
            A backup is written by the vault definition every night.
            """);
        lenient().when(workflowDefinitionService.getAllItems()).thenAnswer(_ -> Stream.of(
            definition(1, "Vault", requestScript("https://vault.local/backup")),
            definition(2, "Backup", "commands: []"),
            definition(3, "Nightly backup", "commands: []")));
        lenient().when(workflowTemplateService.getAllItems()).thenAnswer(_ -> Stream.of(
            template(7, "Backup every night", "0 0 1 * * *", true, List.of(), List.of())));
        lenient().when(docPageService.getAllNames()).thenReturn(Set.of("Backups"));
        lenient().when(docPageService.findByName("Backups")).thenReturn(Optional.of(docPage));
        lenient().when(workflowParameterService.getAllItems()).thenAnswer(_ -> Stream.of(
            parameter("backup.target", WorkflowParameterType.STR, "s3://bucket"),
            parameter("vault.token", WorkflowParameterType.PASS, "backup-secret"),
            parameter("db.password", WorkflowParameterType.STR, "backup-secret"),
            parameter("region", WorkflowParameterType.STR, "backup-region")));
        lenient().when(workflowPlaceholderService.getAllItems()).thenAnswer(_ -> Stream.of(
            placeholder("BACKUP_HOST", "backup.local"),
            placeholder("SMTP_PASSWORD", "backup-secret"),
            placeholder("SMTP_HOST", "mail.backup.local")));
        globalSearchService = new GlobalSearchService(workflowDefinitionService, workflowTemplateService, workflowParameterService,
            workflowPlaceholderService, docPageService, userService);
    }

    @Test
    public void whenUserIsNoAdminThenParametersAndPlaceholdersAreNotSearched() {
        // given
        when(userService.isCurrentUserAnAdmin()).thenReturn(false);

        // when
        final var results = globalSearchService.search("backup", 20);

        // then
        assertTrue(results.stream().noneMatch(result -> result.kind().isForAdmins()));
        assertEquals(List.of(
            new GlobalSearchResult(Kind.DEFINITION, 2, "Backup", null),
            new GlobalSearchResult(Kind.TEMPLATE, 7, "Backup every night", null),
            new GlobalSearchResult(Kind.DOC_PAGE, null, "Backups", null),
            new GlobalSearchResult(Kind.DEFINITION, 3, "Nightly backup", null)), results.subList(0, 4));
        verify(workflowParameterService, never()).getAllItems();
        verify(workflowPlaceholderService, never()).getAllItems();
    }

    @Test
    public void whenTextIsInScriptThenTheDefinitionIsFoundWithTheLineWhichHoldsIt() {
        // given
        when(userService.isCurrentUserAnAdmin()).thenReturn(false);

        // when
        final var results = globalSearchService.search("vault.local", 20);

        // then
        assertEquals(1, results.size());
        assertEquals(Kind.DEFINITION, results.getFirst().kind());
        assertEquals(1, results.getFirst().id());
        assertTrue(results.getFirst().excerpt().contains("https://vault.local/backup"));
    }

    @Test
    public void whenUserIsAnAdminThenParametersAndPlaceholdersAreFoundButNotByTheValueOfASecret() {
        // given
        when(userService.isCurrentUserAnAdmin()).thenReturn(true);

        // when
        final var results = globalSearchService.search("backup", 20);

        // then
        final var adminResults = results.stream().filter(result -> result.kind().isForAdmins()).toList();
        assertEquals(List.of(
            new GlobalSearchResult(Kind.PARAMETER, null, "backup.target", null),
            new GlobalSearchResult(Kind.PLACEHOLDER, null, "BACKUP_HOST", null),
            new GlobalSearchResult(Kind.PARAMETER, null, "region", "backup-region"),
            new GlobalSearchResult(Kind.PLACEHOLDER, null, "SMTP_HOST", "mail.backup.local")), adminResults);
    }

    @Test
    public void whenTextIsTheNameOfASecretThenItIsFoundWithoutItsValue() {
        // given
        when(userService.isCurrentUserAnAdmin()).thenReturn(true);

        // when
        final var results = globalSearchService.search("password", 20);

        // then
        assertEquals(List.of(
            new GlobalSearchResult(Kind.PARAMETER, null, "db.password", null),
            new GlobalSearchResult(Kind.PLACEHOLDER, null, "SMTP_PASSWORD", null)), results);
    }

    @Test
    public void whenResultsAreMoreThanTheLimitThenTheBestOnesAreKept() {
        // given
        when(userService.isCurrentUserAnAdmin()).thenReturn(false);

        // when
        final var results = globalSearchService.search("  BACKUP ", 2);

        // then
        assertEquals(List.of("Backup", "Backup every night"), results.stream().map(GlobalSearchResult::name).toList());
    }

    @Test
    public void whenTextIsBlankThenNothingIsSearched() {
        // when
        final var results = globalSearchService.search(" ", 20);

        // then
        assertTrue(results.isEmpty());
        verify(userService, never()).isCurrentUserAnAdmin();
    }

    @Test
    public void whenLineIsLongThenTheExcerptIsCutAroundTheText() {
        // given
        final var line = "a".repeat(100) + " needle " + "b".repeat(100);

        // when
        final var excerpt = GlobalSearchService.findExcerpt("first line\n" + line, "needle").orElseThrow();

        // then
        assertTrue(excerpt.startsWith("…") && excerpt.endsWith("…"));
        assertTrue(excerpt.contains("needle"));
        assertTrue(excerpt.length() <= 82);
        assertNull(GlobalSearchService.findExcerpt("nothing here", "needle").orElse(null));
    }

}
