# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & run

The project is Maven-based and targets **Java 25 + Spring Boot 4 + Vaadin 25**. SQLite is the default datastore; the file `database.db` is created in the working directory at first run and populated by Flyway migrations under `src/main/resources/db/migration/`.

```bash
mvn spring-boot:run                              # default goal — runs in dev mode (Vaadin dev server, hot reload)
mvn clean package -Pproduction                   # production build — bundles frontend, excludes vaadin-dev
mvn test                                         # full test suite
mvn -Dtest=EventBroadcasterTest test             # single test class
mvn -Dtest=EventBroadcasterTest#methodName test  # single test method
mvn checkstyle:check                             # checkstyle is bound to process-classes; runs on every build
```

Checkstyle config is at `checkstyle.xml`. Build will **fail** at the `process-classes` phase if violations exist. JaCoCo coverage report is generated in the `test` phase (excludes `view/**`).

The `production` profile excludes `vaadin-dev` and runs `vaadin-maven-plugin:build-frontend`. Without it, you get a dev-mode build that depends on the Vaadin dev server.

## Architecture

### Workflow execution model

A **Workflow** is a YAML script (validated against a JSON schema via `networknt/json-schema-validator`) listing **Commands**. Each command runs through a fixed state machine: inject inputs → run → if exception, abort; if has output → process asserts → process outputs. See `domain/command/` for built-in commands and the README state diagram for full semantics.

Commands are run sequentially on a thread from the `workflow-pool` `ExecutorService` (one workflow per thread). `finallyCommands` run regardless of failure — like `try/finally`.

### Command types

`CommandTypeHolder` is the one list of the command types: the built-in ones, named in `findBuiltInTypes()`, plus the ones a library brings (`loadPlugins`, called by `LibraryServiceLoader` — before the definitions are read). A `CommandType` is read from the class of a command: its name is the `prefix` of its `@ExecutionMethod`, its inputs (`CommandInput`: name, type, required, default value) are the parameters of that method with their `@ExecutionParam`, and its output is what the method returns. Two things are written from it alone, for built-in and plugin commands alike: the snippet of a command in the editor (`EditorSnippetUtil`, handed over to the JavaScript of `SnippetConfig` once Ace is ready) and the *Command reference* tab of `DocPageView`, which is not a `DocPage` but Markdown written by `CommandTypesToReferenceTransformer` every time it is shown. Everything that has to know the types asks the holder: `CommandTypeIdResolver` (the class behind a `class` field), `WorkflowDefinitionValidatorService` (the `enum` of `class` and the commands which require `subcommands` are added to `workflow_schema.json` when it is loaded), `EditorAutoCompleteUtil` and `WorkflowPipelinePanel`. What is built from the types — the YAML mappers of `SerializerFactory`, the schema — comes from `CommandTypeHolder.derive(...)`, which builds it again once the types change; a mapper has to be rebuilt because Jackson caches the deserializer it found for a `class` value.

A plugin command is declared in `META-INF/services/org.myworkflows.domain.command.AbstractCommand` of its JAR and found with `java.util.ServiceLoader`. A `class` value nobody knows is an error for `SerializerFactory.toObject` (saving, running from the editor) but not for `toObjectKeepingUnknownCommands` (reading the database, copying a script before a run): there it becomes an `UnknownCommand`, which is written back as it was read and fails only when it is run. A new built-in command is therefore: its class, a line in `findBuiltInTypes()`, its inputs in `EditorAutoCompleteUtil` and its pipeline translations (see the UI layer).

### Approval

`ApprovalCommand` blocks the thread of its run in `WorkflowRun.awaitApproval`, on the `CompletableFuture` of a `WorkflowRunApproval`: approve, reject, the timeout (`completeOnTimeout`) and a cancel (the interrupt) all settle that future, and only the first one counts. A command cannot reach a Spring bean, so the run tells its `changeListener` — set by `WorkflowScriptService` to broadcast a `WorkflowDefinitionOnProgressEvent` — when it starts and stops to wait; this is how `WorkflowRunGrid` redraws the row and `WorkflowDevelopmentView` its status. A decision goes through `WorkflowRunService.decideApproval`, with the approval the user was shown (`WorkflowApprovalDialog`), not the one the run waits for by then. The approval is transient, like everything about a run in progress.

### Debugging

A run started from the editor of `WorkflowDevelopmentView` carries a `WorkflowRunDebugger`, transient like an approval: `watching()` for *Run*, `pausingBefore(breakpoints)` for *Debug*; any other run - a template, the REST API, a read-only link - has none. `WorkflowScriptService.runCommands` walks no fixed range: before every command it reads where the run stands (`lastSuccessfulIndex + 1`) and asks the debugger whether to pause there. A pause is `WorkflowRun.awaitResume`, the twin of `awaitApproval`: the thread is parked on the `CompletableFuture` of a `WorkflowRunPause` until a `Resume` settles it - step, continue, stop, or the timeout of one hour, which stops - and the `changeListener` tells whoever follows the run. A `Resume` carries the script to go on with (the one of the editor, validated and parsed by the view), the command to go on with (`WorkflowRun.moveTo`) and the breakpoints: this is how an edit applies at the next step and how *From cursor* jumps. For a run which is over, the same is `WorkflowRunService.runFrom`: a replay from any command with any script, refused unless `WorkflowRun.isCompleted()`, so that a run is never started twice. Only the commands of `commands` can be paused before; the final commands always run through, after a stop too.

What a run holds is written down as text by the thread of the run (`WorkflowRunDebugger.snapshot`, `WorkflowRunVariable`) each time it pauses and when it ends, as nothing else may read the cache while commands change it; `WorkflowVariableGrid` shows that, never the cache. Breakpoints live in the browser only: `DebugGutterConfig` holds them as Ace anchors, which follow their line through an edit but not through a `setValue` of the server (reformatting, another definition), after which the view draws them again. The server knows neither the cursor nor an edit which was not synced yet, so every action first reads the editor (`debugState`: content, cursor row, breakpoint rows) and `ScriptCommandUtil` turns rows into the positions of commands from that very text. A click shortcut lets the browser handle the key as well, which types a letter into the editor under AltGr: the ones of the debugger call `setBrowserDefaultAllowed(false)`.

### Printed values

A print is a `WorkflowRunPrint`: a name and the value itself, as the cache holds it. `WorkflowRunPrintToValueTransformer` reads it into a `WorkflowPrintValue` for `WorkflowPrintGrid`, once per row (the grid keeps what it read by the identity of the print): a map, a collection or an array is a tree of `Node`s (`STRUCTURE`), a text which parses as a JSON object or array is the same tree (`JSON`), a `CachedRowSet` - unwrapped from its `Optional` - is a table whose rows are the children of the root (`TABLE`), read through `createShared()` so that the cursor of the run stays where it is, and anything else is the text it was (`TEXT`). The same holds for a value which cannot be read - more than `MAX_NODES` entries, deeper than `MAX_DEPTH`, changed by its run in the meantime: the transformer catches that and falls back to the text. Whatever sits under a key or in a column whose name contains `password` becomes a `MASKED` node, and the text of a value in which something was masked is written again from the tree. `WorkflowPrintViewer` is the details row: a tab per `View`, each built when it is first chosen. The tree is `NativeDetails` whose children are added on the first `toggle`, the table a `NativeTable`, both `PAGE_SIZE` entries at a time; the JSON is one `innerHTML` of spans built from `JsonLine`s, so every text which comes from the value goes through `htmlEscape` there. `WorkflowVariableGrid` has none of this: a variable is a text written down by the thread of the run (see Debugging), shown by `WorkflowPrintDetailsFormLayout` as the viewer shows a text.

### Runtime evaluators

Expression `value` fields support 4 evaluators selected by the `class` field:
- `plain` (default) — literal
- `groovy` — Groovy script
- `java` — Janino runtime compiler (subset of Java)
- `spel` — Spring Expression Language

Plus the **cache access pattern** `$(varName:Type.class)` which gets rewritten into a workflow-run-cache lookup before the underlying evaluator runs. Implemented via `RuntimeEvaluator` and the `WorkflowScriptService` pipeline.

### Persistence layer

- SQLite via `org.xerial:sqlite-jdbc` + `hibernate-community-dialects` (`SQLiteDialect`)
- Schema is owned by **Flyway** migrations (`V1__bootstrap.sql` … `V10__import_export.sql`). `spring.jpa.hibernate.ddl-auto=none` — never let Hibernate generate schema.
- A migration which SQL cannot express is a `JavaMigration` bean in `migration/`, which names its own version: `UserTokenHashMigration` is `V9`. Check there too before numbering the next script. Such a class uses plain JDBC and no class of the application, so that it does not change with them.
- Spring Boot 4 requires `spring-boot-starter-flyway` explicitly (the autoconfig was extracted from `spring-boot-autoconfigure`).
- `WorkflowRun.id` is a `UUID` stored as `BLOB` (16 bytes, big-endian) using `@JdbcTypeCode(SqlTypes.BINARY)`. Hibernate 7 forbids `@AttributeConverter` on `@Id` fields.
- Other domain types use `@Convert` with custom `AttributeConverter`s in `converter/` (e.g. `WorkflowDefinitionScriptToStringConverter` serializes the YAML script blob).

### Application skeleton

- `Application` — Spring Boot entry, also Vaadin `AppShellConfigurator` with `@Push` enabled (WebSockets for server-pushed UI updates).
- `ApplicationManager` — thin wrapper around `ApplicationContext` for runtime bean lookup (used by Groovy/Java commands that need to resolve services dynamically).
- `EventBroadcaster` — async pub/sub keyed by `Event` subtype. Subscribers register `Consumer<Event>` and receive callbacks on the `workflow-pool` executor. Used heavily for view-to-view updates and for streaming workflow progress. The consumers of an event run side by side, in no order: a view that lists the runs from the cache listens to `WorkflowRunOnChangeEvent`, which `WorkflowRunProgressService` publishes once the run is in the cache, and not to `WorkflowDefinitionOnProgressEvent` itself.
- `ApplicationReadyManager` — post-startup hooks (loads JAR libraries from `my-workflows.config.library.base-directory`, scheduled workflow templates, etc.).
- `LibraryServiceLoader` — one `URLClassLoader` over every JAR of the libraries directory. A `Library` is what it found when it loaded them, its size, date and SHA-256 included, kept in the `LIBRARY` cache only. `LibraryService.delete` therefore unloads everything, deletes the file, evicts it and loads again, under the lock an upload with `reload-after-upload` takes too: a loaded JAR is locked on Windows.
- `InternalCacheManager` (in `cache/`) — application-level cache fronting database queries. Cache sizes/orders are configured in `ApplicationConfig.cacheManager()` and one of them (`WORKFLOW_RUN`) reads its capacity from the `SettingProvider`.

### Settings layering

`SettingProvider` is built as a JDK dynamic proxy chaining `DatabaseSettingProvider → DefaultSettingProvider` (see `ApplicationConfig.settingProvider()`). Reads check the DB first; misses fall back to `DefaultSettingProvider` defaults. Settings are surfaced/edited from the UI via `SettingView`.

### UI layer (`view/`)

Each top-level Vaadin route is a single class in `view/`. Forms and grids reuse components from `view/component/`. `view/transformer/` adapts domain objects to UI representations. `KeyboardUtil` centralizes the dev-page shortcuts documented in the README. A grid page registers its filters with `ResponsiveLayout.createFilters(ROUTE, PageFilter...)`: `PageFilters` then mirrors them into the URL query parameters (via `replaceState`, never a push) and the session, and renders the removable chips; a filter carried by the path (`workflow/templates/42`) is handed over with `setPathFilterValues` from `setParameter`. A page gets the guide under its grid from `ResponsiveLayout.createGuide(page)`, added after the content: `PageGuide` reads every text from the translations under `<page>.guide` (legend, steps, notes) and takes only the sample components from the view. Every guide starts folded. A page whose content fills the screen instead of ending with a grid (`WorkflowDevelopmentView`) uses `createFoldedGuide(page)`, which keeps a minimum height for that content once the guide is opened. The Ace editor wrapper (`de.f0rce:ace`) is used in `WorkflowDevelopmentView`. Opened from a read-only link (`?ro`), that view shows a `WorkflowPipelinePanel` instead of the editor, which is hidden: `WorkflowDefinitionScriptToPipelineTransformer` turns the script of the definition into a `WorkflowPipeline`, whose `progressOf(WorkflowRun)` gives the status of every command from `lastSuccessfulIndex`, `lastSuccessfulFinallyIndex` and the failure of the run. A new built-in command type needs its `workflow-pipeline.command.<class>.label` and `.text` translations, plus a `.detail` one if it is added to `DETAIL_INPUTS`; a command brought by a library has none, and is drawn under its name with a text shared by all of them.

### Users

`UserService` is the one place that changes a user. It caches nothing - it reads `UserRepository` on every call - because the same rows decide who logs in (`ApplicationUserDetailsService`) and whose token is accepted (`UserTokenService`, which caches nothing either). A session keeps the copy of its user taken at login (`UserAccountDetails`), so `UserSessionGuard`, a `VaadinServiceInitListener`, compares that copy with the database before every navigation: a user who was disabled, changed role or got another password is logged out, and one whose `passwordChangeRequired` is set is forwarded to `ProfileView` whatever they asked for. For the same reason `UserService` checks in the database, not in the session, that whoever manages the users is an enabled admin. An admin never changes their own account (`UserException.Reason.OWN_ACCOUNT`), which is what keeps an enabled admin around. `UserView` (admins) and `ProfileView` (everybody) are the two pages; a new side menu entry is a row in `menu_items`, added by a migration.

### REST API (`restapi/`)

Disabled by default — gated by `my-workflows.config.feature.restApiEnabled`. A call is authenticated by one of the API tokens of a user: `UserTokenFilter` reads it from the `Authorization: Bearer` header only - the `token` URL parameter of the versions before 2.1 is ignored on purpose, as a URL ends up in access logs. `SecurityApiConfig` is the security chain for `/api/**`; `SecurityConfig` covers the Vaadin-served pages.

A user has several named tokens (`user_tokens`, `UserToken`), each with an optional expiry. Only the SHA-256 digest of a token is saved, so `UserTokenService.createOwnToken` returns the one copy of it (`UserTokenSecret`) and `ProfileView` shows it once (`UserTokenDialog`). `UserTokenService.findUserOf` is what accepts a token: it is found by its digest, has not expired and its user is enabled. A user manages only their own tokens.

### Import and export

`WorkflowTransferView` (admins, `workflow/transfer`) is two tabs over `WorkflowTransferService`: `WorkflowExportPanel` and `WorkflowImportPanel`. What travels is a `WorkflowBundle` (`domain/transfer/`): placeholders, parameters, definitions and templates, without a single id - a template names its definitions and its parameters, and an import finds what is already here by name, in a `WorkflowTransferContent` (a snapshot of the four caches). `WorkflowBundleSerializer` writes a bundle as one YAML text or as a zip archive - a manifest, plus a file per definition (its script alone) and per template - with the mappers of `SerializerFactory`, and reads it keeping unknown commands: a file comes from an instance which may have other libraries.

A parameter of a single template (`timeout.42`, see `WorkflowTransferContent.isOwn`) is no item of its own: it is written under its template as `ownParameters`, by the name its workflow gets it under, and saved as that name followed by the id the template has here. A secret - type `PASS`, or `password` in its name - is written without its `value` unless the export is asked for it, so a missing `value` means "left out", never "empty".

`WorkflowExportPlan` resolves a selection - what the picks need and why, what is left out, which names are taken twice - and the panel computes it again on every click; the download is another request, which reads the selection from an `AtomicReference`. On the way in, `WorkflowImportReviewer` turns a bundle into a `WorkflowImportReview`: an item per element, with its `State`, its `target` here and its `WorkflowImportNote`s, or `WorkflowImportProblem`s alone when the file cannot be matched (a name it has twice, or which two items here go by). `check` adds what depends on the choices of the user (`WorkflowImportRequest`), like a template whose definition is skipped. `WorkflowTransferService.apply` reviews once more - other states than the ones which were shown end it with `INSTANCE_CHANGED` - then `WorkflowImportWriter.write` saves everything in one `@Transactional` call, through the repositories and on entities it reads itself. Only then are the caches brought in line, and what was already here is changed in place (`setNameAndScript`, `setTypeAndValue`, `WorkflowTemplateService.applyInternalUpdate`, which schedules the template anew), the way every other change of it is made: a page or a template which holds the object goes on with it. What a note quotes from the file is escaped (`htmlEscape`) before it becomes `innerHTML`.

### Event handlers (`domain/handler/`)

Each entity type (Workflow*, DocPage, Library) has an `*EventHandler` that subscribes to `EventBroadcaster` events and mediates between UI events and services. This is how the views avoid touching repositories directly.

## Conventions worth knowing

- **Lombok is required** — annotation processing must be configured. The `maven-compiler-plugin` declares `annotationProcessorPaths` because JDK 24+ no longer auto-discovers processors from the classpath.
- **All entities are `final` with private fields**, getters generated by Lombok. Constructors generated by `@AllArgsConstructor` / `@RequiredArgsConstructor`. Don't add a no-args constructor unless JPA needs it (then add it manually — Hibernate creates entities reflectively and handles missing default constructors via bytecode).
- **Variables whose name contains `password`** (case-insensitive) are masked in print output by `PrintCommand`.
- **Translations** live in `translation_en.properties`. Add a key, reference it via `TranslationProvider`.
- **Releases** are driven by `mvn release:prepare release:perform` from CI on `main` (`.github/workflows/on-main-push.yml`) — don't bump `<version>` manually.
