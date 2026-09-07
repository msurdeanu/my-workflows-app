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

### Runtime evaluators

Expression `value` fields support 4 evaluators selected by the `class` field:
- `plain` (default) — literal
- `groovy` — Groovy script
- `java` — Janino runtime compiler (subset of Java)
- `spel` — Spring Expression Language

Plus the **cache access pattern** `$(varName:Type.class)` which gets rewritten into a workflow-run-cache lookup before the underlying evaluator runs. Implemented via `RuntimeEvaluator` and the `WorkflowScriptService` pipeline.

### Persistence layer

- SQLite via `org.xerial:sqlite-jdbc` + `hibernate-community-dialects` (`SQLiteDialect`)
- Schema is owned by **Flyway** migrations (`V1__bootstrap.sql` … `V4__sb4java25.sql`). `spring.jpa.hibernate.ddl-auto=none` — never let Hibernate generate schema.
- Spring Boot 4 requires `spring-boot-starter-flyway` explicitly (the autoconfig was extracted from `spring-boot-autoconfigure`).
- `WorkflowRun.id` is a `UUID` stored as `BLOB` (16 bytes, big-endian) using `@JdbcTypeCode(SqlTypes.BINARY)`. Hibernate 7 forbids `@AttributeConverter` on `@Id` fields.
- Other domain types use `@Convert` with custom `AttributeConverter`s in `converter/` (e.g. `WorkflowDefinitionScriptToStringConverter` serializes the YAML script blob).

### Application skeleton

- `Application` — Spring Boot entry, also Vaadin `AppShellConfigurator` with `@Push` enabled (WebSockets for server-pushed UI updates).
- `ApplicationManager` — thin wrapper around `ApplicationContext` for runtime bean lookup (used by Groovy/Java commands that need to resolve services dynamically).
- `EventBroadcaster` — async pub/sub keyed by `Event` subtype. Subscribers register `Consumer<Event>` and receive callbacks on the `workflow-pool` executor. Used heavily for view-to-view updates and for streaming workflow progress.
- `ApplicationReadyManager` — post-startup hooks (loads JAR libraries from `my-workflows.config.library.base-directory`, scheduled workflow templates, etc.).
- `InternalCacheManager` (in `cache/`) — application-level cache fronting database queries. Cache sizes/orders are configured in `ApplicationConfig.cacheManager()` and one of them (`WORKFLOW_RUN`) reads its capacity from the `SettingProvider`.

### Settings layering

`SettingProvider` is built as a JDK dynamic proxy chaining `DatabaseSettingProvider → DefaultSettingProvider` (see `ApplicationConfig.settingProvider()`). Reads check the DB first; misses fall back to `DefaultSettingProvider` defaults. Settings are surfaced/edited from the UI via `SettingView`.

### UI layer (`view/`)

Each top-level Vaadin route is a single class in `view/`. Forms and grids reuse components from `view/component/`. `view/transformer/` adapts domain objects to UI representations. `KeyboardUtil` centralizes the dev-page shortcuts documented in the README. The Ace editor wrapper (`de.f0rce:ace`) is used in `WorkflowDevelopmentView`.

### REST API (`restapi/`)

Disabled by default — gated by `my-workflows.config.feature.restApiEnabled`. Authenticated via a per-user `token` URL parameter (not a header). `SecurityApiConfig` is the security chain for `/api/**`; `SecurityConfig` covers the Vaadin-served pages.

### Event handlers (`domain/handler/`)

Each entity type (Workflow*, DocPage, Library) has an `*EventHandler` that subscribes to `EventBroadcaster` events and mediates between UI events and services. This is how the views avoid touching repositories directly.

## Conventions worth knowing

- **Lombok is required** — annotation processing must be configured. The `maven-compiler-plugin` declares `annotationProcessorPaths` because JDK 24+ no longer auto-discovers processors from the classpath.
- **All entities are `final` with private fields**, getters generated by Lombok. Constructors generated by `@AllArgsConstructor` / `@RequiredArgsConstructor`. Don't add a no-args constructor unless JPA needs it (then add it manually — Hibernate creates entities reflectively and handles missing default constructors via bytecode).
- **Variables whose name contains `password`** (case-insensitive) are masked in print output by `PrintCommand`.
- **Translations** live in `translation_en.properties`. Add a key, reference it via `TranslationProvider`.
- **Releases** are driven by `mvn release:prepare release:perform` from CI on `main` (`.github/workflows/on-main-push.yml`) — don't bump `<version>` manually.
