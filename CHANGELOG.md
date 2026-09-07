# Changelog

All notable changes to this project will be documented in this file.

## 1.3.1 (2026-09-07)

### Features

* Switch the UI theme from Lumo to Aura, replacing every `LUMO_*` component variant with its Aura equivalent and reworking `styles.css` around the `--aura-*` custom properties.
* Render the Markdown editor and viewer on the documentation page with a transparent background, so they blend into the surrounding theme instead of showing a white box.
* Replace the default upload control on the Libraries page with a borderless icon button carrying a tooltip.
* Continue the migration to unnamed lambda parameters (`_`) wherever the parameter is unused.

### Bug fixes

* Detach the `database` command result into a `CachedRowSet` — the original result set was closed by its try-with-resources block before asserts and outputs could read it.
* Run every workflow on a private copy of the definition script: placeholder resolution mutates the expressions in place, and the script instance is shared across runs through the internal cache.
* Always attach the workflow run to `WorkflowDefinitionOnSubmittedEvent`, so subscribers can correlate an outcome — a validation failure included — with the run they submitted.
* Clear the Spring `SecurityContext` once a REST API request has been handled, instead of leaving the authentication behind on the request thread.
* Reject uploaded library file names that resolve outside the configured base directory, and accept only `.jar` files.
* Create the parent directory before writing a file source or an uploaded library, so the very first write no longer fails when the directory does not exist yet.
* Release the native memory held by `Deflater` and `Inflater` through `end()`, and fail fast on truncated compressed input instead of looping forever.
* Take a copy of the consumer list before broadcasting, and register consumers under the lock, in `EventBroadcaster`.
* Fix `InternalCache`: return a defensive copy of the cached values, track keys with `containsKey` so a key mapped to a null value is not recorded twice, and retry an optimistic read under a real read lock when it observes a torn state.
* Ignore a single malformed setting row instead of aborting startup, so the caller transparently falls back to the default provider.
* Keep the editor tip scheduler alive: clamp the frequency to at least one second and contain exceptions, which `scheduleAtFixedRate` would otherwise treat as a permanent cancellation.
* Skip placeholders without a value, and preserve insertion order while allowing null results when evaluating a map expression — `Collectors.toMap` rejects both.
* Treat a blank cron like a missing one, and create the scheduled task while holding the lock so it cannot start before it can be canceled.
* Load JAR libraries defensively: tolerate a missing or unreadable base directory, and report a single broken JAR as not loaded instead of aborting startup.
* Decode decrypted values with the explicit charset in `EncryptionHolder`.
* Drop empty entries when converting an empty column into a `Set<String>`.
* Skip the backoff after the last attempt of `waitUntilSubPasses`.
* Compare setting values with `Objects.equals`, so a null value no longer throws.
* Tolerate a missing `@ExecutionParam` annotation and a null resolved parameter when debug mode records parameter types.
* Remove a duplicated `toArray` call when rendering workflow validation messages.

### Dependencies

* Upgrade Spring Boot Starter Parent to 4.1.1.
* Upgrade Vaadin to 25.2.7.
* Upgrade Groovy to 5.1.1.
* Upgrade Lombok to 1.18.48, declare it with `provided` scope and exclude it from the repackaged Spring Boot artifact.
* Upgrade Ace Editor to 5.0.1.
* Upgrade Checkstyle to 14.1.0.
* Upgrade JaCoCo to 0.8.15.
* Exclude `vaadin-ai-components-flow` from the Vaadin starter and from the production profile.
* Exclude `spring-aspects` from `spring-boot-starter-data-jpa`.
* Let the Spring Boot and Vaadin BOMs manage `commons-lang3` and `junit-jupiter` instead of pinning their versions.

### Documentation

* Add `CLAUDE.md`, describing the build, the architecture and the conventions of the project.
* Correct the English throughout `README.md` and `CHANGELOG.md`.
* Fix the SSH command documentation: the `sshShell` inputs are prefixed with `sshShell.`, not `sshExec.`, and the `password` input of both SSH commands represents the password, not the host.

### Tests

* Add tests for `DatabaseCommand`, `LibraryService` and `WorkflowScriptService`.
* Extend the `EventBroadcaster`, `InternalCache`, `SetOfStringToStringConverter`, `EncryptionHolder`, `UserTokenFilter` and `ByteArrayCompressUtil` tests to cover the fixes above.

## 1.3.0 (2026-04-28)

### Breaking changes

* Require Java 25 as the minimum runtime.
* Migrate to Spring Boot 4 and Vaadin 25.

### Features

* Switch theme to Lumo with a custom `styles.css` served from `META-INF/resources`, replacing the legacy `simple` theme bundled under `frontend/`.
* Use the Java 25 instance `main` method in `Application` and unnamed lambda parameters (`_`) where the parameter is unused.

### Bug fixes

* Replace `@Convert` on `WorkflowRun.id` with `@JdbcTypeCode(SqlTypes.BINARY)` — Hibernate 7 no longer allows `AttributeConverter` on `@Id` fields.
* Replace deprecated `RestTemplateBuilder` timeout API in `HttpRequestCommand` with `SimpleClientHttpRequestFactory`.
* Declare `vaadin-dev` as an optional dependency so dev mode starts under Vaadin 25.1 (no longer pulled transitively by `vaadin-spring-boot-starter`).
* Add `spring-boot-starter-flyway` so Flyway autoconfiguration is picked up under Spring Boot 4 (autoconfigs were extracted from `spring-boot-autoconfigure`).
* Configure Lombok via `annotationProcessorPaths` in `maven-compiler-plugin` — JDK 24+ no longer auto-discovers annotation processors from the classpath.

### Dependencies

* Upgrade Spring Boot Starter Parent to 4.0.6.
* Upgrade Vaadin to 25.1.3.
* Upgrade Lombok to 1.18.46.
* Upgrade Groovy to 5.0.5.
* Upgrade Mockito JUnit Jupiter to 5.23.0.
* Upgrade Ace Editor to 5.0.0.
* Upgrade Checkstyle to 13.4.1.
* Introduce explicit `commons-lang3`, `jackson-databind`, `jackson-datatype-jsr310` and `spring-boot-starter-web` dependencies.

## 1.2.9 (2026-01-24)

### Dependencies

* Upgrade Vaadin to 24.9.9.
* Upgrade Flyway Core to 11.20.2.
* Upgrade Groovy to 5.0.4.
* Upgrade JUnit to 6.0.2.
* Upgrade Mockito JUnit Jupiter to 5.21.0.
* Upgrade Spring Boot Starter Parent to 3.5.10.

## 1.2.8 (2025-12-12)

### Bug fixes

* Fix snippets in the code editor.

### Dependencies

* Upgrade Vaadin to 24.9.7.
* Introduce Ace Editor dependency.

## 1.2.7 (2025-12-04)

### Dependencies

* Upgrade Vaadin to 24.9.6.
* Upgrade Flyway Core to 11.18.0.
* Upgrade Markdown Editor to 2.0.3.
* Upgrade Groovy to 5.0.2.
* Upgrade Maverick Synergy Client to 3.1.4.
* Upgrade Spring Boot Starter Parent to 3.5.8.

## 1.2.6 (2025-11-02)

### Dependencies

* Upgrade Vaadin to 24.9.4.
* Upgrade Spring Boot Starter Parent to 3.5.7.

## 1.2.5 (2025-10-12)

### Dependencies

* Upgrade Vaadin to 24.9.2.
* Upgrade Flyway Core to 11.14.0.
* Upgrade Angus Email to 2.0.5.
* Upgrade JUnit to 6.0.0.

## 1.2.4 (2025-09-25)

### Features

* Offer a mechanism for injecting specific parameters into workflow templates, making them more customizable.

## 1.2.3 (2025-09-22)

### Dependencies

* Upgrade Vaadin to 24.9.0.
* Upgrade Lombok to 1.18.40.
* Upgrade JSON Schema Validator to 1.5.9.

### Features

* Remove deprecated functionality around the `VaadinWebSecurity` class.

## 1.2.2 (2025-09-15)

### Dependencies

* Upgrade Vaadin to 24.8.8.
* Upgrade Lombok to 1.18.40.
* Upgrade Groovy to 5.0.1.
* Upgrade Angus Email to 2.0.4.

## 1.2.1 (2025-09-07)

### Dependencies

* Upgrade Flyway Core to 11.12.0.

### Features

* Make broader use of the `LangUtil` class functionality.

## 1.2.0 (2025-09-01)

### Features

* Provide a way to replay failing workflows by discarding all successful commands from the last run.

## 1.1.1 (2025-09-01)

### Dependencies

* Upgrade Spring Boot to 3.5.5.
* Upgrade Vaadin to 24.8.7.
* Upgrade Markdown Editor to 2.0.1.
* Upgrade Groovy to 5.0.0.
* Upgrade Flyway Core to 11.11.2.
* Upgrade Mockito JUnit Jupiter to 5.19.0.

## 1.1.0 (2025-08-18)

### Bug fixes

* Avoid a `NullPointerException` when no documentation page is available.

### Dependencies

* Upgrade Flyway Core to 11.11.1.

### Features

* Add a new page for settings.
* Avoid displaying large text prints directly in the UI; provide the ability to download the content as a file instead.
* Rely more heavily on Java records.
* Remove the deprecated `ResourceStream` API from the Vaadin framework and rely on `DownloadHandler` instead.

## 1.0.13 (2025-08-11)

### Dependencies

* Upgrade Vaadin to 24.8.6.

### Features

* Remove the deprecated `AntPathRequestMatcher` class API from the Spring framework.

## 1.0.12 (2025-08-10)

### Dependencies

* Upgrade Flyway Core to 11.11.0.
* Upgrade Markdown Editor to 2.0.0.

## 1.0.11 (2025-08-10)

### Features

* Remove the deprecated `UploadHandler` API from Vaadin 24.8.
* Set the default maximum file size for uploaded libraries to 128 MB.

## 1.0.10 (2025-08-06)

### Dependencies

* Upgrade Spring Boot to 3.5.4.
* Upgrade Vaadin to 24.8.5.
* Upgrade Flyway Core to 11.10.5.

## 1.0.9 (2025-07-26)

### Bug fixes

* Fix the bug with `allowedCharsPattern` in the Vaadin `TextField`.

### Features

* Activate weekly Dependabot checks.

## 1.0.8 (2025-07-26)

### Dependencies

* Upgrade Vaadin to 24.8.4.
* Upgrade Flyway Core to 11.10.4.
* Upgrade Groovy to 4.0.28.
* Upgrade JUnit to 5.13.4.

## 1.0.7 (2025-07-19)

### Features

* Add a CHANGELOG file.

## 1.0.6 (2025-07-19)

### Features

* Implement the Ace editor tips functionality to help developers write new workflow scripts.
* An initial set of 23 tips was added. More tips will be added in upcoming releases, together with new snippets.

## 1.0.5 (2025-07-19)

### Bug fixes

* Fix a bug where a workflow definition update was not propagated to the workflow template.
* Fix a bug where a workflow parameter update was not propagated to the workflow template.

### Dependencies

* Upgrade Vaadin to 24.8.3.
* Upgrade Flyway Core to 11.10.2.

### Features

* Introduce Ace editor snippets.
* Activate the Ace editor worker to validate YAML content.
* Exclude the `view` package from JaCoCo scanning. As a side effect, test coverage increased from 30% to 53%.
* Incorporate the Ace editor into our framework. Rely only on the Jackson library for JSON processing.

## 1.0.4 (2025-07-16)

### Dependencies

* Upgrade Vaadin to 24.8.3.
* Upgrade Flyway Core to 11.10.2.

### Features

* Show invisible characters in the Ace editor by default.

## 1.0.3 (2025-07-05)

### Dependencies

* Upgrade Flyway Core to 11.10.1.
* Upgrade JSON Schema Validator to 1.5.8.
* Upgrade JUnit to 5.13.3.

### Features

* Introduce the ability to download libraries.
* Rename the `email` command to `mail` and add optional parameters for CC and BCC.

## 1.0.2 (2025-07-05)

### Bug fixes

* Fix a bug in workflow template creation caused by immutable collections.

## 1.0.1 (2025-07-01)

### Dependencies

* Upgrade Vaadin to 24.8.2.

### Documentation

* Introduce a section about workflow debugging.

### Tests

* Implement a unit test for the `CookieUtil` class.

## 1.0.0 (2025-06-24)

This is the initial version.
It uses Vaadin 24.8.0.
