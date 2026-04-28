# Changelog

All notable changes to this project will be documented in this file.

## 1.3.0 (2026-04-28)

### Breaking changes

* Require Java 25 as the minimum runtime.
* Migrate to Spring Boot 4 and Vaadin 25.

### Features

* Switch theme to Lumo with a custom `styles.css` served from `META-INF/resources`, replacing the legacy `simple` theme bundled under `frontend/`.
* Use Java 25 instance `main` method in `Application` and unnamed lambda parameters (`_`) where the parameter is unused.

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

* Offer a mechanism for injecting specific parameters into workflow templates, increasing their customizability.

## 1.2.3 (2025-09-22)

### Dependencies

* Upgrade Vaadin to 24.9.0.
* Upgrade Lombok to 1.18.40.
* Upgrade JSON Schema Validator to 1.5.9.

### Features

* Remove deprecated functionality around `VaadinWebSecurity` class.

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

* Use more and more `LangUtil` class functionality.

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

* Avoid NullPointerException when no documentation page is available.

### Dependencies

* Upgrade Flyway Core to 11.11.1.

### Features

* New page for settings.
* Avoid displaying bigger text prints directly in the UI. Provide the ability to download the content as a file.
* Rely more and more on Java records.
* Remove the deprecated API regarding ResourceStream from Vaadin Framework and rely on DownloadHandler.

## 1.0.13 (2025-08-11)

### Dependencies

* Upgrade Vaadin to 24.8.6.

### Features

* Remove the deprecated API regarding AntPathRequestMatcher class from Spring Framework.

## 1.0.12 (2025-08-10)

### Dependencies

* Upgrade Flyway Core to 11.11.0.
* Upgrade Markdown Editor to 2.0.0.

## 1.0.11 (2025-08-10)

### Features

* Remove the deprecated API regarding UploadHandler from Vaadin 24.8.
* Set the default maximum file size for uploaded libraries to 128MB.

## 1.0.10 (2025-08-06)

### Dependencies

* Upgrade Spring Boot to 3.5.4.
* Upgrade Vaadin to 24.8.5.
* Upgrade Flyway Core to 11.10.5.

## 1.0.9 (2025-07-26)

### Bug fixes

* Fix the bug with allowedCharsPattern in Vaadin TextField.

### Features

* Activate Dependabot checks every week.

## 1.0.8 (2025-07-26)

### Dependencies

* Upgrade Vaadin to 24.8.4.
* Upgrade Flyway Core to 11.10.4.
* Upgrade Groovy to 4.0.28.
* Upgrade JUnit to 5.13.4.

## 1.0.7 (2025-07-19)

### Features

* Add a file for CHANGELOG.

## 1.0.6 (2025-07-19)

### Features

* Implement Ace editor tips functionality to help developers to write new workflow scripts.
* An initial number of 23 tips were added. New tips will be added in upcoming releases, together with new snippets.

## 1.0.5 (2025-07-19)

### Bug fixes

* Fix a bug with a workflow definition update with is not propagated to the workflow template.
* Fix a bug with a workflow parameter update with is not propagated to the workflow template.

### Dependencies

* Upgrade Vaadin to 24.8.3.
* Upgrade Flyway Core to 11.10.2.

### Features

* Introduce Ace editor snippets.
* Activate worker for Ace editor to validate YAML content.
* Exclude scanning of `view` package in Jacoco. As a side effect, test coverage increased from 30% to 53%.
* Incorporate Ace editor inside our framework. Rely only on the Jackson library for JSON processing.

## 1.0.4 (2025-07-16)

### Dependencies

* Upgrade Vaadin to 24.8.3.
* Upgrade Flyway Core to 11.10.2.

### Features

* Show invisible chars in the Ace editor by default.

## 1.0.3 (2025-07-05)

### Dependencies

* Upgrade Flyway Core to 11.10.1.
* Upgrade JSON Schema Validator to 1.5.8.
* Upgrade JUnit to 5.13.3.

### Features

* Introduce the ability to download libraries.
* Rename `email` command to `mail` command + add optional parameters for CC and BBC.

## 1.0.2 (2025-07-05)

### Bug fixes

* Fix a bug with workflow template creation due to immutable collections.

## 1.0.1 (2025-07-01)

### Dependencies

* Upgrade Vaadin to 24.8.2.

### Documentation

* Introduce a section about workflow debugging.

### Tests

* Implement unitary test for `CookieUtil` class.

## 1.0.0 (2025-06-24)

This is the initial version.
Vaadin 24.8.0 is used.
