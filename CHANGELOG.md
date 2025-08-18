# Changelog

All notable changes to this project will be documented in this file.

## 1.1.0 (2025-08-18)

### Bug fixes

* Avoid NullPointerException when no documentation page is available.

### Dependencies

* Upgrade Flyway Core to 11.11.1.

### Features

* New page for settings.
* Avoid to display bigger text prints directly in the UI. Provide the ability to download the content as a file.
* Rely more and more on Java records.
* Remove deprecated API regarding ResourceStream from Vaadin Framework and rely on DownloadHandler.

## 1.0.13 (2025-08-11)

### Dependencies

* Upgrade Vaadin to 24.8.6.

### Features

* Remove deprecated API regarding AntPathRequestMatcher class from Spring Framework.

## 1.0.12 (2025-08-10)

### Dependencies

* Upgrade Flyway Core to 11.11.0.
* Upgrade Markdown Editor to 2.0.0.

## 1.0.11 (2025-08-10)

### Features

* Remove deprecated API regarding UploadHandler from Vaadin 24.8.
* Set default maximum file size for uploaded libraries to 128MB.

## 1.0.10 (2025-08-06)

### Dependencies

* Upgrade Spring Boot to 3.5.4.
* Upgrade Vaadin to 24.8.5.
* Upgrade Flyway Core to 11.10.5.

## 1.0.9 (2025-07-26)

### Bug fixes

* Fix bug with allowedCharsPattern in Vaadin TextField.

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

* Add file for CHANGELOG.

## 1.0.6 (2025-07-19)

### Features

* Implement Ace editor tips functionality to help developers to write new workflow scripts.
* An initial number of 23 tips were added. New tips will be added in upcoming releases, together with new snippets.

## 1.0.5 (2025-07-19)

### Bug fixes

* Fix bug with workflow definition update with is not propagated to workflow template.
* Fix bug with workflow parameter update with is not propagated to workflow template.

### Dependencies

* Upgrade Vaadin to 24.8.3.
* Upgrade Flyway Core to 11.10.2.

### Features

* Introduce Ace editor snippets.
* Activate worker for Ace editor to validate YAML content.
* Exclude scanning of `view` package in Jacoco. As a side effect, test coverage increased from 30% to 53%.
* Incorporate Ace editor inside our framework. Rely only on Jackson library for JSON processing.

## 1.0.4 (2025-07-16)

### Dependencies

* Upgrade Vaadin to 24.8.3.
* Upgrade Flyway Core to 11.10.2.

### Features

* Show invisible chars in Ace editor by default.

## 1.0.3 (2025-07-05)

### Dependencies

* Upgrade Flyway Core to 11.10.1.
* Upgrade JSON Schema Validator to 1.5.8.
* Upgrade JUnit to 5.13.3.

### Features

* Introduce ability to download libraries.
* Rename `email` command to `mail` command + add optional parameters for CC and BBC.

## 1.0.2 (2025-07-05)

### Bug fixes

* Fix bug with workflow template creation due to immutable collections.

## 1.0.1 (2025-07-01)

### Dependencies

* Upgrade Vaadin to 24.8.2.

### Documentation

* Introduce section about workflow debugging.

### Tests

* Implement unitary test for `CookieUtil` class.

## 1.0.0 (2025-06-24)

This is the initial version.
Vaadin 24.8.0 is used.
