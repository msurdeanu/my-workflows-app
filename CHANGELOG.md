# Changelog

All notable changes to this project will be documented in this file.

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
