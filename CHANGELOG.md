# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [0.3.0] 

### Fixed

- `Use of undeclared Var com.rpl.specter/java` warning – #15
  - Specter is no longer required at runtime as its minimal use wasn't worth the additional dependency (it's still used in testing though). 

### Removed

- Dropped many runtime dependencies – specter, cuerdas, uniontypes, clairvoyant.core, re-frame-tracer, and when running on Clojure – ClojureScript.
  - Specter and cuerdas were grudgingly replaced with core functions, clairvoyant.core and the tracer are now required by/part of the separate `ghostwheel.tracer` module, and uniontypes was meant for a functionality that isn't implemented yet.

### Changed

- The core of the tracing functionality has been extracted into the `ghostwheel.tracer` artifact, the namespace in which must be additionally required once in the project for tracing to work.

- The specs package with gspecs for `clojure.core` and soon hopefully third-party libraries has been extracted into a separate `ghostwheel.specs` artifact/repo.

- `::g/extrument` is now a global-only option and cannot be set on an individual namespace level.

- The releases are now being signed.

- The cljdoc documentation should be much cleaner and more usable now as a number of internal namespaces have been excluded from the doc generation.

### Added

- Full Clojure support including Ghostwheel checking and reporting in the REPL, as well as a `ghostwheel.edn` configuration file and a `-Dghostwheel.enabled=true` JVM system property to explicitly enable it.

- Related to the above – support for checking, reporting and tracing in plain-text ClojureScript REPLs, as well as `ghostwheel.edn` and `-Dghostwheel.enabled=true` 

- `::g/report-output` configuration option to determine reporting/tracing output with sensible defaults for Clojure/ClojureScript (`:repl`/`:js-console` respectively).

- Rewrite of `(g/check)` to support checking (in the REPL or a test-runner) of single or multiple functions and namespaces, including regex-based multi-selection support for the latter.

- Improved and much more detailed coverage warnings for plain `defn` usage, disabled Ghostwheel checking, etc.

- Composability with other `defn`-like macros with the `::g/defn-macro` option to make Ghostwheel desugar to something other than `defn`/`defn-`.

- Support for easy spec-instrumentation with metadata when writing external specs with `>fdef` with identical behaviour to `>defn` – `^::g/outstrument`/`^::g/instrument`

- `::g/expound` option to configure the Expound spec error pretty-printer. On by default.


## [0.2.4-SNAPSHOT] – Pre-Release

### Fixed

- Remove unnecessary calls to `s/unstrument` on namespace reload.

## [0.2.3] – 2018-07-13

### Fixed

- Incorrect generation of empty-arg fspecs => test execution errors

- The ghostwheel core, tracing and reporting namespaces are unnecessarily recompiled on every hot-reload with Shadow CLJS

### Added

- Plain `defn` as well as `declare`d functions can be excluded from coverage checks with the `::g/check-coverage false` metadata

- Allow instrumentation with nil gspecs – trust that there's an external fspec/fdef and simply fail if there isn't.

### Changed

- Updated clojure.spec and test.check

- `::outstrument` overrides `::instrument` when they are used together. Don't use them together. 

## [0.2.2] – 2018-06-30

### Fixed

- Namespace metadata ignored when using Figwheel or cljs.main

- Instrumentation requires `::g/check` option – #1

### Changed

- Global/compiler configuration is now set via the `:external-config` compiler option. The old behaviour is deprecated.

- Ghostwheel is now enabled via `:ghostwheel {}` instead of `:ghostwheel true`. The latter is deprecated.

- Requiring `ghostwheel.core` in CLJS is now simpler and works the same way as in Clojure – #2

## 0.2.1 – 2018-06-28 – Initial release

[0.3.0-SNAPSHOT]: https://github.com/gnl/ghostwheel/compare/v0.2.3...HEAD
[0.2.4-SNAPSHOT]: https://github.com/gnl/ghostwheel/compare/v0.2.3...HEAD
[0.2.3]: https://github.com/gnl/ghostwheel/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/gnl/ghostwheel/compare/v0.2.1...v0.2.2
