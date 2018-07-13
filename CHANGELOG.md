# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [0.2.4-SNAPSHOT] – Pre-Release

## [0.2.3] – 2018-07-13

### Fixed

- Incorrect generation of empty-arg fspecs => test execution errors

- The ghostwheel core, tracing and reporting namespaces are unnecessarily recompiled on every hot-reload with Shadow CLJS

### Added

- Plain `defn` functions can be excluded from coverage checks with `::g/check-coverage false`

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

[0.2.4-SNAPSHOT]: https://github.com/gnl/ghostwheel/compare/v0.2.3...HEAD
[0.2.3]: https://github.com/gnl/ghostwheel/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/gnl/ghostwheel/compare/v0.2.1...v0.2.2
