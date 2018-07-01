# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased] – 0.2.3-SNAPSHOT

### Changed

- Allow instrumentation with nil gspecs – trust that there's an external fspec/fdef and simply fail if there isn't.

- `::outstrument` overrides `::instrument` when they are used together. Don't use them together. 

## [0.2.2]

### Fixed

- Namespace metadata ignored when using Figwheel or cljs.main

- Instrumentation requires `::g/check` option – #1

### Changed

- Global/compiler configuration is now set via the `:external-config` compiler option. The old behaviour is deprecated.

- Ghostwheel is now enabled via `:ghostwheel {}` instead of `:ghostwheel true`. The latter is deprecated.

- Requiring `ghostwheel.core` in CLJS is now simpler and works the same way as in Clojure – #2

## 0.2.1 – 2018-06-28 – Initial release

[Unreleased]: https://github.com/gnl/ghostwheel/compare/v0.2.2...HEAD
[0.2.2]: https://github.com/gnl/ghostwheel/compare/v0.2.1...v0.2.2