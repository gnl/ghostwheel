# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## 0.2.2-SNAPSHOT – [Unreleased]

### Fixed

- Instrumentation requires `::g/check` option – #1

### Changed

- Global/compiler configuration is now set via the `:external-config` compiler option. The old behaviour is deprecated.

- Ghostwheel is now enabled via `:ghostwheel {}` instead of `:ghostwheel true`. The latter is deprecated

- Requiring `ghostwheel.core` in CLJS is now simpler and works the same way as in Clojure – #2

## 0.2.1 – 2018-06-28 – Initial release

[Unreleased]: https://github.com/gnl/ghostwheel/compare/0.2.1...HEAD
