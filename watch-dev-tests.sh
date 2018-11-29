#!/bin/bash

set -e

echo -e "\n### Testing Clojure build..."
lein with-profile +clj-dev-test test ghostwheel.dev-test

echo -e "\n### Watching ClojureScript build (chrome-headless, dev environment)..."
lein doo chrome-headless dev-test auto

