#!/bin/bash

echo -e "\n### Testing Clojure build..."
lein test ghostwheel.dev-test || exit 1

echo -e "\n### Watching ClojureScript build (chrome-headless, dev environment)..."
lein doo chrome-headless dev-test auto || exit 1

