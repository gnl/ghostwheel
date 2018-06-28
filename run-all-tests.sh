#!/bin/bash

echo -e "\n### Testing Clojure build..."
lein test ghostwheel.dev-test || exit 1

echo -e "\n### Testing ClojureScript build (chrome-headless, dev environment)..."
lein doo chrome-headless dev-test once || exit 1

echo -e "\n### Testing ClojureScript build (chrome-headless, prod environment)..."
lein doo chrome-headless prod-test once || exit 1

echo -e "\n### Testing ClojureScript build (node, dev environment)..."
lein doo node dev-test-node once || exit 1

echo -e "\n### Testing ClojureScript build (node, prod environment)..."
lein doo node prod-test-node once || exit 1

echo -e "\n### SUCCESS\n\nAll tests passed in all environments."
