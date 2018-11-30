#!/bin/bash

set -e

echo -e "\n### Testing Clojure build (dev environment)..."
lein with-profile +clj-dev-test test ghostwheel.dev-test

echo -e "\n### Testing Clojure build (prod environment)..."
lein test ghostwheel.prod-test

echo -e "\n### Testing ClojureScript build (chrome-headless, dev environment)..."
lein doo chrome-headless dev-test once

echo -e "\n### Testing ClojureScript build (chrome-headless, prod environment)..."
lein doo chrome-headless prod-test once

echo -e "\n### Testing ClojureScript build (node, dev environment)..."
lein doo node dev-test-node once

echo -e "\n### Testing ClojureScript build (node, prod environment)..."
lein doo node prod-test-node once

echo -e "\n### SUCCESS\n\nAll tests passed in all environments."
