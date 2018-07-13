(defproject gnl/ghostwheel "0.2.4-SNAPSHOT"
  :description "Hassle-free clojure.spec, side effect detection and evaluation tracing for Clojure(-Script)"
  :url "https://github.com/gnl/ghostwheel"
  :license {:name "Eclipse Public License"
            :url  "https://choosealicense.com/licenses/epl-2.0/"}
  :repositories {"clojars" {:url           "https://clojars.org/repo"
                            :sign-releases false}}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.339"]
                 [org.clojure/spec.alpha "0.2.168"]
                 [org.clojure/test.check "0.10.0-alpha3"]
                 [com.rpl/specter "1.1.1"]
                 [orchestra "2017.11.12-1"]
                 [expound "0.7.0"]
                 [gnl/re-frame-tracer "0.1.7"]
                 [org.clojars.stumitchell/clairvoyant "0.2.1"]
                 [funcool/cuerdas "2.0.5"]
                 [lambdaisland/uniontypes "0.3.0"]]
  :plugins [[lein-doo "0.1.10"]]
  :source-paths ["src"]
  :clean-targets ^{:protect false} ["target" "resources"]
  :cljsbuild {:builds [{:id           "dev-test"
                        :source-paths ["src" "test"]
                        :compiler     {:main          ghostwheel.dev-test-runner
                                       :asset-path    "base/resources/test/dev"
                                       :output-dir    "resources/test/dev"
                                       :output-to     "resources/test/dev.js"
                                       :optimizations :none
                                       :ghostwheel    true}}
                       {:id           "prod-test"
                        :source-paths ["src" "test"]
                        :compiler     {:main          ghostwheel.prod-test-runner
                                       :asset-path    "base/resources/test/prod"
                                       :output-dir    "resources/test/prod"
                                       :output-to     "resources/test/prod.js"
                                       :optimizations :none}}
                       {:id           "dev-test-node"
                        :source-paths ["src" "test"]
                        :compiler     {:main          ghostwheel.dev-test-runner
                                       :asset-path    "base/resources/test/dev_node"
                                       :output-dir    "resources/test/dev_node"
                                       :output-to     "resources/test/dev_node.js"
                                       :optimizations :simple
                                       :target        :nodejs
                                       :ghostwheel    true}}
                       {:id           "prod-test-node"
                        :source-paths ["src" "test"]
                        :compiler     {:main          ghostwheel.prod-test-runner
                                       :asset-path    "base/resources/test/prod_node"
                                       :output-dir    "resources/test/prod_node"
                                       :output-to     "resources/test/prod_node.js"
                                       :optimizations :simple
                                       :target        :nodejs}}]})

