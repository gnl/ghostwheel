(defproject gnl/ghostwheel "0.3.5-SNAPSHOT"
  :description "Hassle-free and concise clojure.spec, automatic generative testing, side effect detection, and evaluation tracing for Clojure(-Script)"
  :url "https://github.com/gnl/ghostwheel"
  :scm {:name "git"
        :url  "https://github.com/gnl/ghostwheel"}
  :license {:name "Eclipse Public License"
            :url  "https://choosealicense.com/licenses/epl-2.0/"}
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :dependencies [[gnl/ghostwheel.logging "0.3.1"]
                 [gnl/ghostwheel.unghost "0.3.0"]
                 [org.clojure/spec.alpha "0.2.176"]
                 [org.clojure/test.check "0.10.0-alpha3"]
                 [orchestra "2018.08.19-1"]
                 [expound "0.7.1"]]
  :plugins [[lein-doo "0.1.10"]]
  :source-paths ["src"]
  :clean-targets ^{:protect false} ["target" "resources"]
  :profiles {:dev          {:dependencies [[org.clojure/clojurescript "1.10.439"]
                                           [org.clojure/clojure "1.9.0"]
                                           [com.rpl/specter "1.1.2"]
                                           [gnl/ghostwheel.tracer "0.3.1"]]
                            :jvm-opts     ["-Dghostwheel.cache=false"]}
             :clj-dev-test {:jvm-opts ["-Dghostwheel.enabled=true"]}}
  :cljsbuild {:builds [{:id           "dev-test"
                        :source-paths ["src" "test"]
                        :compiler     {:main            ghostwheel.dev-test-runner
                                       :asset-path      "base/resources/test/dev"
                                       :output-dir      "resources/test/dev"
                                       :output-to       "resources/test/dev.js"
                                       :pretty-print    true
                                       :optimizations   :none
                                       :external-config {:ghostwheel {}}}}
                       {:id           "prod-test"
                        :source-paths ["src" "test"]
                        :compiler     {:main          ghostwheel.prod-test-runner
                                       :asset-path    "base/resources/test/prod"
                                       :output-dir    "resources/test/prod"
                                       :output-to     "resources/test/prod.js"
                                       :pretty-print  true
                                       :optimizations :none}}
                       {:id           "dev-test-node"
                        :source-paths ["src" "test"]
                        :compiler     {:main            ghostwheel.dev-test-runner
                                       :asset-path      "base/resources/test/dev_node"
                                       :output-dir      "resources/test/dev_node"
                                       :output-to       "resources/test/dev_node.js"
                                       :pretty-print    true
                                       :optimizations   :simple
                                       :target          :nodejs
                                       :external-config {:ghostwheel {}}}}
                       {:id           "prod-test-node"
                        :source-paths ["src" "test"]
                        :compiler     {:main          ghostwheel.prod-test-runner
                                       :asset-path    "base/resources/test/prod_node"
                                       :output-dir    "resources/test/prod_node"
                                       :output-to     "resources/test/prod_node.js"
                                       :pretty-print  true
                                       :optimizations :simple
                                       :target        :nodejs}}]})

