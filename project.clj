(defproject gnl/ghostwheel "0.4.0"
  :description "Hassle-free and concise clojure.spec, automatic generative testing, side effect detection, and evaluation tracing for Clojure(-Script)"
  :url "https://github.com/gnl/ghostwheel"
  :scm {:name "git"
        :url  "https://github.com/gnl/ghostwheel"}
  :license {:name "Eclipse Public License"
            :url  "https://choosealicense.com/licenses/epl-2.0/"}
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :dependencies [[gnl/ghostwheel.unghost "0.4.0"]
                 [org.clojure/spec.alpha "0.2.176"]
                 [org.clojure/test.check "0.10.0-alpha4"]
                 [orchestra "2019.02.06-1"]
                 [expound "0.7.2"]
                 [org.clojars.stumitchell/clairvoyant "0.2.1"]]
  :plugins [[lein-doo "0.1.10"]]
  :source-paths ["src"]
  :clean-targets ^{:protect false} ["target" "resources"]
  :profiles {:dev           {:dependencies [[org.clojure/clojurescript "1.10.520"]
                                            [org.clojure/clojure "1.10.0"]
                                            [com.rpl/specter "1.1.2"]
                                            [binaryage/devtools "0.9.10"]]
                             :jvm-opts     ["-Dghostwheel.cache=false"]}
             :clj-prod-test {:jvm-opts ["-Dghostwheel.enabled=false"]}
             :clj-dev-test  {:jvm-opts ["-Dghostwheel.enabled=true"]}}
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
                        :compiler     {:main            ghostwheel.prod-test-runner
                                       :asset-path      "base/resources/test/prod"
                                       :output-dir      "resources/test/prod"
                                       :output-to       "resources/test/prod.js"
                                       :pretty-print    true
                                       :optimizations   :none
                                       :external-config {:ghostwheel {:enabled false}}}}
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
                        :compiler     {:main            ghostwheel.prod-test-runner
                                       :asset-path      "base/resources/test/prod_node"
                                       :output-dir      "resources/test/prod_node"
                                       :output-to       "resources/test/prod_node.js"
                                       :pretty-print    true
                                       :optimizations   :simple
                                       :target          :nodejs
                                       :external-config {:ghostwheel {:enabled false}}}}]})

