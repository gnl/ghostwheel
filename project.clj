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
                 [gnl/clairvoyant "0.2.3"]]
  :source-paths ["src"]
  :clean-targets ^{:protect false} ["target" "resources"]
  :profiles {:dev {:dependencies [[org.clojure/clojurescript "1.10.520"]
                                  [org.clojure/clojure "1.10.0"]]}})
