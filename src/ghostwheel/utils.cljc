;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.utils
  #?(:cljs (:require-macros ghostwheel.utils))
  (:require [cljs.env]
            [clojure.walk :as walk]
            [clojure.pprint :as pprint]
            [cuerdas.core :as cs]
            #?@(:clj  [[clojure.core.specs.alpha]
                       [orchestra.spec.test :as ost]
                       [clojure.edn :as edn]
                       [com.rpl.specter
                        :refer [setval transform select select-any
                                filterer nthpath ALL MAP-VALS MAP-KEYS NAMESPACE]]]
                :cljs [[cljs.core.specs.alpha :include-macros true]
                       [orchestra-cljs.spec.test :as ost]
                       [com.rpl.specter
                        :refer-macros [setval transform select select-any]
                        :refer [filterer nthpath ALL MAP-VALS MAP-KEYS NAMESPACE]]])))


(def ghostwheel-default-config
  #:ghostwheel.core{;; Evaluation trace verbosity level. 0 disables all tracing code generation.
                    :trace           0

                    ;; #RRGGBB, #RGB, or keyword from the `ghostwheel-colors` map.
                    :trace-color     :violet

                    ;; When disabled no checks of any kind are
                    ;; performed and no test code is generated.
                    :check           false

                    ;; Determines whether Ghostwheel should warn on missing fspecs
                    ;; and plain (non-Ghostwheel) `defn` usage. When enabled on a
                    ;; namespace or higher level, you can exclude individual `defn`s or
                    ;; `declare`s by setting it to false in their respective metadata
                    :check-coverage  false

                    ;; Disable side effect detection
                    :ignore-fx       false

                    ;; Number of generative tests performed when quick-checking (on hot-reload)
                    :num-tests-quick 0

                    ;; Number of generative tests performed when checking extensively (test suite)
                    :num-tests-ext   100

                    ;; Determines which of the above two options should take
                    ;; precedence. Set to true in your test build configuration.
                    :extensive-tests false

                    ;; Ghostwheel generates standard `defn` function definitions
                    ;; by default. If you require composability with other
                    ;; `defn`-like macros, you can have Ghostwheel desugar to
                    ;; them instead by setting the macro name as a string here.
                    :defn-macro      nil

                    ;; Spec-instrument functions on namespace reload.
                    :instrument      false

                    ;; Spec-instrument functions on namespace reload using
                    ;; orchestra, which spec-checks the output in addition to
                    ;; the input. Use either this or `::instrument`, not both.
                    :outstrument     false

                    ;; The following options can only be set in
                    ;; the global Ghostwheel configuration layer

                    ;; Nilable vector of qualified external namespaces
                    ;; or functions (unquoted) to spec-instrument before
                    ;; and unstrument after testing to catch incorrect
                    ;; function calls at test time without the runtime
                    ;; performance impact. Fspecs must be defined for
                    ;; the relevant functions in a `require`d namespace
                    ;; using either `s/fdef` or Ghostwheel's `>fdef`.
                    :extrument       nil

                    ;; Output channel for tracing and check
                    ;; reports. Only `:repl` and `:js-console`
                    ;; are supported at the moment. The option is
                    ;; ignored on Clojure where only `:repl` is used.
                    :report-output   :js-console})



(defn cljs-env? [env] (boolean (:ns env)))


(defn get-ghostwheel-compiler-config [env]
  (let [compiler-config
        (if (cljs-env? env)
          (when cljs.env/*compiler*
            (or (get-in @cljs.env/*compiler* [:options :external-config :ghostwheel])
                (get-in @cljs.env/*compiler* [:options :ghostwheel])))
          ;; TODO error checking
          #?(:clj (merge (edn/read-string (slurp "ghostwheel.edn"))
                         {:report-output :repl})))]
    (cond
      (map? compiler-config) (setval [MAP-KEYS NAMESPACE] (str `ghostwheel.core) compiler-config)
      (true? compiler-config) {}
      :else nil)))


(defn get-base-config [env]
  (merge ghostwheel-default-config (get-ghostwheel-compiler-config env)))


(defmacro get-env-config []
  (merge ghostwheel-default-config (get-ghostwheel-compiler-config &env)))


(defn get-ns-meta [env]
  (if (cljs-env? env)
    ;; This isn't necessary, strictly speaking, but it makes hacking on
    ;; Ghostwheel easier, because it allows env to be stubbed in order to
    ;; trace-debug code generating functions at runtime in ClojureScript
    (or (meta *ns*) (some-> env :ns :meta))
    (meta *ns*)))

(defn get-ns-name [env]
  (if (cljs-env? env)
    (or (.-name *ns*) (some-> env :ns :name))
    (.-name *ns*)))

(defn clj->cljs
  ([form]
   (clj->cljs form true))
  ([form strip-core-ns]
   (let [ns-replacements   (cond-> {"clojure.core"            "cljs.core"
                                    "clojure.test"            "cljs.test"
                                    "clojure.spec.alpha"      "cljs.spec.alpha"
                                    "clojure.spec.test.alpha" "cljs.spec.test.alpha"
                                    "orchestra.spec.test"     "orchestra-cljs.spec.test"
                                    "clojure.spec.gen.alpha"  "cljs.spec.gen.alpha"}
                                   strip-core-ns (merge {"clojure.core" nil
                                                         "cljs.core"    nil}))
         replace-namespace #(if-not (qualified-symbol? %)
                              %
                              (let [nspace (namespace %)]
                                (if (contains? ns-replacements nspace)
                                  (symbol (get ns-replacements nspace) (name %))
                                  %)))]
     (walk/postwalk replace-namespace form))))

;; Copy-pasted from Rosetta Code
(defn wrap-line [size text]
  (pprint/cl-format nil
                    (str "~{~<~%~1," size ":;~A~> ~}")
                    (cs/split text #" ")))

(defn gen-exception [env msg]
  `(throw (~(if (cljs-env? env) 'js/Error. 'Exception.) ~msg)))

