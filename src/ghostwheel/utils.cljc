;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^:no-doc ghostwheel.utils
  #?(:cljs (:require-macros ghostwheel.utils))
  (:require [clojure.walk :as walk]
            #?@(:clj  [[clojure.core.specs.alpha]
                       [ghostwheel.stubs.cljs-env :as cljs-env]
                       [orchestra.spec.test :as ost]
                       [clojure.edn :as edn]]
                :cljs [[cljs.core.specs.alpha :include-macros true]
                       [cljs.env :as cljs-env]
                       [orchestra-cljs.spec.test :as ost]])))


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
                    :num-tests       0

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

                    ;; Nilable map of Expound configuration options.
                    ;; If not nil, the spec printer will be set to
                    ;; expound's with the given configuration options.
                    :expound         {:show-valid-values? true
                                      :print-specs?       true}

                    ;; Output channel for tracing and check
                    ;; reports. Only `:repl` and `:js-console`
                    ;; are supported at the moment. The option is
                    ;; ignored on Clojure where only `:repl` is used.
                    :report-output   :js-console})


(defn cljs-env? [env] (boolean (:ns env)))


;; This isn't particularly pretty, but it's how we avoid
;; having ClojureScript as a required dependency on Clojure
#?(:clj (try
          (do
            (ns-unalias (find-ns 'ghostwheel.utils) 'cljs-env)
            (require '[cljs.env :as cljs-env]))
          (catch Exception _ (require '[ghostwheel.stubs.cljs-env :as cljs-env]))))


(let [*config-cache
      (atom {::timestamp 0
             ::value     nil})

      read-config-file
      (fn []
        #?(:clj  (try
                   (edn/read-string (slurp "ghostwheel.edn"))
                   (catch Exception _ nil))
           :cljs nil))

      reload-config
      (fn [env]
        (let [cljs?
              (cljs-env? env)

              ghostwheel-system-property
              (identity #?(:clj  (= (System/getProperty "ghostwheel.enabled") "true")
                           :cljs nil))

              plain-config                            ;; TODO validation
              (if cljs?
                (let [cljs-compiler-config
                      (when cljs-env/*compiler*
                        (or (get-in @cljs-env/*compiler* [:options :external-config :ghostwheel])
                            ;; Deprecated.
                            (get-in @cljs-env/*compiler* [:options :ghostwheel])))]
                  (when (or cljs-compiler-config ghostwheel-system-property)
                    (merge (read-config-file)
                           cljs-compiler-config)))
                (when ghostwheel-system-property
                  (merge (read-config-file)
                         {:report-output :repl})))]
          (when plain-config
            (into {} (map (fn [[k v]] [(keyword "ghostwheel.core" (name k)) v])
                          plain-config)))))

      load-config
      (fn [env]
        (let [now (identity #?(:clj (System/currentTimeMillis) :cljs (js/Date.now)))]
          (if (< (- now (::timestamp @*config-cache))
                 2000)
            (::value @*config-cache)
            (::value (reset! *config-cache
                             {::timestamp now
                              ::value     (reload-config env)})))))]

  (defn get-base-config
    [env]
    (when-let [env-config (load-config env)]
      (merge ghostwheel-default-config env-config))))


(defmacro get-base-config* []
  (get-base-config &env))


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

(defn gen-exception [env msg]
  `(throw (~(if (cljs-env? env) 'js/Error. 'Exception.) ~msg)))

