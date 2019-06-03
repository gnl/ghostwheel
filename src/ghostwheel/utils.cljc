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
            #?@(:clj  [[orchestra.spec.test :as ost]
                       [clojure.edn :as edn]]
                :cljs [[cljs.env :as cljs-env]
                       [orchestra-cljs.spec.test :as ost]])))


;; This isn't particularly pretty, but it's how we avoid
;; having ClojureScript as a required dependency on Clojure
#?(:clj (try
          (do
            (ns-unalias (find-ns 'ghostwheel.utils) 'cljs-env)
            (require '[cljs.env :as cljs-env]))
          (catch Exception _ (require '[ghostwheel.stubs.cljs-env :as cljs-env]))))


(def ghostwheel-default-config
  #:ghostwheel.core{;; Evaluation trace verbosity level. 0 disables all tracing code generation.
                    :trace             0

                    ;; #RRGGBB, #RGB, or keyword from the `ghostwheel-colors` map.
                    :trace-color       :violet

                    ;; When disabled no checks of any kind are
                    ;; performed and no test code is generated.
                    :check             true

                    ;; Determines whether Ghostwheel should warn on missing fspecs
                    ;; and plain (non-Ghostwheel) `defn` usage. When enabled on a
                    ;; namespace or higher level, you can exclude individual `defn`s or
                    ;; `declare`s by setting it to false in their respective metadata
                    :check-coverage    false

                    ;; Enable side effect detection checks
                    :check-fx          true

                    ;; Number of generative tests performed when quick-checking (hot-reload/repl)
                    :gen-tests         0

                    ;; p.ex. {:default 10 :extensive 1000}
                    :gen-test-profiles nil

                    ;; Ghostwheel generates standard `defn` function definitions
                    ;; by default. If you require composability with other
                    ;; `defn`-like macros, you can have Ghostwheel desugar to
                    ;; them instead by setting the macro name as a string here.
                    :defn-macro        nil

                    ;; Spec-instrument functions on namespace reload.
                    :instrument        false

                    ;; Spec-instrument functions on namespace reload using
                    ;; orchestra, which spec-checks the output in addition to
                    ;; the input. Use either this or `::instrument`, not both.
                    :outstrument       false

                    ;; The following options can only be set in
                    ;; the global Ghostwheel configuration layer

                    ;; Nilable vector of qualified external namespaces
                    ;; or functions (unquoted) to spec-instrument before
                    ;; and unstrument after testing to catch incorrect
                    ;; function calls at test time without the runtime
                    ;; performance impact. Fspecs must be defined for
                    ;; the relevant functions in a `require`d namespace
                    ;; using either `s/fdef` or Ghostwheel's `>fdef`.
                    :extrument         nil

                    ;; Nilable map of Expound configuration options.
                    ;; If not nil, the spec printer will be set to
                    ;; expound's with the given configuration options.
                    :expound           {:show-valid-values? true
                                        :print-specs?       true}

                    ;; Output channel for tracing and check
                    ;; reports. Only `:repl` and `:js-console`
                    ;; are supported at the moment. The option is
                    ;; ignored on Clojure where only `:repl` is used.
                    :report-output     :js-console})


(defn cljs-env? [env] (boolean (:ns env)))


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
      (fn []
        ;#?(:clj (.println System/err "Reloaded ghostwheel config.")) ; DEBUG
        (let [plain-config                            ;; TODO validation
              (let [cljs-compiler-config
                    (when cljs-env/*compiler*
                      (get-in @cljs-env/*compiler* [:options :external-config :ghostwheel]))]
                (when (and (not #?(:clj (= (System/getProperty "ghostwheel.enabled") "false") :cljs false))
                           (not (false? (get cljs-compiler-config :enabled))))
                  (merge {}
                         (read-config-file)
                         cljs-compiler-config)))]
          (when plain-config
            (into {} (map (fn [[k v]]
                            [(keyword "ghostwheel.core" (name k))
                             v])
                          plain-config)))))]

  (defn get-env-config
    ([]
     (get-env-config true))
    ([cache?]
     (if (or (not cache?)
             #?(:clj (= (System/getProperty "ghostwheel.cache") "false")))
       (reload-config)
       (let [now (identity #?(:clj (System/currentTimeMillis) :cljs (js/Date.now)))]
         (if (< (- now (::timestamp @*config-cache))
                2000)
           (::value @*config-cache)
           (::value (reset! *config-cache {::timestamp now
                                           ::value     (reload-config)}))))))))


(defn get-base-config
  ([]
   (get-base-config true))
  ([cache?]
   (merge ghostwheel-default-config (get-env-config cache?))))

(defn get-ns-meta [env]
  (if (cljs-env? env)
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


(defmacro set-devtools-config!
  []
  `(let [external-cfg# ~(get-in @cljs-env/*compiler*
                                [:options :external-config :devtools/config])
         default-cfg#  @devtools.defaults/config
         override#     {:max-print-level                                    4
                        :min-expandable-sequable-count-for-well-known-types 2}
         left-adjust#  (str "margin-left: -17px;")]
     (doseq [[k# v#] override#
             :when (not (contains? external-cfg# k#))]
       (devtools.core/set-pref! k# v#))
     (doseq [key# [:header-style]
             :let [val# (get default-cfg# key#)]]
       (devtools.core/set-pref! key# (str val# left-adjust#)))))


