(ns ^:no-doc ghostwheel.config
  #?(:cljs (:require-macros ghostwheel.config))
  (:require [ghostwheel.utils :as util]
            [clojure.spec.alpha :as s]
            #?@(:clj  [[clojure.edn :as edn]]
                :cljs [[cljs.env :as cljs-env]])))

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

                    ;; When `true` no checks of any kind are
                    ;; performed and no test code is generated.
                    :no-check          false

                    ;; Disables side effect detection
                    :no-check-fx       false

                    ;; Determines whether Ghostwheel should warn on missing fspecs
                    ;; and plain (non-Ghostwheel) `defn` usage. When enabled on a
                    ;; namespace or higher level, you can exclude individual `defn`s or
                    ;; `declare`s by setting it to false in their respective metadata
                    :check-coverage    false

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


(defn merge-config [env & meta-maps]
  (s/assert ::ghostwheel-config
            (->> (apply merge-with
                        (fn [a b]
                          (if (every? map? [a b])
                            (merge a b)
                            b))
                        (get-base-config)
                        (util/get-ns-meta env)
                        meta-maps)
                 (filter #(= (-> % key namespace) (name `ghostwheel.core)))
                 (into {}))))
