(ns ^:no-doc ghostwheel.config
  #?(:cljs (:require-macros ghostwheel.config))
  (:require [ghostwheel.utils :as util]
            [ghostwheel.logging :as l]
            [clojure.data :as data]
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

                    ;; Number of generative tests performed by default
                    :gen-tests         0

                    ;; Map of gen-testing profile names to number of
                    ;; tests. You can use the keyword as an argument
                    ;; to `g/check`.
                    :gen-test-profiles {:extensive 300}

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


(defn migrate-deprecated-config
  [config]
  (cond-> config

          (contains? config :ghostwheel.core/check)
          (-> (assoc :ghostwheel.core/no-check (not (:ghostwheel.core/check config)))
              (dissoc :ghostwheel.core/check))

          (contains? config :ghostwheel.core/ignore-fx)
          (-> (assoc :ghostwheel.core/no-check-fx (:ghostwheel.core/ignore-fx config))
              (dissoc :ghostwheel.core/ignore-fx))

          (contains? config :ghostwheel.core/num-tests)
          (-> (assoc :ghostwheel.core/gen-tests (:ghostwheel.core/num-tests config))
              (dissoc :ghostwheel.core/num-tests))

          (contains? config :ghostwheel.core/num-tests-ext)
          (-> (update :ghostwheel.core/gen-test-profiles assoc :extensive (:ghostwheel.core/num-tests-ext config))
              (dissoc :ghostwheel.core/num-tests-ext))

          (contains? config :ghostwheel.core/extensive-tests)
          (dissoc :ghostwheel.core/extensive-tests)))


(defn get-base-config
  ([]
   (get-base-config true))
  ([cache?]
   (->> (get-env-config cache?)
        (merge ghostwheel-default-config)
        migrate-deprecated-config)))


(defn merge-config [env & meta-maps]
  (let [config          (->> (apply merge-with
                                    (fn [a b]
                                      (if (every? map? [a b])
                                        (merge a b)
                                        b))
                                    (get-base-config)
                                    (util/get-ns-meta env)
                                    meta-maps)
                             (filter #(= (-> % key namespace) (name `ghostwheel.core)))
                             (into {}))
        migrated-config (s/assert ::ghostwheel-config
                                  (migrate-deprecated-config config))]
    (when-not (= config migrated-config)
      (let [[old-param _ _] (data/diff config migrated-config)]
        (l/DBG "\nWARNING: Deprecated Ghostwheel configuration options found. To suppress this warning, see `ghostwheel.config/default-config` and `migrate-deprecated-config`, and update your configuration accordingly as it will become invalid in a future release."
               (-> old-param keys vec))))
    migrated-config))
