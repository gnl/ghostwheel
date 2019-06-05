;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.core
  #?(:cljs (:require-macros ghostwheel.core))
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.set :refer [union difference map-invert]]
            [clojure.walk :as walk]
            [clojure.test :as t]
            [clojure.test.check]
            [clojure.test.check.generators]
            [clojure.test.check.properties]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [clojure.spec.gen.alpha :as gen]
            [ghostwheel.reporting :as r]
            [ghostwheel.unghost :refer [clean-defn]]
            [ghostwheel.utils :as u :refer [cljs-env? clj->cljs]]
            [ghostwheel.config :as cfg]
            [ghostwheel.logging :as l]
            [ghostwheel.threading-macros :include-macros true]
            [expound.alpha :as exp]
   ;; REVIEW: Not requiring the clojure.core.specs.alpha
   ;; namespaces for now because they break a lot
   ;; of older code including lein-figwheel <0.5.18
            #?@(:clj  [;[clojure.core.specs.alpha]
                       [orchestra.spec.test :as ost]]
                :cljs [;[cljs.core.specs.alpha :include-macros true]
                       [orchestra-cljs.spec.test :as ost]
                       [cljs.analyzer.api :as ana-api]
                       [cljs.repl :as repl]
                       ghostwheel.tracer])))


;; REVIEW: Replace this pattern:
;; `(let [fn-name (fn  ...)] (defn ...))` with
;; `letfn` when the ClojureScript bug is fixed:
;; https://dev.clojure.org/jira/browse/CLJS-1965


;;;; Global vars and state


;; This isn't particularly pretty, but it's how we avoid
;; having ClojureScript as a required dependency on Clojure
#?(:clj (try
          (do
            (ns-unalias (find-ns 'ghostwheel.core) 'ana-api)
            (require '[cljs.analyzer.api :as ana-api]))
          (catch Exception _ (require '[ghostwheel.stubs.ana-api :as ana-api]))))


(let [{:keys [::expound ::report-output]} (cfg/get-base-config-macro)]
  #?(:clj  (do (alter-var-root #'s/*explain-out*
                               (constantly (exp/custom-printer expound)))
               (alter-var-root #'l/*report-output*
                               (constantly (if (= report-output :js-console)
                                             :repl
                                             report-output))))
     :cljs (do (set! s/*explain-out* (exp/custom-printer expound))
               (set! l/*report-output* report-output))))


;; Borrowed from https://github.com/bhb/expound/issues/152#issuecomment-475621181
;; Uses cljs.repl utilities to format ExceptionInfo objects in Chrome devtools console.
#?(:cljs (do
           (def devtools-error-formatter
             #js{:header  (fn [object config]
                            (when (instance? ExceptionInfo object)
                              (let [err     (repl/error->str object)
                                    message (some->> err
                                                     (re-find #"[^\n]+"))]
                                (println err)
                                #js["span" message])))
                 :hasBody (constantly true)
                 :body    (fn [object config]
                            #js["div" (repl/error->str object)])})

           (defonce _
             (try (do js/window
                      (some-> js/window.devtoolsFormatters
                              (.unshift devtools-error-formatter)))
                  (catch :default _# nil)))))


(def ^:private test-suffix "__ghostwheel-test")
(def ^:private *after-check-callbacks (atom []))
(def ^:private ^:dynamic *unsafe-bound-ops* #{})


(def ^:dynamic *global-trace-allowed?* true)
(def ^:dynamic *global-check-allowed?* true)          ; REVIEW: Is anyone using this?

(def ^:dynamic *gen-tests-or-profile* nil)

;;;; Misc helper functions


(defn- set-trace [enabled]
  #?(:clj  (alter-var-root #'*global-trace-allowed?* (constantly enabled))
     :cljs (set! *global-trace-allowed?* enabled)))


(defn enable-trace! [] (set-trace true) "Tracing enabled.")
(defn disable-trace! [] (set-trace false) "Tracing disabled.")


(defn- set-check [enabled]
  #?(:clj  (alter-var-root #'*global-check-allowed?* (constantly enabled))
     :cljs (set! *global-check-allowed?* enabled)))


(defn enable-check! [] (set-check true) "Check enabled.")
(defn disable-check! [] (set-check false) "Check disabled.")


(defn- count-args
  "Returns a tuple with the number of regular and non-variadic arguments."
  [conformed-args]
  [(count (:args conformed-args))
   (if (:varargs conformed-args) 1 0)])


(defn- resolve-trace-color [color]
  (let [[color-type color-value] (s/conform ::trace-color color)]
    (case color-type
      :literal color-value
      :keyword (if-let [color-value (get l/ghostwheel-colors color)]
                 color-value
                 (:black l/ghostwheel-colors)))))


(defn gen-cleanup-console-on-exception
  [cljs? form]
  `(try ~form
        (catch ~(if cljs? :default 'Throwable) e#
          (do
            (dorun (repeatedly 10 l/group-end))
            (throw e#)))))


(defn- get-file-position [env]
  (if (cljs-env? env)
    (let [{:keys [line column]} env]
      (if (> line 1)
        (str line ":" column)
        "REPL"))
    ;; TODO implement for clojure
    nil))


;;;; Operators


;; It doesn't actually matter what these are bound to, they are stripped by
;; the macros they're used in and never end up in the final code. This is just
;; so they can be used without '=> cannot be resolved' errors in the IDE.
(def => :ret)
(def | :st)
(def <- :gen)


(defmacro ? [& forms]
  (cond-> `(s/nilable ~@forms)
          (cljs-env? &env) clj->cljs))


;;;; Specs


(s/def ::trace #{0 1 2 3 4 5 6 true})
(s/def ::trace-color (s/or :keyword keyword?
                           :literal (s/and string?
                                           #(re-matches #"#[a-fA-F0-9]+" %)
                                           #(or (= (count %) 7)
                                                (= (count %) 4)))))
(s/def ::no-check boolean?)
(s/def ::check-coverage boolean?)
(s/def ::no-check-fx boolean?)
(s/def ::gen-tests nat-int?)
(s/def ::gen-test-profiles (s/map-of keyword? int?))
(s/def ::defn-macro (s/nilable string?))
(s/def ::instrument boolean?)
(s/def ::outstrument boolean?)
(s/def ::extrument (s/nilable (s/coll-of qualified-symbol? :kind vector?)))
(s/def ::expound (s/nilable (s/map-of keyword? any?)))
(s/def ::report-output #{:repl :js-console nil})

;; TODO: Integrate bhauman/spell-spec
(s/def ::ghostwheel-config
  (s/and (s/keys :req [::trace ::trace-color ::no-check ::no-check-fx ::check-coverage
                       ::gen-tests ::gen-test-profiles ::defn-macro
                       ::instrument ::outstrument ::extrument ::expound ::report-output])))

(s/assert ::ghostwheel-config cfg/ghostwheel-default-config)
;; TODO: Add check to make sure instrument and outstrument aren't both on


;; These are lifted straight from clojure.core.specs.alpha, because it
;; didn't seem possible to access them directly in the original namespace.

(s/def ::local-name (s/and simple-symbol? #(not= '& %)))

;; sequential destructuring

(s/def ::seq-binding-form
  (s/and vector?
         (s/cat :elems (s/* ::binding-form)
                :rest (s/? (s/cat :amp #{'&} :form ::binding-form))
                :as (s/? (s/cat :as #{:as} :sym ::local-name)))))

;; map destructuring

(s/def ::keys (s/coll-of ident? :kind vector?))
(s/def ::syms (s/coll-of symbol? :kind vector?))
(s/def ::strs (s/coll-of simple-symbol? :kind vector?))
(s/def ::or (s/map-of simple-symbol? any?))
(s/def ::as ::local-name)

(s/def ::map-special-binding
  (s/keys :opt-un [::as ::or ::keys ::syms ::strs]))

(s/def ::map-binding (s/tuple ::binding-form any?))

(s/def ::ns-keys
  (s/tuple
   (s/and qualified-keyword? #(-> % name #{"keys" "syms"}))
   (s/coll-of simple-symbol? :kind vector?)))

(s/def ::map-bindings
  (s/every (s/or :mb ::map-binding
                 :nsk ::ns-keys
                 :msb (s/tuple #{:as :or :keys :syms :strs} any?))
           :into {}))

(s/def ::map-binding-form (s/merge ::map-bindings ::map-special-binding))

(s/def ::binding-form
  (s/or :sym ::local-name
        :seq ::seq-binding-form
        :map ::map-binding-form))

;;; Function and >defn specs

(s/def ::arg-list
  (s/and vector?
         (s/cat :args (s/* ::binding-form)
                :varargs (s/? (s/cat :amp #{'&} :form ::binding-form)))))

(s/def ::pred-arg-list
  (s/and vector?
         (s/cat :args (s/* (s/or :sym ::local-name)))))

(s/def ::anon-args+body
  (s/cat :args ::arg-list
         :body (s/* any?)))

(s/def ::anon-fn
  (s/and seq?
         (s/cat :op #{'fn* 'fn}
                :name (s/? simple-symbol?)
                :bs (s/alt :arity-1 ::anon-args+body
                           :arity-n (s/+ (s/spec ::anon-args+body))))))

(s/def ::pred-fn
  (s/and seq?
         (s/cat :op #{'fn* 'fn}
                :name (s/? simple-symbol?)
                :args ::pred-arg-list
                :body any?)))

(s/def ::spec-elem
  (s/or :set set?
        :pred-sym (s/and symbol?
                         (complement #{'| '=>})
                         ;; REVIEW: should the `?` be a requirement?
                         #(string/ends-with? (str %) "?"))
        :gspec (s/or :nilable-gspec ::nilable-gspec :gspec ::gspec)
        :spec-key qualified-keyword?
        :fun ::pred-fn
        :list seq?))

(s/def ::such-that-op #{:st '|})
(s/def ::ret-op #{:ret '=>})
(s/def ::gen-op #{:gen '<-})

(s/def ::gspec
  (s/and vector?
         (s/cat :args (s/? (s/cat :args (s/+ ::spec-elem)
                                  :args-such-that (s/? (s/cat :op ::such-that-op
                                                              :preds (s/+ ::pred-fn)))))
                :ret-op ::ret-op
                :ret ::spec-elem
                :fn-such-that (s/? (s/cat :op ::such-that-op
                                          :preds (s/+ ::pred-fn)))
                :gen (s/? (s/cat :op ::gen-op
                                 :gen-fn (s/? (some-fn seq? symbol?)))))))

(s/def ::nilable-gspec
  (s/and vector?
         (s/cat :maybe #{'? 's/nilable}
                :gspec ::gspec)))

(s/def ::prepost (s/map-of #{:pre :post}
                           (s/coll-of seq?
                                      :kind vector?
                                      :distinct true)))

(s/def ::args+body
  (s/cat :args ::arg-list
         :body (s/alt :prepost+body (s/cat :prepost ::prepost
                                           :body (s/+ any?))
                      :body (s/* any?))))

(s/def ::args+gspec+body
  (s/&
   (s/cat :args ::arg-list
          :gspec (s/nilable ::gspec)
          :body (s/alt :prepost+body (s/cat :prepost ::prepost
                                            :body (s/+ any?))
                       :body (s/* any?)))
   (fn arg-specs-match-param-count? [{:keys [args gspec]}]
     (if-not gspec
       true
       (let [argcount  (->> args count-args (apply +))
             spec-args (:args gspec)]
         (if spec-args
           (-> spec-args :args count (= argcount))
           (= argcount 0)))))))


(s/def ::defn
  (s/and seq?
         (s/cat :op #{'defn 'defn-}
                :name simple-symbol?
                :docstring (s/? string?)
                :meta (s/? map?)
                :bs (s/alt :arity-1 ::args+body
                           :arity-n (s/cat :bodies (s/+ (s/spec ::args+body))
                                           :attr (s/? map?))))))


(s/def ::deftest
  (s/and seq?
         (s/cat :op #{'clojure.test/deftest 'cljs.test/deftest}
                :name symbol?
                :body any?)))


;;; Side effect detection specs

(def threading-macro-syms
  #{'-> '->> 'as-> 'cond-> 'cond->> 'some-> 'some->>
    '*-> '*->> '*as-> '*cond-> '*cond->> '*some-> '*some->>})

(s/def ::threading-macro-op threading-macro-syms)

(s/def ::binding-op
  #{'let 'for 'doseq 'binding})

(s/def ::single-function-composition
  #{'partial 'fnil})

(s/def ::safe-single-function-composition
  #{'memoize 'complement})

(s/def ::multi-function-composition
  #{'comp})

(s/def ::safe-multi-function-composition
  #{'juxt 'every-pred 'some-fn})

(s/def ::function-application
  #{'apply 'map 'fmap 'map-indexed 'reduce})

(s/def ::safe-function-application
  #{'mapcat 'reduce-kv 'mapv 'reductions 'iterate 'keep 'keep-indexed
    'remove 'filter 'filterv 'take-while 'drop-while
    'sort 'sort-by 'sorted-map-by 'group-by 'merge-with})

(s/def ::unsafe-clj-block #{'do
                            'doseq
                            'dotimes})

;; REVIEW: maybe move the re-frame stuff out of here
(s/def ::unsafe-clj-call #{'dorun
                           'repeatedly
                           'dispatch
                           'js-delete
                           'aset})

(s/def ::unsafe-clj-comp
  (s/alt :single-fn (s/cat :composition (s/alt :generic ::single-function-composition
                                               :safe ::safe-single-function-composition)
                           :unsafe-op ::unsafe-op
                           :rest ::rest)
         :multi-fn (s/cat :composition (s/alt :generic ::multi-function-composition
                                              :safe ::safe-multi-function-composition)
                          :some-unsafe-ops ::some-unsafe-ops
                          :rest ::rest)))

(let [bang-suffix? #(string/ends-with? (str %) "!")]
  (s/def ::bang-suffix (every-pred symbol? bang-suffix?))
  (s/def ::unsafe-op
    (s/alt :bang-suffix ::bang-suffix
           :unsafe-anon-fn (s/and seq?
                                  (s/alt :unsafe-body (s/cat :fun #{'fn 'fn*}
                                                             :name (s/? simple-symbol?)
                                                             :args ::arg-list
                                                             :body ::unsafe-form)
                                         :unsafe-name (s/cat :fun #{'fn 'fn*}
                                                             :name (every-pred simple-symbol?
                                                                               bang-suffix?)
                                                             :args ::arg-list
                                                             :body any?)))
           :unsafe-clj-call ::unsafe-clj-call
           :unsafe-clj-comp (s/spec ::unsafe-clj-comp)
           :unsafe-bound-call #(contains? *unsafe-bound-ops* %)
           :multi-form-op (s/cat :op #{'when 'when-not 'when-let 'when-first
                                       'when-some 'let 'binding}
                                 :pred-or-bindings any?
                                 :fx (s/+ any?)
                                 :return any?))))

(s/def ::safe-op #(not (s/valid? ::unsafe-op (list %))))

(s/def ::some-unsafe-ops (s/+ (s/cat :skipped-ops (s/* ::safe-op)
                                     :unsafe-op ::unsafe-op)))

(s/def ::rest (s/* any?))

(s/def ::some-unsafe-bindings
  (s/and vector?
         (s/+ (s/cat :skipped-bindings (s/* (s/cat :binding ::binding-form
                                                   :value ::safe-op))
                     :unsafe-binding (s/cat :binding ::binding-form
                                            :value ::unsafe-op)))))

(s/def ::unsafe-form
  ;; REVIEW: maybe make sure we are only matching on the simple symbol part
  (s/or :unsafe-block (s/and seq?
                             (s/cat :unsafe-clj-block ::unsafe-clj-block
                                    :rest ::rest))

        :unsafe-call
        (s/and seq?
               (s/alt :direct (s/cat :application
                                     (s/? (s/alt :generic ::function-application
                                                 :safe ::safe-function-application))
                                     :unsafe-op ::unsafe-op
                                     :rest ::rest)
                      :threading (s/cat :threading-macro-op ::threading-macro-op
                                        :threaded-form any?
                                        :some-unsafe-ops ::some-unsafe-ops
                                        :rest ::rest)
                      :update (s/cat :update #{'update 'update-in}
                                     :map any?
                                     :path any?
                                     :unsafe-op ::unsafe-op
                                     :rest ::rest)))

        :unsafe-composition (s/and seq? ::unsafe-clj-comp)
        :unsafe-binding (s/and seq?
                               (s/cat :binding-op ::binding-op
                                      :bindings ::some-unsafe-bindings
                                      :rest ::rest))
        :unsafe-argument (s/and seq?
                                (s/cat :fun ::safe-op
                                       :some-unsafe-ops ::some-unsafe-ops
                                       :rest ::rest))
        #_::unsafe-something #_(s/spec (s/cat ::some-unsafe-ops ::some-unsafe-ops
                                              ::rest ::rest))))


;;;; Main code generating functions


(let [find-fx
      (fn find-fx [form]
        (let [maybe-fx           (s/conform ::unsafe-form form)
              [found-fx
               unsafe-bindings] (if (= maybe-fx ::s/invalid)
                                  [nil nil]
                                  [(conj {} maybe-fx)
                                   (when (= (key maybe-fx) :unsafe-binding)
                                     (-> maybe-fx
                                         val
                                         :bindings))])
              ;; TODO implement map and vec destructuring support for bindings
              unsafe-binding-set (when unsafe-bindings
                                   (->> unsafe-bindings
                                        (map #(-> % :unsafe-binding :binding val))
                                        (set)))]
          [found-fx (vec
                     (for [nested-form form
                           :when (and (coll? nested-form))]
                       ;; REVIEW go into nested anon-fns or not?
                       ;(not (contains? #{'fn 'fn*} (first nested-form))))]
                       (binding [*unsafe-bound-ops* (cond-> *unsafe-bound-ops*
                                                            unsafe-bindings (union unsafe-binding-set))]
                         (find-fx nested-form))))]))

      check-arity-fx
      (fn [unformed-args-gspec-body]
        (let [effects (->> (find-fx unformed-args-gspec-body)
                           (flatten)
                           (remove nil?)
                           (map first)
                           (vec))]
          (-> (for [fx effects]
                [(-> fx key name keyword)
                 (->> fx
                      (s/unform ::unsafe-form)
                      (apply list)
                      (str))
                 (->> fx
                      val
                      (walk/postwalk #(if (qualified-keyword? %)
                                        (keyword (name %))
                                        %))
                      vec)])
              (cond->> (next unformed-args-gspec-body) (cons [:multiple-body-forms])))))]
  (defn- generate-test [fn-name fspecs body-forms config cljs?]
    (let [{:keys [::gen-tests ::gen-test-profiles ::check-coverage ::check-fx]}
          config
          marked-unsafe     (s/valid? ::bang-suffix fn-name)
          found-fx          (if-not check-fx
                              []
                              (->> (case (key body-forms)
                                     :arity-1 [(val body-forms)]
                                     :arity-n (val body-forms))
                                   (map #(->> % (s/unform ::args+gspec+body) (drop 2)))
                                   (mapcat check-arity-fx)
                                   distinct
                                   vec))
          unexpected-fx     (boolean (and check-fx
                                          (not marked-unsafe)
                                          (seq found-fx)))
          unexpected-safety (boolean (and check-fx
                                          marked-unsafe
                                          (empty? found-fx)))
          spec-keyword-ns   (if cljs? 'clojure.test.check 'clojure.spec.test.check)
          num-tests-sym     (gensym "num-tests_")]
      [unexpected-fx
       `(t/deftest ~(symbol (str fn-name test-suffix))
          (let [~num-tests-sym (if (int? *gen-tests-or-profile*)
                                 *gen-tests-or-profile*
                                 (get ~gen-test-profiles *gen-tests-or-profile* ~gen-tests))
                spec-checks# ~(let [defined-fspecs (->> fspecs (remove nil?) vec)]
                                (when (and (seq defined-fspecs)
                                           (not marked-unsafe)
                                           (empty? found-fx))
                                  `(when (> ~num-tests-sym 0)
                                     (for [spec# ~defined-fspecs]
                                       (st/check-fn
                                        ~fn-name
                                        spec#
                                        {~(keyword (str spec-keyword-ns) "opts")
                                         {:num-tests ~num-tests-sym}})))))]
            ;; TODO The `spec-checks#` thing trips up clairvoyant
            ;; and prevents tracing during ghostwheel development
            (t/is (and (every? #(-> %
                                    ~(keyword (str spec-keyword-ns) "ret")
                                    :pass?)
                               spec-checks#)
                       ~(not unexpected-fx)
                       ~(not unexpected-safety))
                  {::r/fn-name        (quote ~fn-name)
                   ::r/fspec          ~(every? some? fspecs)
                   ::r/spec-checks    spec-checks#
                   ::r/check-coverage ~check-coverage
                   ::r/num-tests      ~num-tests-sym
                   ::r/failure        ~(cond unexpected-fx ::r/unexpected-fx
                                             unexpected-safety ::r/unexpected-safety
                                             :else ::r/spec-failure)
                   ::r/found-fx       (quote ~found-fx)
                   ::r/marked-unsafe  ~marked-unsafe})))])))


(defn- unscrew-vec-unform
  "Half-arsed workaround for spec bugs CLJ-2003 and CLJ-2021."
  [unformed-arg]
  (if-not (sequential? unformed-arg)
    unformed-arg
    (let [malformed-seq-destructuring? (every-pred seq? (comp #{:as '&} first))
          [unformed malformed] (split-with (complement malformed-seq-destructuring?) unformed-arg)]
      (vec (concat unformed (apply concat malformed))))))


(defn- gspec->fspec*
  [conformed-arg-list conformed-gspec anon-fspec? multi-arity-args? nilable?]
  (let [{argspec-def              :args
         retspec                  :ret
         fn-such-that             :fn-such-that
         {:keys [gen-fn] :as gen} :gen}
        conformed-gspec]
    (if (and anon-fspec?
             argspec-def
             (not gen)
             (some #{'any?} (-> argspec-def :args vals)))
      (if nilable? `(s/nilable ifn?) `ifn?)
      (let [extract-spec
            (fn extract-spec [[spec-type spec]]
              (if (= spec-type :gspec)
                (if (= (key spec) :nilable-gspec)
                  (gspec->fspec* nil (-> spec val :gspec) true false true)
                  (gspec->fspec* nil (val spec) true false false))
                spec))

            named-conformed-args
            (when argspec-def
              (let [all-args     (remove nil? (concat (:args conformed-arg-list)
                                                      [(-> conformed-arg-list :varargs :form)]))
                    gen-arg-name (fn [index] (str "arg" (inc index)))
                    gen-name     (fn [index [arg-type arg :as full-arg]]
                                   (let [arg-name (if-not arg-type
                                                    (gen-arg-name index)
                                                    (case arg-type
                                                      :sym arg
                                                      :seq (or (-> arg :as :sym)
                                                               (gen-arg-name index))
                                                      :map (or (-> arg :as)
                                                               (gen-arg-name index))))]
                                     [(keyword arg-name) full-arg]))]
                (map-indexed gen-name (or (seq all-args)
                                          (-> argspec-def :args count (repeat nil))))))

            arg-binding-map
            (if-not conformed-arg-list
              {}
              (if (every? #(= (-> % second key) :sym) named-conformed-args)
                `{:keys ~(vec (map #(-> % first name symbol) named-conformed-args))}
                (->> named-conformed-args
                     (map (fn [[arg-key conformed-arg]]
                            [(->> conformed-arg (s/unform ::binding-form) unscrew-vec-unform)
                             arg-key]))
                     (into {}))))

            process-arg-pred
            (fn process-arg-pred [{:keys [name args body]}]
              (let [bindings (if-let [anon-arg (some-> args :args first second)]
                               (assoc arg-binding-map :as anon-arg)
                               arg-binding-map)]
                (remove nil? `(fn ~name [~bindings] ~body))))

            processed-args
            (if-not argspec-def
              `(s/cat)
              (let [wrapped-params (->> argspec-def
                                        :args
                                        (map extract-spec)
                                        (interleave (map first named-conformed-args))
                                        (cons `s/cat))]
                (if-let [args-such-that (:args-such-that argspec-def)]
                  (->> args-such-that
                       :preds
                       (map process-arg-pred)
                       (list* `s/and wrapped-params))
                  wrapped-params)))

            process-ret-pred
            (fn process-ret-pred [{:keys [name args body]}]
              (let [anon-arg       (some-> args :args first second)
                    ret-sym        (gensym "ret__")
                    bindings       [{(if multi-arity-args?
                                       ['_ arg-binding-map]
                                       arg-binding-map) :args
                                     ret-sym            :ret}]
                    processed-body (if anon-arg
                                     (walk/postwalk-replace {anon-arg ret-sym} body)
                                     body)]
                (remove nil? `(fn ~name ~bindings ~processed-body))))

            fn-spec
            (when fn-such-that
              (let [processed-ret-preds (map process-ret-pred (:preds fn-such-that))]
                (if (next processed-ret-preds)
                  (cons `s/and processed-ret-preds)
                  (first processed-ret-preds))))

            final-fspec
            (concat (when anon-fspec? [`s/fspec])
                    [:args processed-args]
                    [:ret (extract-spec retspec)]
                    (when fn-spec [:fn fn-spec])
                    (when gen-fn [:gen gen-fn]))]
        (if nilable? `(s/nilable ~final-fspec) final-fspec)))))


;; TODO make sure we check whether the variadic bodies are legit
;; Can not have more than one
;; Can not have one with more regular args than the variadic one
;; To what extent does the compiler already check this?
(let [get-fspecs    (fn [fn-body]
                      (let [[param-count variadic] (-> fn-body :args count-args)
                            gspec (or (:gspec fn-body)
                                      (s/conform ::gspec
                                                 (vec (concat (repeat param-count 'any?)
                                                              (when (> variadic 0)
                                                                `[(s/* any?)])
                                                              '[=> any?]))))]
                        [(->> (if (> variadic 0) "n" param-count)
                              (str "arity-")
                              keyword)
                         (gspec->fspec* (:args fn-body) gspec false true false)]))
      get-spec-part (fn [part spec]
                      (->> spec
                           (drop-while (complement #{part}))
                           second))]
  (defn- generate-fspec-body [fn-bodies]
    (case (key fn-bodies)
      :arity-1
      (when-let [gspec (-> fn-bodies val :gspec)]
        (gspec->fspec* (-> fn-bodies val :args) gspec false false false))

      :arity-n
      (when (some :gspec (val fn-bodies))
        (let [fspecs           (map get-fspecs (val fn-bodies))
              arg-specs        (mapcat (fn [[arity spec]]
                                         [arity (or (get-spec-part :args spec) `empty?)])
                                       fspecs)
              fn-param         (gensym "p1__")
              multi-ret-specs  (when (->> fspecs
                                          (map #(get-spec-part :ret (second %)))
                                          distinct
                                          count
                                          (not= 1))
                                 (mapcat (fn [[arity spec]]
                                           [arity `(s/valid? ~(get-spec-part :ret spec)
                                                             (:ret ~fn-param))])
                                         fspecs))
              get-fn-clause    (partial get-spec-part :fn)
              fn-specs         (when (->> fspecs (map second) (some get-fn-clause))
                                 (mapcat (fn [[arity spec]]
                                           [arity (if-let [fn-spec (get-fn-clause spec)]
                                                    `(s/valid? ~fn-spec ~fn-param)
                                                    true)])
                                         fspecs))
              ;; NOTE: destructure args and ret in the arg vec
              multi-ret-clause (when multi-ret-specs
                                 `(fn ~'valid-multi-arity-ret? [~fn-param]
                                    (case (-> ~fn-param :args key)
                                      ~@multi-ret-specs)))
              multi-fn-clause  (when fn-specs
                                 `(fn ~'valid-multi-arity-fn? [~fn-param]
                                    (case (-> ~fn-param :args key)
                                      ~@fn-specs)))]
          ;; Using s/or here even though s/alt seems to be more common
          ;; for multi-arity specs in the wild. The spec error reporting
          ;; is much better and it's immediately clear what didn't match.
          (concat [:args `(s/or ~@arg-specs)]
                  (when-not multi-ret-clause
                    [:ret (get-spec-part :ret (-> fspecs first second))])
                  (when (or multi-ret-clause multi-fn-clause)
                    [:fn (if multi-fn-clause
                           (if multi-ret-clause
                             `(s/and ~multi-ret-clause ~multi-fn-clause)
                             multi-fn-clause)
                           multi-ret-clause)])))))))


(def ^:private spec-op->type
  (let [map-prot     "cljs.core.IMap"
        coll-prot    "cljs.core.ICollection"
        ;; Needed because Closure compiler/JS doesn't consider strings seqable
        seqable-prot "(cljs.core.ISeqable|string)"]
    {'number?      "number"
     'integer?     "number"
     'int?         "number"
     'nat-int?     "number"
     'pos-int?     "number"
     'neg-int?     "number"
     'float?       "number"
     'double?      "number"
     'int-in       "number"
     'double-in    "number"

     'string?      "string"

     'boolean?     "boolean"

     'keys         map-prot
     'map-of       map-prot
     'map?         map-prot
     'merge        map-prot

     'set?         "cljs.core.ISet"
     'vector?      "cljs.core.IVector"
     'tuple        "cljs.core.IVector"
     'seq?         "cljs.core.ISeq"
     'seqable?     seqable-prot
     'associative? "cljs.core.IAssociative"
     'atom?        "cljs.core.IAtom"

     'coll-of      coll-prot
     'every        coll-prot

     'keyword?     "cljs.core.Keyword"
     'ifn?         "cljs.core.IFn"
     'fn?          "Function"}))


(declare get-gspec-type)


(defn- get-type [recursive-call conformed-spec-elem]
  (let [[spec-type spec-def] conformed-spec-elem

        spec-op
        ;; REVIEW: This kinda wants to be a multi-method when it grows up.
        (case spec-type
          :list (let [op (-> spec-def first name symbol)]
                  (cond
                    (#{'nilable '?} op) (concat (->> spec-def
                                                     second
                                                     (s/conform ::spec-elem)
                                                     (get-type true))
                                                [::nilable])
                    (#{'* '+} op) (concat (->> spec-def
                                               second
                                               (s/conform ::spec-elem)
                                               (get-type true))
                                          [::variadic])
                    (#{'and} op) [(-> spec-def second)] ; TODO
                    (#{'coll-of 'every} op) [(or (->> spec-def
                                                      (drop-while (complement #{:kind}))
                                                      second)
                                                 op)]
                    :else [op]))
          ;;TODO support (some-fn and (s/or
          :gspec (let [gspec-def (val spec-def)]
                   (if (= (key spec-def) :nilable-gspec)
                     [(get-gspec-type (:gspec gspec-def)) ::nilable]
                     [(get-gspec-type gspec-def)]))
          :pred-sym [spec-def]
          [nil])]
    (if recursive-call
      spec-op
      (if-let [js-type (spec-op->type (first spec-op))]
        (let [modifiers (set (rest spec-op))]
          (as-> js-type t
            (str (if (::nilable modifiers) "?" "!") t)
            (str (when (::variadic modifiers) "...") t)))
        "*"))))


(defn- get-gspec-type [conformed-gspec]
  (let [argspec-def (:args conformed-gspec)
        args-jstype (if-not argspec-def
                      ""
                      (->> (-> conformed-gspec :args :args)
                           (map (partial get-type false))
                           (string/join ", ")))
        ret-jstype  (get-type false (:ret conformed-gspec))]
    (str "function(" args-jstype "): " ret-jstype)))


(defn- generate-type-annotations [env conformed-bs]
  (when (cljs-env? env)
    (case (key conformed-bs)
      :arity-1 (when-let [gspec (-> conformed-bs val :gspec)]
                 {:jsdoc [(str "@type {" (get-gspec-type gspec) "}")]})
      ;; REVIEW: There doesn't seem to be a way to get valid annotations for args of
      ;; multi-arity functions and attempts to just annotate the return value(s) failed
      ;; as well. It wasn't possible to put together an annotation which was both
      ;; considered valid and resulted in a successful type check.
      :arity-n nil #_(when-let [ret-types (as-> (val conformed-bs) x
                                            (map #(get-type false (-> % :gspec :ret)) x)
                                            (distinct x)
                                            (when (not-any? #{"*" "?"} x) x))]
                       {:jsdoc [(str "@return {" (string/join "|" ret-types) "}")]}))))


(defn- get-quoted-qualified-fn-name [fn-name env]
  `(quote ~(symbol (str (u/get-ns-name env)) (str fn-name))))


(defn- trace-threading-macros [forms trace cljs?]
  (if (< trace 4)
    forms
    (let [untraced-macros->traced
          {'->      'ghostwheel.threading-macros/*->
           '->>     'ghostwheel.threading-macros/*->>
           'as->    'ghostwheel.threading-macros/*as->
           'cond->  'ghostwheel.threading-macros/*cond->
           'cond->> 'ghostwheel.threading-macros/*cond->>
           'some->  'ghostwheel.threading-macros/*some->
           'some->> 'ghostwheel.threading-macros/*some->>}

          traced-macros->untraced
          (map-invert untraced-macros->traced)]
      (cond->> (walk/postwalk-replace untraced-macros->traced forms)

               ;; Make sure we don't trace threading macros in anon-fns
               ;; when anon-fns themselves aren't traced
               (< trace 5)
               (walk/postwalk
                (fn [form]
                  (if (and (list? form)
                           (#{'fn 'fn*} (first form)))
                    (walk/postwalk-replace traced-macros->untraced form)
                    form)))

               :always
               (walk/postwalk
                (fn [form]
                  (if (and (list? form)
                           (-> (keys traced-macros->untraced)
                               set
                               (contains? (first form))))
                    (gen-cleanup-console-on-exception cljs? form)
                    form)))))))


;; REVIEW – refactor/keywordify args, this is getting unwieldy
(defn- clairvoyant-trace [forms trace color env label position]
  (let [clairvoyant 'clairvoyant.core/trace-forms
        tracer      'ghostwheel.tracer/tracer
        exclude     (case trace
                      2 '#{'fn 'fn* 'let}
                      3 '#{'fn 'fn*}
                      4 '#{'fn 'fn*}
                      5 '#{:unnamed-fn}
                      nil)
        ;; Uncommenting the block below will strip nested `|>` or `tr` traces
        #_(comment
           inline-trace? (fn [form]
                           (and (seq? form)
                                (symbol? (first form))
                                (let [sym (first form)

                                      qualified-sym
                                          (if (cljs-env? env)
                                            (:name (ana-api/resolve env sym))
                                            ;; REVIEW: Clairvoyant doesn't work on
                                            ;; Clojure yet – check this when it does
                                            #?(:clj (name (resolve sym))))]
                                  (contains? #{'ghostwheel.core/|> 'ghostwheel.core/tr} qualified-sym))))
           forms (walk/postwalk
                  #(if (inline-trace? %) (second %) %)
                  forms))]
    ;; REVIEW: This doesn't quite work right and seems to cause issues for some people. Disabling for now.
    (comment
     #?(:clj (if cljs?
               (when-not (and (find-ns (symbol (namespace clairvoyant)))
                              (find-ns (symbol (namespace tracer))))
                 (throw (Exception. "Can't find tracing namespaces. Either add `gnl/ghostwheel-tracer` artifact and `(:require [ghostwheel.tracer])`, or disable tracing in order to compile.")))
               (throw (Exception. "Tracing is not yet implemented for Clojure.")))))
    (if (< trace 2)
      forms
      `(~clairvoyant
        {:enabled true
         :binding [~'devtools.prefs/*current-config*
                   ~(u/devtools-config-override)]
         :tracer  (~tracer
                   :color "#fff"
                   :background ~color
                   :prefix ~label
                   :suffix ~position
                   :expand ~(cond (>= trace 5) '#{:bindings 'let 'defn 'defn- 'fn 'fn*}
                                  (>= trace 3) '#{:bindings 'let 'defn 'defn-}
                                  :else '#{'defn 'defn-}))
         :exclude ~exclude}
        ~forms))))


(defn- generate-fdef
  [env forms]
  (let [{[type fn-name] :name bs :bs} (s/conform ::>fdef-args forms)]
    (case type
      :sym (let [quoted-qualified-fn-name (get-quoted-qualified-fn-name fn-name env)
                 {:keys [::instrument ::outstrument]} (cfg/merge-config env (meta fn-name))
                 instrumentation          (cond outstrument `(ost/instrument ~quoted-qualified-fn-name)
                                                instrument `(st/instrument ~quoted-qualified-fn-name)
                                                :else nil)
                 fdef                     `(s/fdef ~fn-name ~@(generate-fspec-body bs))]
             (if instrumentation
               `(do ~fdef ~instrumentation)
               fdef))
      :key `(s/def ~fn-name (s/fspec ~@(generate-fspec-body bs))))))


(defn- process-defn-body
  [cfg fspec args+gspec+body]
  (let [{:keys [env fn-name traced-fn-name trace color unexpected-fx]} cfg
        {:keys [args body]} args+gspec+body
        [prepost orig-body-forms] (case (key body)
                                    :prepost+body [(-> body val :prepost)
                                                   (-> body val :body)]
                                    :body [nil (val body)])
        process-arg (fn [[arg-type arg]]
                      (as-> arg arg
                        (case arg-type
                          :sym [arg-type arg]
                          :seq [arg-type (update arg :as #(or % {:as :as :sym (gensym "arg_")}))]
                          :map [arg-type (update arg :as #(or % (gensym "arg_")))])))
        ;; NOTE: usage of extract-arg isn't elegant, there's duplication, refactor
        extract-arg (fn [[arg-type arg]]
                      (case arg-type
                        :sym arg
                        :seq (get-in arg [:as :sym])
                        :map (:as arg)
                        nil))
        unform-arg  #(->> % (s/unform ::binding-form) unscrew-vec-unform)
        reg-args    (->> args :args (map process-arg))
        var-arg     (some-> args :varargs :form process-arg)
        arg-list    (vec (concat (map unform-arg reg-args)
                                 (when var-arg ['& (unform-arg var-arg)])))
        body-forms  (if (and fspec (empty? orig-body-forms))
                      ;; TODO error handling when specs too fancy for stub auto-generation
                      [`(do
                          (when l/*report-output*
                            (println ~(str fn-name " – No function body => Generating random spec-based output.")))
                          (apply (-> ~fspec s/gen gen/generate)
                                 ~@(map extract-arg reg-args) ~(extract-arg var-arg)))]

                      (cond unexpected-fx
                            [`(throw (~(if (cljs-env? env) 'js/Error. 'Exception.)
                                      ~(str "Calling function `"
                                            fn-name
                                            "` which has unexpected side effects.")))]

                            (= trace :dispatch)
                            [`(if *global-trace-allowed?*
                                (apply ~traced-fn-name
                                       ~@(map extract-arg reg-args)
                                       ~(extract-arg var-arg))
                                (do ~@orig-body-forms))]

                            (= trace 1)
                            `[(do
                                (l/dlog nil
                                        ~(str (list fn-name arg-list))
                                        {::l/background ~color})
                                ~@orig-body-forms)]

                            (>= trace 4)
                            (trace-threading-macros orig-body-forms trace (cljs-env? env))

                            :else
                            orig-body-forms))]
    (remove nil? `(~arg-list ~prepost ~@body-forms))))


(defn- generate-defn
  [forms private env]
  (let [cljs?             (cljs-env? env)
        conformed-gdefn   (s/conform ::>defn-args forms)
        fn-bodies         (:bs conformed-gdefn)
        empty-bodies      (every? empty?
                                  (case (key fn-bodies)
                                    :arity-1 (list (-> fn-bodies val :body val))
                                    :arity-n (->> fn-bodies
                                                  val
                                                  (map :body)
                                                  (map val))))
        arity             (key fn-bodies)
        fn-name           (:name conformed-gdefn)
        quoted-qualified-fn-name
                          (get-quoted-qualified-fn-name fn-name env)
        traced-fn-name    (gensym (str fn-name "__"))
        docstring         (:docstring conformed-gdefn)
        meta-map          (merge (:meta conformed-gdefn)
                                 (generate-type-annotations env fn-bodies)
                                 {::ghostwheel true})
        ;;; Assemble the config
        config            (cfg/merge-config env (meta fn-name) meta-map)
        color             (resolve-trace-color (::trace-color config))
        {:keys [::defn-macro ::instrument ::outstrument ::trace
                ::no-check ::gen-tests]} config
        defn-sym          (cond defn-macro (with-meta (symbol defn-macro) {:private private})
                                private 'defn-
                                :else 'defn)
        trace             (if (cljs-env? env)
                            (cond empty-bodies 0
                                  (true? trace) 5
                                  :else trace)
                            0)                        ; TODO: Clojure
        ;;; Code generation
        fdef-body         (generate-fspec-body fn-bodies)
        fdef              (when fdef-body `(s/fdef ~fn-name ~@fdef-body))
        instrumentation   (when (not empty-bodies)
                            (cond outstrument `(ost/instrument ~quoted-qualified-fn-name)
                                  instrument `(st/instrument ~quoted-qualified-fn-name)
                                  :else nil))
        individual-arity-fspecs
                          (map (fn [{:keys [args gspec]}]
                                 (when gspec
                                   (gspec->fspec* args gspec true false false)))
                               (val fn-bodies))
        [unexpected-fx generated-test] (when (and (not empty-bodies)
                                                  (not no-check))
                                         (let [fspecs (case arity
                                                        :arity-1 [(when fdef-body `(s/fspec ~@fdef-body))]
                                                        :arity-n individual-arity-fspecs)]
                                           (generate-test fn-name fspecs fn-bodies config cljs?)))
        process-fn-bodies (fn [trace]
                            (let [process-cfg {:env            env
                                               :fn-name        fn-name
                                               :traced-fn-name traced-fn-name
                                               :trace          trace
                                               :color          color
                                               :unexpected-fx  unexpected-fx}]
                              (case arity
                                :arity-1 (->> fn-bodies val (process-defn-body process-cfg `(s/fspec ~@fdef-body)))
                                :arity-n (map (partial process-defn-body process-cfg)
                                              individual-arity-fspecs
                                              (val fn-bodies)))))
        main-defn         (remove nil? `(~defn-sym
                                         ~fn-name
                                         ~docstring
                                         ~meta-map
                                         ~@(process-fn-bodies (if (> trace 0) :dispatch 0))))
        traced-defn       (when (> trace 0)
                            (let [traced-defn (remove nil? `(~defn-sym
                                                             ~traced-fn-name
                                                             ~@(process-fn-bodies trace)))]
                              (if (= trace 1)
                                traced-defn
                                (clairvoyant-trace traced-defn trace color env nil (get-file-position env)))))]
    `(do ~fdef (declare ~fn-name) ~traced-defn ~main-defn ~instrumentation ~generated-test)))


(defn after-check-async [done]
  (let [success @r/*all-tests-successful]
    (when success (doseq [f @*after-check-callbacks] (f)))
    (reset! r/*all-tests-successful true)
    (reset! *after-check-callbacks [])
    (when success (done))))


(defn- generate-coverage-check [env nspace]
  (let [cljs?           (cljs-env? env)
        {:keys [::check-coverage ::no-check]} (merge (cfg/get-base-config-fn)
                                                     (if cljs?
                                                       (:meta (ana-api/find-ns nspace))
                                                       #?(:clj (meta nspace))))
        get-intern-meta (comp meta (if cljs? key val))
        all-checked-fns (when check-coverage
                          (some->> (if cljs? (ana-api/ns-interns nspace) #?(:clj (ns-interns nspace)))
                                   (filter #(if cljs? (-> % val :fn-var) #?(:clj (t/function? (key %)))))
                                   (remove #(-> % key str (string/ends-with? test-suffix)))
                                   (remove #(-> % get-intern-meta ::check-coverage false?))))
        plain-defns     (when check-coverage
                          (some->> all-checked-fns
                                   (remove #(-> % get-intern-meta ::ghostwheel))
                                   (map (comp str key))
                                   vec))
        unchecked-defns (when check-coverage
                          (some->> all-checked-fns
                                   (filter #(-> % get-intern-meta ::ghostwheel))
                                   (filter #(-> % get-intern-meta ::no-check))
                                   (map (comp str key))
                                   vec))]
    `(do
       ~(when no-check
          `(do
             (l/group ~(str "WARNING: "
                            "`::g/check` disabled for "
                            nspace
                            (::r/incomplete-coverage r/snippets))
                      ~r/warning-style)
             (l/group-end)))
       ~(when (not-empty plain-defns)
          `(do
             (l/group ~(str "WARNING: "
                            "Plain `defn` functions detected in "
                            nspace
                            (::r/incomplete-coverage r/snippets))
                      ~r/warning-style)
             (l/log (mapv symbol ~plain-defns))
             (l/log-bold "=> Use `>defn` instead.")
             (l/group-end)))
       ~(when (not-empty unchecked-defns)
          `(do
             (l/group ~(str "WARNING: "
                            "`::g/check` disabled for some functions in "
                            nspace
                            (::r/incomplete-coverage r/snippets))
                      ~r/warning-style)
             (l/log (mapv symbol ~unchecked-defns))
             (l/group-end))))))


(defn- generate-check [targets gen-tests-or-profile env]
  (let [base-config
        (cfg/get-base-config-fn)

        cljs?
        (cljs-env? env)

        {:keys [::extrument ::report-output]}
        base-config

        quotify-thing
        (fn [thing]
          (if (symbol? thing)
            `(quote ~thing)
            thing))

        conformed-targets
        (as-> targets targets
          (if (vector? targets)
            (map quotify-thing targets)
            (quotify-thing targets))
          (s/conform ::check-targets targets)
          (if (= (key targets) :multi)
            (val targets)
            [(val targets)]))

        processed-targets
        (mapcat (fn [[type target]]
                  (if (not= type :regex)
                    [[type (:sym target)]]
                    (for [ns (if cljs? (ana-api/all-ns) #?(:clj (all-ns)))
                          :when (re-matches target (str (if cljs? ns #?(:clj (ns-name ns)))))]
                      [:ns ns])))
                conformed-targets)

        errors
        (->> (for [target processed-targets
                   :let [[type sym] target]]
               (case type
                 :fn (let [fn-data  (if cljs? (ana-api/resolve env sym) #?(:clj (resolve sym)))
                           metadata (if cljs? (:meta fn-data) #?(:clj (meta fn-data)))

                           {:keys [::check-coverage ::no-check]}
                           (merge (cfg/get-base-config-fn)
                                  (meta (:ns fn-data))
                                  metadata)]
                       (cond (not fn-data)
                             (str "Cannot resolve `" (str sym) "`")

                             (not (if cljs? (:fn-var fn-data) #?(:clj (t/function? sym))))
                             (str "`" sym "` is not a function.")

                             (not (::ghostwheel metadata))
                             (str "`" sym "` is not a Ghostwheel function => Use `>defn` to define it.")

                             no-check
                             (str "Checking disabled for `" sym "` => Set `{:ghostwheel.core/check true}` to enable.")

                             :else
                             nil))
                 :ns (let [ns-data  (if cljs? (ana-api/find-ns sym) #?(:clj sym))
                           metadata (if cljs? (:meta ns-data) #?(:clj (meta ns-data)))
                           {:keys [::no-check]} (merge base-config metadata)]
                       (cond (not ns-data)
                             (str "Cannot resolve `" (str sym) "`")

                             no-check
                             (str "Checking disabled for `" sym "` => Set `{:ghostwheel.core/check true}` to enable.")

                             :else
                             nil))))
             (remove nil?))]
    (if (not-empty errors)
      (u/gen-exception env (str "\n" (string/join "\n" errors)))
      (gen-cleanup-console-on-exception
       cljs?
       `(when *global-check-allowed?*
          (binding [*global-trace-allowed?* false
                    *gen-tests-or-profile*  ~gen-tests-or-profile]
            (do
              ~@(remove nil?
                        `[~(when extrument
                             `(st/instrument (quote ~extrument)))
                          ~@(for [target processed-targets
                                  :let [[type sym] target]]
                              (case type
                                :fn `(binding [t/report r/report]
                                       (~(symbol (str sym test-suffix))))
                                :ns `(binding [t/report r/report]
                                       (t/run-tests (quote ~sym)))))
                          ~@(->> (for [target processed-targets
                                       :let [[type sym] target]
                                       :when (= type :ns)]
                                   (generate-coverage-check env sym))
                                 (remove nil?))
                          ~(when extrument
                             `(st/unstrument (quote ~extrument)))]))))))))


(defn- generate-after-check [callbacks]
  (let [{:keys [::no-check]}
        (merge (cfg/get-base-config-fn)
               (meta *ns*))]
    ;; TODO implement for clj
    (when (and (not no-check) (seq callbacks))
      `(swap! *after-check-callbacks (comp vec concat) ~(vec callbacks)))))


(defn- generate-traced-expr
  [expr label env]
  (let [cfg      (cfg/merge-config env (meta expr))
        color    (resolve-trace-color (::trace-color cfg))
        trace    (let [trace (::trace cfg)]
                   (if (= trace 0) 5 trace))
        cljs?    (cljs-env? env)
        position (get-file-position env)
        context  (str (when label (str label " – "))
                      (u/get-ns-name env) ":" position)
        generic-trace
                 (fn generic-trace
                   [expr expanded? & [has-nested is-nested?]]
                   (let [style {::l/background (:cyan l/ghostwheel-colors)}]
                     (gen-cleanup-console-on-exception
                      cljs?
                      (if ((some-fn string? number? nil? boolean? keyword?) expr)
                        `(let [ret# ~expr]
                           (l/log ret# ~style)
                           ret#)
                        `(let [code# ~(str expr)]
                           ((if ~expanded? l/group l/group-collapsed) code# ~style 55 ~context)
                           ~(when (and (not is-nested?)
                                       (coll? expr)
                                       (> (-> expr str count) 55))
                              `(do
                                 (l/group-collapsed "...")
                                 #_(~(if (list? expr) `l/group `l/group-collapsed)
                                    "...")
                                 (l/log ~(-> expr pprint/pprint with-out-str))
                                 (l/group-end)))
                           (let [ret# ~expr]
                             ~(when has-nested
                                `(do
                                   ~@(for [x has-nested
                                           :when (not (and (seq? x)
                                                           (#{'tr '|>} (-> x first name symbol))))]
                                       (generic-trace x true nil true))))
                             (l/log-exit ret#)
                             (l/group-end)
                             ret#))))))]
    (cond
      (and (seq? expr)
           (contains? #{'>defn '>defn-} (first expr)))
      `(~(first expr)
        ~(let [fname (second expr)
               fmeta (meta fname)]
           (->> fmeta
                (merge
                 {::trace       trace
                  ::trace-color color})
                (with-meta fname)))

        ~@(drop 2 expr))

      (and (seq? expr)
           (contains? l/ops-with-bindings (first expr)))
      (as-> (trace-threading-macros expr trace cljs?) expr
        (if (and (#{'fn 'fn*} (first expr))
                 (not (symbol? (second expr))))
          `(~(first expr) ~(gensym "fn_") ~@(rest expr))
          expr)
        ;; REVIEW: Clairvoyant doesn't work on Clojure yet
        (if cljs?
          (clairvoyant-trace expr trace color env label position)
          expr))

      (and (seq? expr)
           (contains? threading-macro-syms (first expr)))
      (trace-threading-macros expr trace cljs?)

      (seq? expr)
      (generic-trace expr true (rest expr))

      :else
      (generic-trace expr true))))


;;;; Main macros and public API


(s/def ::>defn-args
  (s/and seq?                                         ; REVIEW
         (s/cat :name simple-symbol?
                :docstring (s/? string?)
                :meta (s/? map?)
                :bs (s/alt :arity-1 ::args+gspec+body
                           ;; TODO: add tail-attr-map support after this
                           :arity-n (s/+ (s/and seq? ::args+gspec+body))))))


(s/fdef >defn :args ::>defn-args)

(defmacro >defn
  "Like defn, but requires a (nilable) gspec definition and generates
  additional `s/fdef`, generative tests, instrumentation code, an
  fspec-based stub, and/or tracing code, depending on the configuration
  metadata and the existence of a valid gspec and non-nil body."
  {:arglists '([name doc-string? attr-map? [params*] gspec prepost-map? body?]
               [name doc-string? attr-map? ([params*] gspec prepost-map? body?) + attr-map?])}
  [& forms]
  (if (cfg/get-env-config)
    (cond-> (remove nil? (generate-defn forms false &env))
            (cljs-env? &env) clj->cljs)
    (clean-defn 'defn forms)))


(s/fdef >defn- :args ::>defn-args)

;; NOTE: lots of duplication - refactor this to set/pass ^:private differently and call >defn
(defmacro >defn-
  "Like defn-, but requires a (nilable) gspec definition and generates
  additional `s/fdef`, generative tests, instrumentation code, an
  fspec-based stub, and/or tracing code, depending on the configuration
  metadata and the existence of a valid gspec and non-nil body."
  {:arglists '([name doc-string? attr-map? [params*] gspec prepost-map? body?]
               [name doc-string? attr-map? ([params*] gspec prepost-map? body?) + attr-map?])}
  [& forms]
  (if (cfg/get-env-config)
    (cond-> (remove nil? (generate-defn forms true &env))
            (cljs-env? &env) clj->cljs)
    (clean-defn 'defn- forms)))


(defmacro after-check
  "Takes a number of 0-arity functions to run
  after all checks are completed successfully.

  Meant to be used in a hot-reloading environment by putting it at the bottom
  of a `(g/check)`-ed namespace and calling `ghostwheel.core/after-check-async`
  correctly in the build system post-reload hooks."
  [& callbacks]
  (when (cfg/get-env-config)
    (cond-> (generate-after-check callbacks)
            (cljs-env? &env) (clj->cljs false))))


(s/def ::check-target
  (s/or :fn (s/and seq?
                   (s/cat :quote #{'quote}
                          :sym (s/and symbol?
                                      #(let [s (str %)]
                                         (or (string/includes? s "/")
                                             (not (string/includes? s ".")))))))
        :ns (s/and seq? (s/cat :quote #{'quote} :sym symbol?))
        :regex #?(:clj  #(instance? java.util.regex.Pattern %)
                  :cljs regexp?)))


(s/def ::check-targets
  (s/or :single ::check-target
        :multi (s/spec (s/+ ::check-target))))


;; TODO: Get this to work again after recent rewrite
#_(s/fdef check
    :args (s/or
           :arity0 (s/cat)
           :arity1 (s/cat :things ::check-targets)
           :arity2 (s/cat :tests (some-fn int? keyword?)
                          :things ::check-targets)))


(defmacro check
  "Runs Ghostwheel checks on the given namespaces and/or functions.
  Checks the current namespace if called without arguments."
  {:arglists '([]
               [quoted-sym-or-ns-regex]
               [[quoted-sym-or-ns-regex+]]
               [quoted-sym-or-ns-regex gen-tests-or-profile]
               [[quoted-sym-or-ns-regex+] gen-tests-or-profile])}
  ([]
   `(check (quote ~(u/get-ns-name &env)) nil))
  ([things]
   `(check ~things nil))
  ([things gen-tests-or-profile]
   (if (cfg/get-env-config)
     (cond-> (generate-check things gen-tests-or-profile &env)
             (cljs-env? &env) (clj->cljs false))
     ;; TODO: Fix message
     (str "Ghostwheel disabled => "
          (if (cljs-env? &env)
            "Add `:external-config {:ghostwheel {}}` to your compiler options to enable."
            "Start the REPL with the `-Dghostwheel.enabled=true` JVM system property to enable.")))))


(s/def ::>fdef-args
  (s/and seq?                                         ;REVIEW
         (s/cat :name (s/or :sym symbol? :key qualified-keyword?)
                :bs (s/alt :arity-1 ::args+gspec+body
                           :arity-n (s/+ (s/and seq? ::args+gspec+body))))))


(s/fdef >fdef :args ::>fdef-args)

(defmacro >fdef
  "Defines an fspec using gspec syntax – pretty much a `>defn` without the body.

  `name` can be a symbol or a qualified keyword, depending on whether the
  fspec is meant to be registered as a top-level fspec (=> s/fdef fn-sym
  ...) or used in other specs (=> s/def ::spec-keyword (s/fspec ...)).

  When defining global fspecs, instrumentation can be directly enabled by
  setting the `^::g/instrument` or `^::g/outstrument` metadata on the symbol."
  {:arglists '([name [params*] gspec]
               [name ([params*] gspec) +])}
  [& forms]
  (when (cfg/get-env-config)
    (cond-> (remove nil? (generate-fdef &env forms))
            (cljs-env? &env) clj->cljs)))


(defmacro |>
  "Traces and returns the wrapped expression, depending on its type"
  ([expr]
   `(|> nil ~expr))
  ([label expr]
   (if (cfg/get-env-config)
     (cond-> (generate-traced-expr expr label &env)
             (cljs-env? &env) clj->cljs)
     expr)))


(defmacro tr
  "Traces and returns the wrapped expression, depending on its type"
  ([expr] `(|> ~expr))
  ([label expr] `(|> ~label ~expr)))

