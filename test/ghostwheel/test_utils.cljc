;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.test-utils
  #?(:cljs (:require-macros ghostwheel.test-utils))
  (:require [clojure.test :as t]
            [clojure.walk :as walk]
            [clojure.string :as string]
            [ghostwheel.core :as g :refer [>defn >defn- >fdef => | <- ? |> tr]]
            [ghostwheel.utils :as u :refer [cljs-env? clj->cljs]]
            [ghostwheel.test-utils-cljs :as ucljs]
            #?@(:clj  [[com.rpl.specter
                        :refer [setval transform select select-any
                                filterer nthpath ALL MAP-VALS MAP-KEYS NAMESPACE]]
                       [ghostwheel.test-utils-clj :as uclj]
                       [orchestra.spec.test :as ost]]
                :cljs [[com.rpl.specter
                        :refer-macros [setval transform select select-any]
                        :refer [filterer nthpath ALL MAP-VALS MAP-KEYS NAMESPACE]]
                       [orchestra-cljs.spec.test :as ost]])))

(defmacro threading-test
  [threading-a threading-b & expr]
  (cond-> `(= (~threading-a ~@expr)
              (~threading-b ~@expr)
              (tr (~threading-a ~@expr))
              (|> (~threading-a ~@expr)))
          (cljs-env? &env) clj->cljs))

(defmacro threading-expansion-test
  [threading-plain threading-fancy & expr]
  (let [in-cljs (cljs-env? &env)]
    (cond-> `(= (~(if in-cljs `ucljs/expand `uclj/expand) (~threading-fancy ~@expr))
                (quote (~threading-plain ~@expr)))
            in-cljs clj->cljs)))

(defn process-fdef
  [fdef]
  (walk/postwalk (fn [form]
                   (cond (and (symbol? form)
                              (-> form str (string/starts-with? "ret__")))
                         'ret__1

                         (and (symbol? form)
                              (-> form str (string/starts-with? "p1__")))
                         'p1__1

                         :else
                         form))
                 (u/clj->cljs fdef)))

(defn extract-fdef
  [form]
  (process-fdef (second form)))

(defmacro deftest-permutations
  "This creates functions and tests for all possible
  combinations of trace-level, >defn/>defn-, etc."
  [base-name {:keys [::args-ret-mappings ::expected-fdef]} & bodies]
  (let [in-cljs (cljs-env? &env)
        nspace  (.-name *ns*)]
    (cond-> `(do ~(let [fn-sym   (symbol (str base-name "-plain"))
                        test-sym (symbol (str fn-sym "-test"))]
                    `(do
                       (defn ~fn-sym ~@bodies)
                       (t/deftest ~test-sym
                         ~@(for [[args ret] args-ret-mappings]
                             `(t/is (= (~fn-sym ~@args) ~ret))))))
                 ~@(for [trace-level (range 0 7)
                         op          ['>defn '>defn-]
                         :let [fn-sym     (symbol (str base-name "-trace-" trace-level "-" op))
                               test-sym   (symbol (str fn-sym "-test"))
                               fdef-sym   (symbol (str "fdef-" fn-sym))
                               instrumentable-sym
                                          `(quote ~(symbol (str nspace) (str fn-sym)))
                               defn-forms `(~op ~fn-sym
                                            {:ghostwheel.core/no-check-fx true
                                             :ghostwheel.core/trace       ~trace-level}
                                            ~@bodies)]]
                     `(do
                        ~defn-forms
                        (ost/instrument ~instrumentable-sym)
                        (t/deftest ~test-sym
                          (let [~fdef-sym (extract-fdef (~(if in-cljs `ucljs/expand `uclj/expand) ~defn-forms))]
                            ~(when expected-fdef
                               `(t/is (= ~fdef-sym (setval [(nthpath 1)] (quote ~fn-sym) ~expected-fdef))))
                            ~@(for [[args ret] args-ret-mappings]
                                `(t/is (= (~fn-sym ~@args) ~ret))))))))
            in-cljs clj->cljs)))

