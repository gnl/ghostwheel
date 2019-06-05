;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.test-utils
  #?(:cljs (:require-macros ghostwheel.test-utils))
  (:require [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]
            [clojure.string :as string]
            [ghostwheel.core :as g :refer [>defn >defn- >fdef => | <- ? |> tr]]
            [ghostwheel.logging]
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
  (cond-> `(binding [ghostwheel.logging/*report-output* nil]
             (= (~threading-a ~@expr)
                (~threading-b ~@expr)
                (tr (~threading-a ~@expr))
                (|> (~threading-a ~@expr))))
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


(defn- name-variation
  [fn-sym & suffixes]
  (->> suffixes
       (map #(str "-" %))
       (apply str fn-sym)
       symbol))


(defn deftest-defn-variations*
  [base-name {:keys [::args-ret-mappings ::expected-fdef]} bodies cljs? nspace]
  (let [gen-test-assertions (fn [fn-sym]
                              (for [[args ret] args-ret-mappings]
                                `(binding [ghostwheel.logging/*report-output* nil]
                                   (is (= (~fn-sym ~@args) ~ret)))))]
    `(do ~(let [fn-sym (name-variation base-name "plain")]
            `(do
               (defn ~fn-sym ~@bodies)
               (deftest ~(name-variation fn-sym "test")
                 ~@(gen-test-assertions fn-sym))))
         ~@(for [tracing-wrapper ['|> 'tr]
                 :let [fn-sym         (name-variation base-name tracing-wrapper "fn")
                       test-sym       (name-variation fn-sym "test")
                       test-sym-named (name-variation fn-sym "named" "test")]]
             `(do
                (let [~fn-sym (~tracing-wrapper (~'fn ~@bodies))]
                  (deftest ~test-sym
                    ~@(gen-test-assertions fn-sym)))
                (let [~fn-sym (~tracing-wrapper (~'fn ~fn-sym ~@bodies))]
                  (deftest ~test-sym-named
                    ~@(gen-test-assertions fn-sym)))))
         ~@(for [tracing-wrapper ['|> 'tr]
                 op              ['defn 'defn- '>defn '>defn-]
                 :let [fn-sym   (name-variation base-name tracing-wrapper op)
                       test-sym (name-variation fn-sym "test")]]
             `(do
                (~tracing-wrapper (~op ~fn-sym ~@bodies))
                (deftest ~test-sym
                  ~@(gen-test-assertions fn-sym))))
         ~@(for [trace-level (range 0 7)
                 op          ['>defn '>defn-]
                 :let [fn-sym     (name-variation base-name op "trace" trace-level)
                       test-sym   (name-variation fn-sym "test")
                       fdef-sym   (name-variation fn-sym "fdef")
                       instrumentable-sym
                                  `(quote ~(symbol (str nspace) (str fn-sym)))
                       defn-forms `(~op ~fn-sym
                                    {:ghostwheel.core/no-check-fx true
                                     :ghostwheel.core/trace       ~trace-level}
                                    ~@bodies)]]
             `(do
                ~defn-forms
                (ost/instrument ~instrumentable-sym)
                (deftest ~test-sym
                  (let [~fdef-sym (extract-fdef (~(if cljs? `ucljs/expand `uclj/expand) ~defn-forms))]
                    ~(when expected-fdef
                       `(is (= ~fdef-sym (setval [(nthpath 1)] (quote ~fn-sym) ~expected-fdef))))
                    ~@(gen-test-assertions fn-sym))))))))


(defmacro deftest-defn-variations
  "This creates functions and tests for all possible
  combinations of trace-level, >defn/>defn-, etc."
  [base-name data & bodies]
  (let [cljs?  (cljs-env? &env)
        nspace (.-name *ns*)]
    (cond-> (deftest-defn-variations* base-name data bodies cljs? nspace)
            cljs? clj->cljs)))


(defn deftest-adhoc-trace-variations*
  [expr-type expr]
  `(do
     ~@(for [tracing-wrapper ['|> 'tr]
             :let [test-sym (name-variation tracing-wrapper expr-type "test")]]
         `(deftest ~test-sym
            (binding [ghostwheel.logging/*report-output* nil]
              (is (= ~expr
                     (~tracing-wrapper ~expr))))))))


(defmacro deftest-adhoc-trace-variations
  [expr-type expr]
  (let [cljs? (cljs-env? &env)]
    (cond-> (deftest-adhoc-trace-variations* expr-type expr)
            cljs? clj->cljs)))

