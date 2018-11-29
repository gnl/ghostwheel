;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.threading-macros
  (:require [ghostwheel.logging :as l :refer [ghostwheel-colors pr-clog]]
            [ghostwheel.utils :as u :refer [cljs-env? get-ghostwheel-compiler-config get-ns-meta clj->cljs]]))


;;;; Traced threading macros


;; REVIEW: Consider doing this at compile time and passing the &env to differentiate
;; between Clojure and -Script
(defn log-threading-header
  [threading-type expr & [name]]
  (l/group (str threading-type " " expr (when name " ") name)
           {::l/background (:black ghostwheel-colors)}))

(defmacro *->
  "Traced version of ->"
  [orig-x & orig-forms]
  (let [untraced `(-> ~orig-x ~@orig-forms)]
    (cond->
     (if-not (get-ghostwheel-compiler-config &env)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          ~(loop [x orig-x, forms orig-forms]
             (if forms
               (let [form     (first forms)
                     threaded (if (seq? form)
                                (with-meta `(pr-clog ~(str form)
                                                     (~(first form) ~x ~@(next form)))
                                           (meta form))
                                `(pr-clog ~(str form)
                                          ~(list form x)))]
                 (recur threaded (next forms)))
               `(do
                  (log-threading-header "->" ~(str orig-x))
                  (pr-clog ~(str orig-x) ~orig-x)
                  (let [x# ~x]
                    ~(when (cljs-env? &env)
                       `(l/group-end))
                    x#))))))
     (cljs-env? &env) clj->cljs)))

(defmacro *->>
  "Traced version of ->>"
  [orig-x & orig-forms]
  (let [untraced `(->> ~orig-x ~@orig-forms)]
    (cond->
     (if-not (get-ghostwheel-compiler-config &env)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          (do
            ~(loop [x orig-x, forms orig-forms]
               (if forms
                 (let [form     (first forms)
                       threaded (if (seq? form)
                                  (with-meta `(pr-clog ~(str form)
                                                       (~(first form) ~@(next form) ~x))
                                             (meta form))
                                  `(pr-clog ~(str form)
                                            ~(list form x)))]
                   (recur threaded (next forms)))
                 `(do
                    (log-threading-header "->>" ~(str orig-x))
                    (pr-clog ~(str orig-x) ~orig-x)
                    (let [x# ~x]
                      ~(when (cljs-env? &env)
                         `(l/group-end))
                      x#)))))))
     (cljs-env? &env) clj->cljs)))

(defmacro *as->
  "Traced version of as->"
  [expr name & forms]
  (let [untraced `(as-> ~expr ~name ~@forms)
        log-step (fn [form] `(pr-clog ~(str form) ~form))]
    (cond->
     (if-not (get-ghostwheel-compiler-config &env)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          (do
            (log-threading-header "as->" ~(str expr) ~(str name))
            (pr-clog ~(str name) ~expr)
            (let [~name ~expr
                  ~@(interleave (repeat name) (map log-step forms))]
              ~(when (cljs-env? &env)
                 `(l/group-end))
              ~name))))
     (cljs-env? &env) clj->cljs)))

(defn- log-cond-step
  [test step data & [style]]
  `(pr-clog
    ~(str test
          " "
          step)
    ~data
    ~style))

(defmacro *cond->
  "Traced version of cond->"
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [untraced `(cond-> ~expr ~@clauses)]
    (cond->
     (if-not (get-ghostwheel-compiler-config &env)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          ~(let [g     (gensym)
                 pstep (fn [[test step]]
                         `(if ~test
                            ~(log-cond-step test step `(-> ~g ~step) {::l/weight :bold})
                            ~(log-cond-step test step g {::l/foreground (:base0 ghostwheel-colors)})))]
             `(do
                (log-threading-header "cond->" ~(str expr))
                (pr-clog ~(str expr) ~expr)
                (let [~g ~expr
                      ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
                  ~(when (cljs-env? &env)
                     `(l/group-end))
                  ~g)))))
     (cljs-env? &env) clj->cljs)))

(defmacro *cond->>
  "Traced version of cond->>"
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [untraced `(cond->> ~expr ~@clauses)]
    (cond->
     (if-not (get-ghostwheel-compiler-config &env)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          ~(let [g     (gensym)
                 pstep (fn [[test step]]
                         `(if ~test
                            ~(log-cond-step test step `(->> ~g ~step) {::l/weight :bold})
                            ~(log-cond-step test step g {::l/foreground (:base0 ghostwheel-colors)})))]
             `(do
                (log-threading-header "cond->>" ~(str expr))
                (pr-clog ~(str expr) ~expr)
                (let [~g ~expr
                      ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
                  ~(when (cljs-env? &env)
                     `(l/group-end))
                  ~g)))))
     (cljs-env? &env) clj->cljs)))

(defn- log-some-step
  [some-step]
  `(pr-clog
    ~(str #_(second some-step)
      #_" "
      (->> some-step (filter seq?) last last))
    ~some-step))

(defmacro *some->
  "Traced version of some->"
  [expr & forms]
  (let [untraced `(some-> ~expr ~@forms)]
    (cond->
     (if-not (get-ghostwheel-compiler-config &env)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          ~(let [g     (gensym)
                 pstep (fn [step] `(if (nil? ~g) nil (-> ~g ~step)))]
             `(do
                (log-threading-header "some->" ~(str expr))
                (pr-clog ~(str expr) ~expr)
                (let [~g ~expr
                      ~@(interleave (repeat g) (map log-some-step (map pstep forms)))]
                  ~(when (cljs-env? &env)
                     `(l/group-end))
                  ~g)))))
     (cljs-env? &env) clj->cljs)))

(defmacro *some->>
  "Traced version of some->>"
  [expr & forms]
  (let [untraced `(some->> ~expr ~@forms)]
    (cond->
     (if-not (get-ghostwheel-compiler-config &env)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          ~(let [g     (gensym)
                 pstep (fn [step] `(if (nil? ~g) nil (->> ~g ~step)))]
             `(do
                (log-threading-header "some->>" ~(str expr))
                (pr-clog ~(str expr) ~expr)
                (let [~g ~expr
                      ~@(interleave (repeat g) (map log-some-step (map pstep forms)))]
                  ~(when (cljs-env? &env)
                     `(l/group-end))
                  ~g)))))
     (cljs-env? &env) clj->cljs)))


