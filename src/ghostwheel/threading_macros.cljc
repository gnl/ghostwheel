;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.threading-macros
  #?(:cljs (:require-macros ghostwheel.threading-macros))
  (:require [ghostwheel.logging :as l
             :refer [ghostwheel-colors dlog group group-collapsed group-end log]]
            [ghostwheel.utils :as u :refer [cljs-env? clj->cljs]]
            [clojure.data :as data]
            [clojure.pprint :as pprint]))


;;;; Traced threading macros


(defn gen-log-threading-header
  [threading-type expr env & [name]]
  (let [context (str (-> env :ns :name) ":" (str (-> env :line) ":" (-> env :column)))]
    `(group (str "(" ~threading-type " " ~expr (when ~name " ") ~name " ...)")
            {::l/background (:black ~ghostwheel-colors)}
            55
            ~context)))


(defn gen-log-threading-diff
  ([old new form]
   (gen-log-threading-diff old new form {}))
  ([old new form style]
   `(let [old-x# ~old
          new-x# ~new
          [before# after# common#] (data/diff old-x# new-x#)]
      (group ~(str form) ~style 80)
      ~(when (> (-> form str count) 80)
         `(do
            (group-collapsed "...")
            (l/log ~(-> form pprint/pprint with-out-str))
            (group-end)))
      (cond
        (nil? common#)
        (log new-x#)

        (and (nil? before#) (nil? after#))
        (log "==="
             {::l/background (:base00 ghostwheel-colors)
              ::l/weight     "bold"}
             "No change.")

        :else
        (do
          (when before#
            (log "---"
                 {::l/background (:red ghostwheel-colors)
                  ::l/weight     "bold"}
                 before#))
          (when after#
            (log "+++"
                 {::l/background (:green ghostwheel-colors)
                  ::l/weight     "bold"}
                 after#))))
      (group-end)
      new-x#)))


(defmacro *->
  "Traced version of ->"
  [orig-x & orig-forms]
  (let [untraced `(~'-> ~orig-x ~@orig-forms)]
    (cond->
     (if-not (u/get-env-config)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          ~(loop [x orig-x, x-print orig-x, forms orig-forms]
             (if forms
               (let [form           (first forms)
                     threaded       (if (seq? form)
                                      (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                                      (list form x))
                     threaded-print (gen-log-threading-diff x-print threaded form)]
                 (recur threaded threaded-print (next forms)))
               `(do
                  ~(gen-log-threading-header "->" (str orig-x) &env)
                  (dlog ~orig-x ~(str orig-x))
                  (let [x# ~x-print]
                    (log (:symbol l/arrow) (:style l/arrow) x#)
                    (group-end)
                    x#))))))
     (cljs-env? &env) clj->cljs)))


(defmacro *->>
  "Traced version of ->>"
  [orig-x & orig-forms]
  (let [untraced `(~'->> ~orig-x ~@orig-forms)]
    (cond->
     (if-not (u/get-env-config)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          (do
            ~(loop [x orig-x, x-print orig-x, forms orig-forms]
               (if forms
                 (let [form           (first forms)
                       threaded       (if (seq? form)
                                        (with-meta `(~(first form) ~@(next form) ~x) (meta form))
                                        (list form x))
                       threaded-print (gen-log-threading-diff x-print threaded form)]
                   (recur threaded threaded-print (next forms)))
                 `(do
                    ~(gen-log-threading-header "->>" (str orig-x) &env)
                    (dlog ~orig-x ~(str orig-x))
                    (let [x# ~x-print]
                      (log (:symbol l/arrow) (:style l/arrow) x#)
                      (group-end)
                      x#)))))))
     (cljs-env? &env) clj->cljs)))


(defmacro *as->
  "Traced version of as->"
  [expr name & forms]
  (let [untraced `(~'as-> ~expr ~name ~@forms)
        log-step (fn [form] (gen-log-threading-diff name form form))]
    (cond->
     (if-not (u/get-env-config)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          (do
            ~(gen-log-threading-header "as->" (str expr) &env (str name))
            (dlog ~expr ~(str name))
            (let [~name ~expr
                  ~@(interleave (repeat name) (map log-step forms))]
              (log (:symbol l/arrow) (:style l/arrow) ~name)
              (group-end)
              ~name))))
     (cljs-env? &env) clj->cljs)))


(defmacro *cond->
  "Traced version of cond->"
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [untraced `(~'cond-> ~expr ~@clauses)]
    (cond->
     (if-not (u/get-env-config)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          ~(let [g     (gensym)
                 pstep (fn [[test step]]
                         (let [label (str test " " step)]
                           `(if ~test
                              ~(gen-log-threading-diff g `(-> ~g ~step) label {::l/weight :bold})
                              (do (group ~label ~{::l/foreground (:base0 ghostwheel-colors)})
                                  (group-end)
                                  ~g))))]
             `(do
                ~(gen-log-threading-header "cond->" (str expr) &env)
                (dlog ~expr ~(str expr))
                (let [~g ~expr
                      ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
                  (log (:symbol l/arrow) (:style l/arrow) ~g)
                  (group-end)
                  ~g)))))
     (cljs-env? &env) clj->cljs)))


(defmacro *cond->>
  "Traced version of cond->>"
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [untraced `(~'cond->> ~expr ~@clauses)]
    (cond->
     (if-not (u/get-env-config)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          ~(let [g     (gensym)
                 pstep (fn [[test step]]
                         (let [label (str test " " step)]
                           `(if ~test
                              ~(gen-log-threading-diff g `(->> ~g ~step) label {::l/weight :bold})
                              (do (group ~label ~{::l/foreground (:base0 ghostwheel-colors)})
                                  (group-end)
                                  ~g))))]
             `(do
                ~(gen-log-threading-header "cond->>" (str expr) &env)
                (dlog ~expr ~(str expr))
                (let [~g ~expr
                      ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
                  (log (:symbol l/arrow) (:style l/arrow) ~g)
                  (group-end)
                  ~g)))))
     (cljs-env? &env) clj->cljs)))


(defmacro *some->
  "Traced version of some->"
  [expr & forms]
  (let [untraced `(~'some-> ~expr ~@forms)]
    (cond->
     (if-not (u/get-env-config)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          ~(let [g         (gensym)
                 log-pstep (fn [step]
                             `(if (nil? ~g)
                                nil
                                ~(gen-log-threading-diff g `(-> ~g ~step) step)))]
             `(do
                ~(gen-log-threading-header "some->" (str expr) &env)
                (dlog ~expr ~(str expr))
                (let [~g ~expr
                      ~@(interleave (repeat g) (map log-pstep forms))]
                  (log (:symbol l/arrow) (:style l/arrow) (if (nil? ~g) "nil" ~g))
                  (group-end)
                  ~g)))))
     (cljs-env? &env) clj->cljs)))


(defmacro *some->>
  "Traced version of some->>"
  [expr & forms]
  (let [untraced `(~'some->> ~expr ~@forms)]
    (cond->
     (if-not (u/get-env-config)
       untraced
       `(if-not ghostwheel.core/*global-trace-allowed?*
          ~untraced
          ~(let [g         (gensym)
                 log-pstep (fn [step]
                             `(if (nil? ~g)
                                nil
                                ~(gen-log-threading-diff g `(->> ~g ~step) step)))]
             `(do
                ~(gen-log-threading-header "some->>" (str expr) &env)
                (dlog ~expr ~(str expr))
                (let [~g ~expr
                      ~@(interleave (repeat g) (map log-pstep forms))]
                  (log (:symbol l/arrow) (:style l/arrow) (if (nil? ~g) "nil" ~g))
                  (group-end)
                  ~g)))))
     (cljs-env? &env) clj->cljs)))


