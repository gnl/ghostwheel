;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^:dev/always ghostwheel.tracing
  (:require [cuerdas.core :as cs]
            [ghostwheel.utils :as u :refer [cljs-env? get-ghostwheel-compiler-config get-ns-meta clj->cljs]]))

(def ghostwheel-colors
  {:purple0 "#967a93"
   :purple1 "#b87a93"
   :purple2 "#7d9cf8"
   :orange0 "#fe8709"
   :orange1 "#f17d3e"
   :green0  "#82da38"
   :green1  "#54a627"
   ;; Solarized colours
   :base03  "#002b36"
   :black   "#002b36"
   :base02  "#073642"
   :base01  "#586e75"
   :base00  "#657b83"
   :base0   "#839496"
   :base1   "#93a1a1"
   :base2   "#eee8d5"
   :base3   "#fdf6e3"
   :yellow  "#b58900"
   :orange  "#cb4b16"
   :red     "#dc322f"
   :magenta "#d33682"
   :violet  "#6c71c4"
   :blue    "#268bd2"
   :cyan    "#2aa198"
   :green   "#859900"})

(defn truncate-string
  [long-string limit]
  (str (cs/slice long-string 0 limit)
       (when (>= (count long-string) limit) "...")))

(defn get-styled-label [label {:keys [::foreground ::background ::weight]} & [length]]
  (let [label (str "%c"
                   #_(when background " ")
                   (if length
                     (truncate-string label length)
                     label))
        #_(when background " ")
        style (str "color: " (cond foreground foreground
                                   background "white"
                                   :else (:black ghostwheel-colors)) ";"
                   "background: " (if background background "white") ";"
                   "font-weight: " (if weight weight "500") ";"
                   (when background "text-shadow: 0.5px 0.5px black;")
                   (when background "padding: 2px 6px; border-radius: 2px;"))]
    [label style]))

(defn log [& msgs]
  #?(:cljs (apply js/console.log (or msgs [""]))
     :clj  (apply println msgs)))

(defn log-bold [msg]
  #?(:cljs (apply js/console.log (get-styled-label msg {::weight "bold"}))
     :clj  (println msg)))

(defn clog [data]
  (do
    (log data)
    data))

;; TODO: implement basic pr-clog version for Clojure using
;; https://github.com/clojure/tools.logging or something.
(defn pr-clog
  "Pretty console log"
  [label data & [style]]
  (do
    #?(:cljs (let [[label style] (get-styled-label label style)]
               (if data
                 (do
                   (js/console.group label style)
                   (js/console.log data)
                   (js/console.groupEnd))
                 (js/console.log label style))))
    data))


;;;; Traced threading macros


;; REVIEW: Consider doing this at compile time and passing the &env to differentiate
;; between Clojure and -Script
(defn log-threading-header
  [threading-type expr & [name]]
  #?(:cljs
     (apply js/console.group (get-styled-label (str threading-type " " expr (when name " ") name)
                                               {::background (:black ghostwheel-colors)}))))

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
                       `(js/console.groupEnd))
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
                         `(js/console.groupEnd))
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
                 `(js/console.groupEnd))
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
                            ~(log-cond-step test step `(-> ~g ~step) {::weight :bold})
                            ~(log-cond-step test step g {::foreground (:base0 ghostwheel-colors)})))]
             `(do
                (log-threading-header "cond->" ~(str expr))
                (pr-clog ~(str expr) ~expr)
                (let [~g ~expr
                      ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
                  ~(when (cljs-env? &env)
                     `(js/console.groupEnd))
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
                            ~(log-cond-step test step `(->> ~g ~step) {::weight :bold})
                            ~(log-cond-step test step g {::foreground (:base0 ghostwheel-colors)})))]
             `(do
                (log-threading-header "cond->>" ~(str expr))
                (pr-clog ~(str expr) ~expr)
                (let [~g ~expr
                      ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
                  ~(when (cljs-env? &env)
                     `(js/console.groupEnd))
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
                     `(js/console.groupEnd))
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
                     `(js/console.groupEnd))
                  ~g)))))
     (cljs-env? &env) clj->cljs)))


