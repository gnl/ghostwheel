;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.logging
  (:require [cuerdas.core :as cs]
            [clojure.pprint :refer [pprint]]
            [ghostwheel.utils :as u :refer [DBG] :include-macros true]))

(def *nesting (atom ""))

(def ^:dynamic *report-output* (:ghostwheel.core/report-output (u/get-env-config)))

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


(defn wrap [line]
  (u/wrap-line 80 line))

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

(defn get-styled-label-2 [label {:keys [::foreground ::background ::weight] :as style} output & [length]]
  (if-not style
    [label]
    (let [label (cond->> (if length
                           (truncate-string label length)
                           label)
                         (and (= output :repl) weight) (#(str "*" % "*"))
                         (= output :js-console) (str "%c"))
          style (when (= output :js-console)
                  (str "color: " (cond foreground foreground
                                       background "white"
                                       :else (:black ghostwheel-colors)) ";"
                       "background: " (if background background "white") ";"
                       "font-weight: " (if weight weight "500") ";"
                       (when background "text-shadow: 0.5px 0.5px black;")
                       (when background "padding: 2px 6px; border-radius: 2px;")))]
      (vec (remove nil? [label style])))))



(defn- plain-log [msg]
  (if-not msg
    (println @*nesting)
    (println (->> (if (string? msg) msg (with-out-str (pprint msg)))
                  (cs/lines)
                  (map #(str @*nesting %))
                  (cs/join "\n")))))

(defn log
  ([]
   (log nil nil))
  ([msg]
   (log msg nil))
  ([msg style]
   (let [styled-msg (get-styled-label-2 (or msg "") style *report-output*)]
     (do
       #?(:clj  (apply plain-log styled-msg)
          :cljs (case *report-output*
                  :repl (apply plain-log styled-msg)
                  :js-console (apply js/console.log styled-msg)))
       msg))))

(defn- plain-group [label]
  (do
    (log)
    (log (str "|> " label))
    (swap! *nesting #(str % "| "))))

(defn- group*
  ([open? label]
   (group* open? label nil))
  ([open? label style]
   (let [styled-label (get-styled-label-2 label style *report-output*)]
     #?(:clj  (apply plain-group styled-label)
        :cljs (case *report-output*
                :repl (apply plain-group styled-label)
                :js-console (apply (if open?
                                     js/console.group
                                     js/console.groupCollapsed)
                                   styled-label))))))

(defn group
  ([label]
   (group label nil))
  ([label style]
   (group* true label style)))

(defn group-collapsed
  ([label]
   (group label nil))
  ([label style]
   (group* false label style)))

(defn- plain-group-end []
  (swap! *nesting #(subs % 0 (- (count %) 2))))

(defn group-end []
  #?(:clj (plain-group-end)
     :cljs (case *report-output*
             :repl (plain-group-end)
             :js-console (js/console.groupEnd))))

(defn log-bold [msg]
  (log msg {::weight "bold"}))

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



