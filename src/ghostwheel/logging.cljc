;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.logging
  (:require [cuerdas.core :as cs]
            [ghostwheel.utils :as u :refer [DBG]]))

(def *nesting (atom ""))

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


(def wrap (partial u/wrap-line 80))


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

(comment
 (defn- log [destinations & msgs]
   (let [plain-log #(println (apply str @*nesting %))]
     #?(:clj  (plain-log msgs)
        :cljs (do (when (contains? destinations :js-console)
                    (apply js/console.log (or msgs [""])))
                  (when (contains? destinations :repl)
                    (plain-log msgs)))))))

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

(defn- group [msg])
(defn- group-collapsed [msg])
(defn- group-end [msg])


