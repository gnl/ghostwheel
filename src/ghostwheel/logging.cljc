;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.logging
  "The primary purpose of this is to provide a simple
  cross-platform console-logging API – inspired by JavaScript's
  console.log/group/groupCollapsed/groupEnd – which makes sure that
  structured/styled information is displayed to the best of the output
  destination's abilities, be it a simple REPL or a browser-based JS console."
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]))


(def *nesting (atom ""))


(def ^:dynamic *report-output* #?(:clj  :repl
                                  :cljs :js-console))


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


(def ops-with-bindings
  #{'fn*
    'fn
    'ghostwheel.tracer/fn
    'defn
    'ghostwheel.tracer/defn
    'defn-
    'ghostwheel.tracer/defn-
    'defmethod
    'ghostwheel.tracer/defmethod
    'deftype
    'ghostwheel.tracer/deftype
    'defrecord
    'ghostwheel.tracer/defrecord
    'reify
    'ghostwheel.tracer/reify
    'let
    'ghostwheel.tracer/let
    'extend-type
    'ghostwheel.tracer/extend-type
    'extend-protocol
    'ghostwheel.tracer/extend-protocol})


;; Borrowed from Rosetta Code
(defn wrap-line [size text]
  (pprint/cl-format nil
                    (str "~{~<~%~1," size ":;~A~> ~}")
                    (string/split text #" ")))


(defn wrap [line]
  (wrap-line 80 line))


(defn truncate-string
  [long-string limit]
  (if (> (count long-string) limit)
    (str (subs long-string 0 limit) "...")
    long-string))


(defn get-styled-data
  [[main & extra] nested? {:keys [::foreground ::background ::weight ::css] :as style} output & [length]]
  (let [browser?              (= output :js-console)
        main                  (as-> main main
                                (if length
                                  (truncate-string (str main) length)
                                  main)
                                (if (and style browser?)
                                  (str "%c" main)
                                  main))
        left-shift-text       "margin-left: -11px;"
        left-shift-text-plain "margin-left: -15px;"
        style-main            (when (and style browser?)
                                (str "color: " (cond foreground foreground
                                                     background "white"
                                                     :else (:black ghostwheel-colors)) ";"
                                     "background: " (if background background "white") ";"
                                     "font-weight: " (if weight weight "500") ";"
                                     (when background (str "text-shadow: 0.5px 0.5px black;"
                                                           "padding: 2px 6px; border-radius: 2px;"))
                                     (when nested? left-shift-text)
                                     css))
        [main style-extra extra] (cond
                                   (not browser?)
                                   [main nil extra]

                                   (and (not nested?)
                                        (string? main)
                                        (not-empty extra)
                                        ((some-fn string? number?) (first extra)))
                                   [(str main "%c " (first extra)) "font-weight: 500;" (rest extra)]

                                   (and (empty? extra)
                                        (not (coll? main))
                                        (not style)
                                        nested?)
                                   [(str "%c" main) left-shift-text-plain nil]

                                   :else
                                   [main nil extra])]
    (->> (concat [main style-main style-extra
                  (when (and nested? (not-empty extra))
                    (if (string? (first extra))
                      ""
                      "  "))]
                 extra)
         (remove nil?)
         vec)))


(defn- plain-log [& msgs]
  (doseq [msg msgs]
    (println (->> (if (string? msg)
                    msg
                    (with-out-str (pprint/pprint msg)))
                  string/split-lines
                  (map #(str @*nesting %))
                  (string/join "\n")))))


(defn log
  ([]
   (log "" nil))
  ([msg]
   (log msg nil))
  ([msg style & data]
   (let [all-data    (concat [msg] data)
         styled-data (get-styled-data all-data true style *report-output*)]
     (case *report-output*
       :repl (apply plain-log styled-data)
       :js-console #?(:cljs (.apply js/console.log js/console (to-array styled-data))
                      :clj  nil)))))


(defn log-exit [data]
  #_(log "=>" #_""
         {::weight "bold"}
         data)
  (log data))


(defn DBG
  ([]
   (log "#> MARK"))
  ([& msgs]
   (do
     (doseq [msg msgs]
       #?(:clj  (let [msg (if (string? msg)
                            msg
                            (with-out-str (pprint/pprint msg)))]
                  (.println *err* msg))
          :cljs (if (nil? msg) (log "nil") (log msg)))
       (last msgs)))))


(defn log-raw
  [format-strings & objs]
  (case *report-output*
    :repl (doseq [obj objs] (plain-log obj))
    :js-console #?(:cljs (.apply js/console.log js/console (to-array (concat format-strings objs)))
                   :clj  nil)))


(defn error [msg]
  (case *report-output*
    :repl (do (plain-log "ERROR:") (plain-log msg))
    :js-console #?(:cljs (js/console.error msg)
                   :clj  nil)))


(defn warning [msg]
  (case *report-output*
    :repl (do (plain-log "WARNING:") (plain-log msg))
    :js-console #?(:cljs (js/console.warn msg)
                   :clj  nil)))


(defn- plain-group [& labels]
  (do
    (log)
    (log (str "|> " (string/join " – " labels)))
    (swap! *nesting #(str % "| "))))


(defn- group*
  ([open? labels]
   (group* open? labels nil nil))
  ([open? labels style]
   (group* open? labels style nil))
  ([open? labels style length]
   (let [styled-label (get-styled-data labels false style *report-output* length)]
     (case *report-output*
       :repl (apply plain-group styled-label)
       :js-console #?(:cljs (.apply (if open?
                                      js/console.group
                                      js/console.groupCollapsed)
                                    js/console
                                    (to-array styled-label))
                      :clj  nil)))))


(defn group
  ([label]
   (group label nil nil))
  ([label style]
   (group label style nil))
  ([label style length & data]
   (group* true (concat [label] data) style length)))


(defn group-collapsed
  ([label]
   (group-collapsed label nil nil))
  ([label style]
   (group-collapsed label style nil))
  ([label style length & data]
   (group* false (concat [label] data) style length)))


(let [plain-group-end
      (fn [] (swap! *nesting #(subs % 0 (max 0 (- (count %) 2)))))]
  (defn group-end []
    (case *report-output*
      :repl (plain-group-end)
      :js-console #?(:cljs (js/console.groupEnd)
                     :clj  nil))))


(defn log-bold [msg]
  (log msg {::weight "bold"}))


(defn dlog
  "Data log – logs data and returns it."
  ([data]
   (do
     (log data)
     data))
  ([data label]
   (dlog data label nil nil))
  ([data label style]
   (dlog data label style nil))
  ([data label style label-length]
   (if data
     (do
       (group label style label-length)
       (log data)
       (group-end))
     (log label style))
   data))



