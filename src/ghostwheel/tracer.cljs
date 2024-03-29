;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ghostwheel.tracer
  (:require [clojure.walk :refer [prewalk walk]]
            [clojure.pprint :as ppr :refer [pprint]]
            [clojure.set :as set]
            [clairvoyant.core
             :refer [ITraceEnter ITraceError ITraceExit]
             :include-macros true]
            [devtools.prefs]
            [ghostwheel.logging :as logging
             :refer [log group group-collapsed group-end log-exit]]
            [clojure.string :as string]))


(def *inside-let (atom false))

(defn- trace-enter
  [{:keys [anonymous? named? arglist args dispatch-val form init name ns op protocol] :as data}
   {:keys [color background prefix suffix expand] :as options}]
  (let [binding-group (if (contains? expand :bindings)
                        group
                        group-collapsed)
        ;init          (if (and (seq? init)
        ;                       (= (first init) 'try)
        ;                       (seq? (second init))
        ;                       (-> (second init)
        ;                           first
        ;                           str
        ;                           (string/starts-with? "ghostwheel.threading-macros/*")))
        ;                nil
        ;                init)
        op-sym        (symbol (cljs.core/name op))
        unnamed-fn?   (and (#{'fn 'fn*} op-sym)
                           (not named?))
        group         (if (contains? expand op-sym)
                        (if unnamed-fn?
                          group-collapsed
                          group)
                        group-collapsed)]
    (cond
      (contains? logging/fn-like-ops op)
      (let [title (if protocol
                    (str protocol " " name " " arglist)
                    (str (when prefix (str prefix " – "))
                         ns "/" (when anonymous? "__anon_") name
                         (when dispatch-val
                           (str " " (pr-str dispatch-val)))
                         (str " " arglist)))]
        (group title
               {::logging/background background
                ::logging/foreground color
                ::logging/weight     "bold"}
               80
               suffix))

      (#{'let} op)
      (do
        (reset! *inside-let true)
        (group (str op)
               {::logging/background background
                ::logging/foreground color
                ::logging/weight     "bold"}
               80
               suffix))

      (#{:binding} op)
      (let [max-length 80
            init       (when @*inside-let
                         (logging/truncate-string (str init) max-length))]
        (binding-group (str form) nil nil init)
        #_(when (> (count label) max-length)
            (group-collapsed "...")
            (log (with-out-str (pprint/pprint label)))
            (group-end)))

      ;(#{'->} op)
      ;(log (str form))

      :else (log (str form)))))


(defn- trace-exit
  [{:keys [op exit] :as data} options]
  (cond
    (#{:binding} op)
    (do (log-exit exit)
        (group-end))

    (contains? logging/complex-trace-ops op)
    (do
      (when (#{'let `let} op)
        (reset! *inside-let false))
      (log-exit exit)
      (group-end))

    :else (log-exit exit)))


(defn- trace-error
  [{:keys [op form error ex-data] :as data} options]
  (cond
    (#{:binding} op)
    (do
      (error (.-stack error))
      (when ex-data
        (group-collapsed "ex-data")
        (group-collapsed ex-data)
        (group-end)
        (group-end)))

    (contains? logging/complex-trace-ops op)
    (do (group-end)
        (do
          (error (.-stack error))
          (when ex-data
            (group-collapsed "ex-data")
            (group-collapsed ex-data)
            (group-end)
            (group-end)))
        (group-end))))


(defn tracer
  "Custom tracer for Clairvoyant used by Ghostwheel but not dependent on it.

  Parameters:
  :color - string, Example: \"#aabbcc\"
  :background - same as color
  :prefix - string tag to display before the traced op heading
  :suffix - ...
  :expand - set of op symbols to display expanded by default. Use :bindings to expand all bindings.
  Example: #{'defn 'let :bindings}"
  [& options]
  (reify
    ITraceEnter (-trace-enter [_ data] (trace-enter data options))

    ITraceExit (-trace-exit [_ data] (trace-exit data options))

    ITraceError (-trace-error [_ data] (trace-error data options))))

