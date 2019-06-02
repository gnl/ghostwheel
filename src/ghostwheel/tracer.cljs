(ns ghostwheel.tracer
  (:require [clojure.walk :refer [prewalk walk]]
            [clairvoyant.core
             :refer [ITraceEnter ITraceError ITraceExit]
             :include-macros true]
            [ghostwheel.logging :as l]
            [clojure.string :as string]))

(defn tracer
  "Custom tracer for Clairvoyant used by Ghostwheel but not dependent on it.

  Parameters:
  :color - string, Example: \"#aabbcc\"
  :background - same as color
  :prefix - string tag to display before the traced op heading
  :suffix - ...
  :expand - set of op symbols to display expanded by default. Use :bindings to expand all bindings.
  Example: #{'defn 'let :bindings}"
  [& {:keys [color background prefix suffix expand] :as options}]
  (let [binding-group (if (contains? expand :bindings)
                        l/group
                        l/group-collapsed)
        log-exit      (fn [exit] (l/log (:symbol l/arrow) (:style l/arrow) exit))
        has-bindings? l/ops-with-bindings
        fn-like?      (disj has-bindings? 'let `let)]
    (reify
      ITraceEnter
      (-trace-enter
        [_ {:keys [anonymous? arglist args dispatch-val form init name ns op protocol]}]
        (let [init        (if (and (seq? init)
                                   (symbol? (first init)))
                            (let [f      (str (first init))
                                  prefix "ghostwheel.threading-macros/*"]
                              (if (string/starts-with? f prefix)
                                (cons (-> f (string/replace prefix "") symbol)
                                      (rest init))
                                init))
                            init)
              op-sym      (symbol (cljs.core/name op))
              unnamed-fn? (and (#{'fn 'fn*} op-sym)
                               (string/starts-with? (str name) "fn_"))
              group       (if (contains? expand op-sym)
                            (if unnamed-fn?
                              l/group-collapsed
                              l/group)
                            l/group-collapsed)]
          (cond
            (fn-like? op)
            (let [title (if protocol
                          (str protocol " " name " " arglist)
                          (str (when prefix (str prefix " – "))
                               ns "/" (when (and anonymous? unnamed-fn?) "__anon_") name
                               (when dispatch-val
                                 (str " " (pr-str dispatch-val)))
                               (str " " arglist)))]
              (group title
                     {::l/background background
                      ::l/foreground color
                      ::l/weight     "bold"}
                     80
                     suffix)
              (l/group "bindings"))

            (#{'let `let} op)
            (let [title (str op)]
              (group title)
              (l/group "bindings"))

            (#{'binding} op)
            (binding-group form nil nil init))))

      ITraceExit
      (-trace-exit [_ {:keys [op exit]}]
        (cond
          (#{'binding} op)
          (do (log-exit exit)
              (l/group-end))

          (has-bindings? op)
          (do (l/group-end)
              (log-exit exit)
              (l/group-end))))

      ITraceError
      (-trace-error [_ {:keys [op form error ex-data]}]
        (cond
          (#{'binding} op)
          (do
            (l/error (.-stack error))
            (when ex-data
              (l/group-collapsed "ex-data")
              (l/group-collapsed ex-data)
              (l/group-end)
              (l/group-end)))

          (has-bindings? op)
          (do (l/group-end)
              (do
                (l/error (.-stack error))
                (when ex-data
                  (l/group-collapsed "ex-data")
                  (l/group-collapsed ex-data)
                  (l/group-end)
                  (l/group-end)))
              (l/group-end)))))))
